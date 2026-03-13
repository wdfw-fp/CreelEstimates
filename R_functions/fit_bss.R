# Migrated from rstan to cmdstanr
# API substitutions:
#   - rstan::stan() -> compiled CmdStanModel$sample()
#   - chains -> chains
#   - cores -> parallel_chains
#   - iter -> iter_sampling = (n_iter - n_warmup)
#   - warmup -> iter_warmup
#   - thin -> thin
#   - control = list(adapt_delta, max_treedepth) -> adapt_delta, max_treedepth as direct args
#   - init = "0" (character) -> init = 0 (numeric)
#   - Within-chain parallelization via reduce_sum:
#     cpp_options = list(stan_threads = TRUE) enables OpenMP threading at compile time
#     threads_per_chain controls runtime thread count per chain
#     Total CPU = parallel_chains * threads_per_chain
#
# Exe management strategy:
#   - ONE compiled exe per machine, stored in stan_exe_dir
#   - cmdstan_model() skips recompilation when the exe is already current (force_recompile = FALSE)
#   - For loops over multiple catch groups: call compile_bss_model() once before the loop,
#     then pass the result as bss_model_obj to each fit_bss() call (compile once, sample N times)
#
# Exe directory (stan_exe_dir) — WDFW-managed endpoints (DFW-275708):
#   Default = cmdstanr::cmdstan_path()  — the VERSIONED CmdStan install directory
#   e.g.  C:\Users\<user>\.cmdstan\cmdstan-2.38.0\          (Windows)
#         /home/<user>/.cmdstan/cmdstan-2.38.0/              (Linux)
#   AppLocker/WDAC permits execution of CmdStan's own tools from this exact directory
#   (stanc.exe, etc. all run from here). Placing the compiled model exe in the same
#   directory inherits that permission.
#
#   The .cmdstan ROOT (dirname of cmdstan_path()) is NOT AppLocker-permitted for execution:
#   the IT ASR exclusion (C:\Users*\.cmdstan*) covers write/scan operations only.
#   Error 5 (Access Denied) on process creation = AppLocker block = permanent, not transient.


# Compile or load a BSS Stan model, returning a CmdStanModel object.
#
# Call this ONCE before a catch-group loop, then pass the result as
# bss_model_obj to fit_bss() to avoid repeating the compilation check
# on every iteration.
#
# On Windows/WDFW-managed endpoints the compiled exe is briefly locked by
# AV/EDR software immediately after writing; the retry loop handles that.
# On Linux/macOS the loop exits on the first attempt.
#
# @param model_file_name Path to the .stan file.
# @param threads_per_chain Must match the threads_per_chain value used in
#   fit_bss() — compiling with stan_threads = TRUE is required for > 1 thread.
# @param stan_exe_dir Directory where the compiled exe is stored.
#   Default keeps the exe inside the CmdStan install tree (AppLocker-safe).
# @return A CmdStanModel object ready for $sample() calls.
compile_bss_model <- function(
    model_file_name   = here::here("stan_models/BSS_creel_model_02_2021-01-22_ppc.stan"),
    threads_per_chain = 1,
    stan_exe_dir      = cmdstanr::cmdstan_path()) {

  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("Package 'cmdstanr' is required.")
  }
  if (!file.exists(model_file_name)) {
    stop("Stan model file not found: ", model_file_name)
  }

  # stan_threads = TRUE is required at compile time for threads_per_chain > 1.
  # It enables OpenMP/TBB; without it, $sample() ignores threads_per_chain.
  cpp_opts <- if (threads_per_chain > 1) list(stan_threads = TRUE) else list()

  # Create the target directory (cross-platform; no-op if it already exists).
  dir.create(stan_exe_dir, showWarnings = FALSE, recursive = TRUE)
  if (!dir.exists(stan_exe_dir)) {
    stop("Could not create stan_exe_dir: ", stan_exe_dir,
         "\nSet stan_exe_dir to a writable path inside the CmdStan install tree.")
  }

  # Resolve the expected exe path (cross-platform).
  # cmdstanr names the exe after the .stan file stem; this must match exactly.
  exe_stem     <- tools::file_path_sans_ext(basename(model_file_name))
  exe_filename <- if (.Platform$OS.type == "windows") paste0(exe_stem, ".exe") else exe_stem
  exe_path     <- file.path(stan_exe_dir, exe_filename)

  t0 <- proc.time()[["elapsed"]]

  # A real CmdStan-compiled exe is always several MB; treat anything < 100 KB as invalid.
  EXE_MIN_BYTES <- 100000L

  exe_size  <- if (file.exists(exe_path)) file.info(exe_path)$size else -1L
  exe_valid <- exe_size >= EXE_MIN_BYTES

  if (exe_valid) {
    # Exe already in the AppLocker-safe target directory — nothing to do.
    message(sprintf("[compile_bss_model] Existing exe found — skipping compilation\n  %s", exe_path))
  } else {
    # Remove any zero-byte or stub file left by a failed copy.
    if (file.exists(exe_path)) {
      message(sprintf(
        "[compile_bss_model] Removing invalid/truncated exe (%d bytes): %s",
        exe_size, exe_path
      ))
      file.remove(exe_path)
    }

    # Check the parent directory (dirname(stan_exe_dir)) as a fallback.
    # This handles the case where the exe was previously compiled directly into
    # the .cmdstan root (e.g. C:\Users\<user>\.cmdstan\) before the AppLocker-safe
    # subdirectory convention was established.
    parent_exe_path  <- file.path(dirname(stan_exe_dir), exe_filename)
    parent_exe_size  <- if (file.exists(parent_exe_path)) file.info(parent_exe_path)$size else -1L
    parent_exe_valid <- parent_exe_size >= EXE_MIN_BYTES

    if (parent_exe_valid) {
      message(sprintf(
        "[compile_bss_model] Exe found in parent dir — copying to AppLocker-safe location\n  from: %s\n  to:   %s",
        parent_exe_path, exe_path
      ))
      copied_ok      <- file.copy(parent_exe_path, exe_path, overwrite = TRUE)
      copied_size    <- if (file.exists(exe_path)) file.info(exe_path)$size else 0L
      copy_succeeded <- copied_ok && copied_size >= EXE_MIN_BYTES
      if (!copy_succeeded) {
        message(sprintf(
          "[compile_bss_model] Copy failed or produced truncated file (%d bytes) — will recompile.\n  (Source may be read-restricted by AV/EDR.)",
          copied_size
        ))
        if (file.exists(exe_path)) file.remove(exe_path)
      }
    }

    if (!file.exists(exe_path)) {
      message(sprintf("[compile_bss_model] No valid exe found — compiling: %s", basename(model_file_name)))
    }
  }

  # Call cmdstan_model() with force_recompile = FALSE.
  # Existing exe: cmdstanr loads it directly (no compilation).
  # Absent exe: cmdstanr compiles it.
  # AV/EDR retry loop handles Windows briefly locking a newly written .exe
  # (only relevant on first-time compilation; loads never trigger AV scanning).
  is_windows   <- .Platform$OS.type == "windows"
  max_attempts <- 6L

  for (attempt in seq_len(max_attempts)) {
    mod <- tryCatch(
      cmdstanr::cmdstan_model(
        model_file_name,
        dir             = stan_exe_dir,
        cpp_options     = cpp_opts,
        force_recompile = FALSE
      ),
      error = function(e) {
        msg        <- conditionMessage(e)
        is_av_lock <- is_windows && grepl("system error 32", msg, fixed = TRUE)
        is_blocked <- is_windows && grepl("system error 5",  msg, fixed = TRUE)
        if (is_blocked) {
          stop(
            "[compile_bss_model] AppLocker blocked execution of the compiled exe.\n",
            "  Path: ", exe_path, "\n",
            "  This path is not in the AppLocker execution whitelist (DFW-275708).\n",
            "  The IT exclusion C:\\Users*\\.cmdstan* is an ASR rule (write/scan only),\n",
            "  not an AppLocker execution permission. Submit an IT ticket to add\n",
            "  an AppLocker rule for: ", stan_exe_dir
          )
        }
        if (is_av_lock && attempt < max_attempts) {
          message(sprintf(
            "[compile_bss_model] Exe locked by AV scan (attempt %d/%d) — waiting 10 s...",
            attempt, max_attempts
          ))
          Sys.sleep(10)
          return(NULL)
        }
        stop(e)
      }
    )
    if (!is.null(mod)) break
  }

  message(sprintf("[fit_bss] Model compile/load: %.1f s  |  exe: %s",
                  proc.time()[["elapsed"]] - t0,
                  mod$exe_file()))
  mod
}


# Fit a BSS creel model to one catch group's data.
#
# Pass bss_model_obj (from compile_bss_model()) when sampling multiple catch
# groups in a loop — the model is compiled once and reused, saving ~1-2 s of
# cmdstan_model() overhead per iteration.  When bss_model_obj is NULL the
# model is compiled/loaded automatically from stan_exe_dir.
#
# @param bss_inputs_list  Named list produced by prep_inputs_bss() for one catch group.
# @param bss_model_obj    Pre-compiled CmdStanModel (from compile_bss_model()).
#   NULL triggers internal compilation.
# @param model_file_name  Path to the .stan file (used only when bss_model_obj is NULL).
# @param n_chain,n_cores,n_iter,n_warmup,n_thin  Sampling controls.
# @param adapt_delta,max_treedepth  HMC tuning.
# @param threads_per_chain  Within-chain parallelization threads (requires
#   compile_bss_model() to have been called with the same value).
# @param init  Starting values. "0" (rstan convention) is converted to 0 (numeric).
# @param refresh  Progress update interval; NULL auto-scales to ~10 updates.
# @param stan_exe_dir  Used only when bss_model_obj is NULL.
# @return A CmdStanFit object.
fit_bss <- function(
    bss_inputs_list,
    bss_model_obj     = NULL,
    model_file_name   = here::here("stan_models/BSS_creel_model_02_2021-01-22_ppc.stan"),
    n_chain           = 4,
    n_cores           = NULL,
    n_iter            = 2000,
    n_warmup          = 1000,
    n_thin            = 1,
    adapt_delta       = 0.8,
    max_treedepth     = 10,
    threads_per_chain = 1,
    init              = "0",
    refresh           = NULL,
    stan_exe_dir      = cmdstanr::cmdstan_path(),
    ...) {

  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("Package 'cmdstanr' is required.")
  }

  # Use the provided compiled model object, or compile/load one now.
  mod <- if (!is.null(bss_model_obj)) {
    bss_model_obj
  } else {
    compile_bss_model(
      model_file_name   = model_file_name,
      threads_per_chain = threads_per_chain,
      stan_exe_dir      = stan_exe_dir
    )
  }

  # Convert init: rstan accepts "0" (character); cmdstanr requires 0 (numeric).
  init_value <- if (is.character(init) && init == "0") 0 else init

  # Calculate iter_sampling from the rstan-style n_iter / n_warmup convention.
  iter_sampling <- n_iter - n_warmup

  # Auto-scale refresh to give ~10 progress updates; NULL = auto, 0 = silent.
  refresh_val <- if (is.null(refresh)) {
    max(1L, as.integer(floor(iter_sampling / 10)))
  } else {
    as.integer(refresh)
  }

  # Remove non-Stan bookkeeping fields added by prep_inputs_bss().
  # rstan silently ignored these; cmdstanr's write_stan_json() rejects them.
  stan_data <- bss_inputs_list[setdiff(names(bss_inputs_list), "est_cg")]

  # grainsize is required by the _ppc.stan model's data block.
  # Value 1 = auto-tune (optimal for reduce_sum regardless of thread count).
  stan_data$grainsize <- 1L

  t_sample_start <- proc.time()[["elapsed"]]
  fit <- mod$sample(
    data              = stan_data,
    chains            = n_chain,
    parallel_chains   = n_cores,
    iter_sampling     = iter_sampling,
    iter_warmup       = n_warmup,
    thin              = n_thin,
    adapt_delta       = adapt_delta,
    max_treedepth     = max_treedepth,
    threads_per_chain = threads_per_chain,
    init              = init_value,
    refresh           = refresh_val
  )
  message(sprintf("[fit_bss] Sampling: %.1f s", proc.time()[["elapsed"]] - t_sample_start))

  fit
}
