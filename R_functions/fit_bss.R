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
# Exe directory (stan_exe_dir):
#   Default = dirname(cmdstanr::cmdstan_path())  — the .cmdstan root directory
#   e.g.  C:\Users\<user>\.cmdstan\   (Windows)
#         ~/.cmdstan/                  (Linux/Mac)
#
# AppLocker fallback (Windows only):
#   If the primary stan_exe_dir is AppLocker-blocked for execution, compile_bss_model()
#   automatically copies the compiled exe to each fallback dir in order and uses the
#   first one that permits execution. Fallbacks are tried only when error 5 is seen;
#   they are never created — only used if already present on the machine.
#   Proven AppLocker-safe on WDFW endpoints: C:/rtools, C:/rtools44, C:/rtools43, C:/RBuildTools


# Internal helper: test whether Windows AppLocker permits execution of an exe.
# Runs the exe with no arguments; Stan compiled models exit immediately with usage
# text when called bare, so this is safe. Returns TRUE if process creation succeeded
# (any exit code), FALSE only if error 5 (Access Denied = AppLocker block) is raised.
.test_exe_runnable <- function(exe_path) {
  if (.Platform$OS.type != "windows") return(TRUE)
  tryCatch({
    processx::run(exe_path, args = character(0),
                  error_on_status = FALSE, timeout = 10,
                  stdout = NULL, stderr = NULL)
    TRUE
  }, error = function(e) {
    !grepl("system error 5", conditionMessage(e), fixed = TRUE)
  })
}


# Compile or load a BSS Stan model, returning a CmdStanModel object.
#
# Call this ONCE before a catch-group loop, then pass the result as
# bss_model_obj to fit_bss() to avoid repeating the compilation check
# on every iteration.
#
# On Windows/WDFW-managed endpoints the compiled exe is briefly locked by
# AV/EDR software immediately after writing; the retry loop handles that.
# If the primary stan_exe_dir is AppLocker-blocked at execution time, the
# function automatically falls back to rtools/RBuildTools directories.
# On Linux/macOS the loop exits on the first attempt.
#
# @param model_file_name Path to the .stan file.
# @param threads_per_chain Must match the threads_per_chain value used in
#   fit_bss() — compiling with stan_threads = TRUE is required for > 1 thread.
# @param stan_exe_dir Primary directory where the compiled exe is stored.
#   Default is the .cmdstan root (dirname of cmdstan_path()), resolved for
#   whatever user is running the code.
# @return A CmdStanModel object ready for $sample() calls.
compile_bss_model <- function(
    model_file_name   = here::here("stan_models/BSS_creel_model_02_2021-01-22_ppc.stan"),
    threads_per_chain = 1,
    stan_exe_dir      = dirname(cmdstanr::cmdstan_path())) {

  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("Package 'cmdstanr' is required.")
  }
  if (!file.exists(model_file_name)) {
    stop("Stan model file not found: ", model_file_name)
  }

  # stan_threads = TRUE is required at compile time for threads_per_chain > 1.
  cpp_opts <- if (threads_per_chain > 1) list(stan_threads = TRUE) else list()

  exe_stem     <- tools::file_path_sans_ext(basename(model_file_name))
  exe_filename <- if (.Platform$OS.type == "windows") paste0(exe_stem, ".exe") else exe_stem

  # A real CmdStan-compiled exe is always several MB; treat anything < 100 KB as invalid.
  EXE_MIN_BYTES <- 100000L
  is_windows    <- .Platform$OS.type == "windows"

  t0 <- proc.time()[["elapsed"]]

  # ---------------------------------------------------------------------------
  # Step 1: Compile (or load cached exe) into the primary stan_exe_dir.
  # ---------------------------------------------------------------------------
  dir.create(stan_exe_dir, showWarnings = FALSE, recursive = TRUE)
  if (!dir.exists(stan_exe_dir)) {
    stop("Could not create stan_exe_dir: ", stan_exe_dir)
  }

  primary_exe <- file.path(stan_exe_dir, exe_filename)

  # Remove any zero-byte or truncated stub left by a previous failed run.
  if (file.exists(primary_exe) && file.info(primary_exe)$size < EXE_MIN_BYTES) {
    message(sprintf("[compile_bss_model] Removing truncated exe (%d bytes): %s",
                    file.info(primary_exe)$size, primary_exe))
    file.remove(primary_exe)
  }

  if (file.exists(primary_exe)) {
    message(sprintf("[compile_bss_model] Existing exe found — skipping compilation\n  %s", primary_exe))
  } else {
    message(sprintf("[compile_bss_model] No exe found — compiling: %s", basename(model_file_name)))
  }

  # AV/EDR retry loop (error 32 = transient scan lock; wait and retry).
  max_attempts <- 6L
  mod <- NULL
  for (attempt in seq_len(max_attempts)) {
    mod <- tryCatch(
      cmdstanr::cmdstan_model(model_file_name, dir = stan_exe_dir,
                              cpp_options = cpp_opts, force_recompile = FALSE),
      error = function(e) {
        msg <- conditionMessage(e)
        if (is_windows && grepl("system error 32", msg, fixed = TRUE) && attempt < max_attempts) {
          message(sprintf("[compile_bss_model] Exe locked by AV scan (attempt %d/%d) — waiting 10 s...",
                          attempt, max_attempts))
          Sys.sleep(10)
          return(NULL)
        }
        stop(e)
      }
    )
    if (!is.null(mod)) break
  }

  message(sprintf("[compile_bss_model] Compiled/loaded: %.1f s  |  exe: %s",
                  proc.time()[["elapsed"]] - t0, mod$exe_file()))

  # ---------------------------------------------------------------------------
  # Step 2: Test whether the exe can actually be executed (AppLocker check).
  # On non-Windows or when the primary dir is permitted, return immediately.
  # ---------------------------------------------------------------------------
  if (.test_exe_runnable(mod$exe_file())) {
    return(mod)
  }

  # ---------------------------------------------------------------------------
  # Step 3: AppLocker blocked the primary dir — copy exe to fallback dirs and
  # use the first one that permits execution.
  # Fallbacks are only used if the directory already exists on the machine
  # (rtools/RBuildTools are installed by the user, not created here).
  # ---------------------------------------------------------------------------
  fallback_dirs <- c("C:/rtools", "C:/rtools44", "C:/rtools43", "C:/RBuildTools")
  compiled_exe  <- mod$exe_file()

  message(sprintf(
    "[compile_bss_model] AppLocker blocked execution from: %s\n  Trying fallback directories: %s",
    stan_exe_dir, paste(fallback_dirs, collapse = ", ")
  ))

  for (fb_dir in fallback_dirs) {
    if (!dir.exists(fb_dir)) next  # skip if not installed on this machine

    fb_exe    <- file.path(fb_dir, exe_filename)
    copied_ok <- file.copy(compiled_exe, fb_exe, overwrite = TRUE)
    fb_size   <- if (file.exists(fb_exe)) file.info(fb_exe)$size else 0L

    if (!copied_ok || fb_size < EXE_MIN_BYTES) {
      message(sprintf("[compile_bss_model] Copy to %s failed (%d bytes) — skipping.", fb_dir, fb_size))
      if (file.exists(fb_exe)) file.remove(fb_exe)
      next
    }

    if (!.test_exe_runnable(fb_exe)) {
      message(sprintf("[compile_bss_model] AppLocker also blocked %s — skipping.", fb_dir))
      file.remove(fb_exe)
      next
    }

    # This fallback dir permits execution — load the model from it.
    mod_fb <- tryCatch(
      cmdstanr::cmdstan_model(model_file_name, dir = fb_dir,
                              cpp_options = cpp_opts, force_recompile = FALSE),
      error = function(e) {
        message(sprintf("[compile_bss_model] Failed to load from %s: %s", fb_dir, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(mod_fb)) {
      message(sprintf("[compile_bss_model] Using fallback dir: %s", fb_dir))
      return(mod_fb)
    }
  }

  stop(
    "[compile_bss_model] All candidate directories are AppLocker-blocked.\n",
    "  Tried: ", paste(c(stan_exe_dir, fallback_dirs), collapse = "\n         "), "\n",
    "  Submit an IT ticket requesting an AppLocker execution rule for one of these paths."
  )
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
    stan_exe_dir      = dirname(cmdstanr::cmdstan_path()),
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
