# smoke_test_exe_dirs.R
# Tests whether each candidate directory allows BOTH writing a compiled Stan exe
# AND executing it (running $sample()). Produces a pass/fail report per directory.
#
# Run from R console or RStudio:
#   source(here::here("smoke_test_exe_dirs.R"))

library(cmdstanr)

# ---------------------------------------------------------------------------
# 1. Minimal Stan model (written once to a stable temp location)
# ---------------------------------------------------------------------------
STAN_SRC <- "
// Minimal smoke-test model — no data, single parameter
parameters { real mu; }
model     { mu ~ normal(0, 1); }
"

stan_file <- file.path(tempdir(), "smoke_test_model.stan")
writeLines(STAN_SRC, stan_file)
cat(sprintf("[smoke test] .stan source written to: %s\n\n", stan_file))

# ---------------------------------------------------------------------------
# 2. Candidate exe directories — mapped from IT exclusion patterns
# ---------------------------------------------------------------------------
# Resolve paths dynamically so the script works for any user on any machine.
local_app_data <- Sys.getenv("LOCALAPPDATA",
                              unset = file.path(Sys.getenv("USERPROFILE"), "AppData", "Local"))
user_profile   <- Sys.getenv("USERPROFILE", unset = path.expand("~"))

# RStudio install dir: try common locations and use the first one that exists.
rstudio_candidates <- c(
  "C:/Program Files/RStudio",
  "C:/Program Files/Posit/RStudio",
  file.path(local_app_data, "Programs/RStudio"),
  file.path(local_app_data, "Programs/Posit/RStudio")
)
rstudio_dir <- Filter(dir.exists, rstudio_candidates)
rstudio_dir <- if (length(rstudio_dir)) rstudio_dir[[1]] else "C:/Program Files/RStudio"

candidates <- list(
  # C:\Users*\.cmdstan*
  ".cmdstan root"               = dirname(cmdstan_path()),
  ".cmdstan versioned subdir"   = cmdstan_path(),

  # C:\Users*\AppData\Local\Temp\RtmpX**
  "AppData Temp (tempdir)"      = tempdir(),

  # C:\rtools** (wildcard — test the two most common versions)
  "C:/rtools"                   = "C:/rtools",
  "C:/rtools44"                 = "C:/rtools44",
  "C:/rtools43"                 = "C:/rtools43",

  # C:\RBuildTools*
  "C:/RBuildTools"              = "C:/RBuildTools",

  # C:\Users*\Documents\R\win-library*\cmdstanr*
  "cmdstanr pkg dir (win-lib)"  = find.package("cmdstanr"),
  "Documents win-library"       = file.path(user_profile, "Documents", "R", "win-library"),

  # C:\Program Files\R\R-*\bin\x64\R.exe  /  Rscript.exe
  "R bin/x64 dir"               = file.path(R.home(), "bin", "x64"),
  "R bin dir"                   = file.path(R.home(), "bin"),

  # C:\Program Files\RStudio*
  "RStudio dir"                 = rstudio_dir,

  # %LOCALAPPDATA%\Programs\R*
  "LocalAppData/Programs/R"     = file.path(local_app_data, "Programs", "R")
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
fmt <- function(x) {
  if (is.na(x)) "?" else if (isTRUE(x)) "OK" else "NO"
}

# ---------------------------------------------------------------------------
# 3. Helper — attempt compile + sample in one directory
# ---------------------------------------------------------------------------
test_dir <- function(label, path) {
  result <- list(
    label       = label,
    path        = path,
    dir_exists  = NA,
    write_ok    = NA,
    compile_ok  = NA,
    sample_ok   = NA,
    error       = NA_character_,
    elapsed_s   = NA_real_
  )

  t0 <- proc.time()[["elapsed"]]

  # -- Can we create / reach the directory? ----------------------------------
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  result$dir_exists <- dir.exists(path)
  if (!result$dir_exists) {
    result$error <- "Directory could not be created"
    result$elapsed_s <- proc.time()[["elapsed"]] - t0
    return(result)
  }

  # -- Write a probe file to confirm write access ----------------------------
  probe <- file.path(path, ".smoke_write_probe")
  result$write_ok <- tryCatch({
    writeLines("ok", probe)
    file.exists(probe)
  }, error = function(e) FALSE)
  if (file.exists(probe)) file.remove(probe)

  if (!result$write_ok) {
    result$error <- "Write access denied"
    result$elapsed_s <- proc.time()[["elapsed"]] - t0
    return(result)
  }

  # -- Compile ---------------------------------------------------------------
  mod <- tryCatch(
    cmdstan_model(stan_file, dir = path, force_recompile = TRUE),
    error = function(e) {
      result$compile_ok <<- FALSE
      result$error      <<- conditionMessage(e)
      NULL
    }
  )

  if (is.null(mod)) {
    result$elapsed_s <- proc.time()[["elapsed"]] - t0
    return(result)
  }
  result$compile_ok <- TRUE

  # Clean up exe after test
  on.exit({
    exe <- mod$exe_file()
    if (!is.null(exe) && file.exists(exe)) file.remove(exe)
  }, add = TRUE)

  # -- Sample (minimal: 1 chain, 10 warmup, 10 draws) -----------------------
  fit <- tryCatch(
    mod$sample(chains = 1, iter_warmup = 10, iter_sampling = 10,
               show_messages = FALSE, show_exceptions = FALSE,
               refresh = 0),
    error = function(e) {
      result$sample_ok <<- FALSE
      result$error     <<- conditionMessage(e)
      NULL
    }
  )

  if (is.null(fit)) {
    result$elapsed_s <- proc.time()[["elapsed"]] - t0
    return(result)
  }

  result$sample_ok <- TRUE
  result$elapsed_s <- round(proc.time()[["elapsed"]] - t0, 1)
  result
}

# ---------------------------------------------------------------------------
# 4. Run all tests
# ---------------------------------------------------------------------------
cat("Running smoke tests...\n",
    "Each test: write probe + compile + sample (1 chain, 10 iter)\n",
    rep("-", 60), "\n", sep = "")

results <- mapply(test_dir, names(candidates), candidates, SIMPLIFY = FALSE)

# ---------------------------------------------------------------------------
# 5. Report
# ---------------------------------------------------------------------------
cat("\n", rep("=", 60), "\n", sep = "")
cat("SMOKE TEST RESULTS\n")
cat(rep("=", 60), "\n\n", sep = "")

for (r in results) {
  status <- if (isTRUE(r$sample_ok)) "PASS" else "FAIL"
  cat(sprintf("[%s]  %s\n", status, r$label))
  cat(sprintf("       path     : %s\n", r$path))
  cat(sprintf("       dir      : %s   write: %s   compile: %s   sample: %s\n",
              fmt(r$dir_exists), fmt(r$write_ok), fmt(r$compile_ok), fmt(r$sample_ok)))
  if (!is.na(r$elapsed_s))
    cat(sprintf("       time     : %.1f s\n", r$elapsed_s))
  if (!is.na(r$error))
    cat(sprintf("       error    : %s\n", r$error))
  cat("\n")
}

# Summary line
n_pass <- sum(sapply(results, function(r) isTRUE(r$sample_ok)))
n_fail <- length(results) - n_pass
cat(sprintf("Summary: %d/%d passed, %d failed\n", n_pass, length(results), n_fail))

# Return results invisibly so callers can inspect programmatically
invisible(results)
