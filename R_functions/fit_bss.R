# Migrated from rstan to cmdstanr
# API substitutions:
#   - rstan::stan() -> cmdstanr::cmdstan_model()$sample()
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
fit_bss <- function(
  model_file_name = here::here("stan_models/BSS_creel_model_02_2021-01-22_ppc.stan"),
  bss_inputs_list,
  n_chain = 4,
  n_cores = NULL,
  n_iter = 2000,
  n_warmup = 1000,
  n_thin = 1,
  adapt_delta = 0.8,
  max_treedepth = 10,
  threads_per_chain = 1,
  init = "0",
  ...){
  
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("Package 'cmdstanr' is required. Install with: install.packages('cmdstanr', repos = c('https://mc-stan.org/r-packages/', getOption('repos')))")
  }
  
  # Compile the model, optionally with threading support for reduce_sum.
  # stan_threads = TRUE enables OpenMP/TBB; only needed when threads_per_chain > 1.
  # Without it, the standard exe is used (no TBB overhead, AppLocker-friendly).
  cpp_opts <- if (threads_per_chain > 1) list(stan_threads = TRUE) else list()
  mod <- cmdstanr::cmdstan_model(
    model_file_name,
    cpp_options = cpp_opts
  )
  
  # Convert init: rstan accepts init = "0" (all zeros), cmdstanr needs init = 0 (numeric)
  if (is.character(init) && init == "0") {
    init_value <- 0
  } else {
    init_value <- init
  }
  
  # Calculate iter_sampling from the rstan-style n_iter / n_warmup convention
  iter_sampling <- n_iter - n_warmup
  
  # Remove non-Stan variables from the data list
  # rstan silently ignored these, but cmdstanr's write_stan_json() rejects invalid types
  # est_cg is a character label added by prep_inputs_bss() for bookkeeping
  stan_data <- bss_inputs_list[setdiff(names(bss_inputs_list), "est_cg")]
  
  # Inject grainsize for reduce_sum (1 = auto-tune, recommended default)
  stan_data$grainsize <- 1L
  
  # Fit the model using cmdstanr's $sample() method
  fit <- mod$sample(
    data = stan_data,
    chains = n_chain,
    parallel_chains = n_cores,
    iter_sampling = iter_sampling,
    iter_warmup = n_warmup,
    thin = n_thin,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    threads_per_chain = threads_per_chain,
    init = init_value,
    refresh = 200
  )
  
  return(fit)
}