#thin wrapper on stan()
fit_bss <- function(
    model_file = "stan_models/BSS_creel_model_02_2021-01-22.stan",
    bss_inputs_list,
    n_chain = 4,  
    n_cores = 4,
    n_iter = 1000,
    n_warmup = n_iter/2,
    n_thin = 1,
    adapt_delta = 0.95,
    max_treedepth = 12,
    init = "0",
    ...){
  
  stan(
    file = model_file,
    data = bss_inputs_list,
    chains = n_chain,
    cores = n_cores,
    iter = n_iter,
    warmup = n_warmup,
    thin = n_thin, init = init, include = T,
    control = list(
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth
    )
  )
  
}