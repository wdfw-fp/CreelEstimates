#thin wrapper on stan()
fit_bss <- function(
  # model_file = here::here("stan_models/BSS_creel_model_02_2021-01-22.stan"),
  #model_file_name = here::here(paste0("stan_models/", model_file_name)), #BSS_creel_model_02_2021-01-22.stan"),
  model_file_name = here::here("stan_models/BSS_creel_model_02_2024-04-03.stan"),
  bss_inputs_list,
  n_chain = n_chain,  
  n_cores = n_cores,
  n_iter = n_iter,
  n_warmup = n_warmup,
  n_thin = n_thin,
  adapt_delta = adapt_delta,
  max_treedepth = max_treedepth,
  init = "0",
  ...){
  
  stan(
    file = model_file_name,
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