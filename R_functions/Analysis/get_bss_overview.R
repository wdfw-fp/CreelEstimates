get_bss_overview <- function(bss_fit, ecg, ...){
  bss_fit |> 
    summary(pars = c("E_sum", "C_sum")) |> 
    pluck("summary") |> 
    as.data.frame() |> 
    rownames_to_column("estimate") |> 
    as_tibble() |> 
    bind_cols(
      bss_fit |> 
        rstan::get_sampler_params(inc_warmup = FALSE) |> #list of matrices rows-iterations by 6 measures
        set_names(~paste0("n_div_",1:length(bss_fit@stan_args))) |> 
        purrr::map_dbl(~.x[, "divergent__"] |> sum()) |> #n-chains cols of n-divergent transitions per chain
        sum() |> as_tibble_col(column_name = "n_div")
    ) |> 
    mutate(est_cg = ecg) |> 
    relocate(estimate, est_cg)
}