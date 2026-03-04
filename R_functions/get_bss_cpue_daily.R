# Migrated from rstan to cmdstanr
# API substitutions:
#   - summary(bss_fit, pars = "lambda_C_S")$summary -> bss_fit$summary(variables = "lambda_C_S")
#   - pluck("summary") removed; $summary() returns tibble directly
#   - as.data.frame() |> rownames_to_column("estimate_index") ->
#     mutate(estimate_index = variable) since cmdstanr uses a "variable" column
get_bss_cpue_daily <- function(bss_fit, ecg, dwg, ...){
  dwg <- dwg
  bss_fit$summary(variables = "lambda_C_S", ~quantile(.x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))) |>
    mutate(estimate_index = variable) |>
    mutate(indices = str_sub(estimate_index, 12, 20) |> str_remove("\\]")) |> 
    separate(
      col = indices,
      into = c("section_num", "day_index", "angler_final")
    ) |> 
    mutate(
      across(c(section_num, day_index), as.integer),
      angler_final = if_else(angler_final == "1", "bank", "boat"),
      est_cg = ecg,
      estimate = "CPUE_daily"
    ) |> 
    left_join(dwg$days |> 
                select(event_date, day_index, week, month),
              by = "day_index") |> 
    relocate(estimate, estimate_index, est_cg, day_index, event_date, week, month, section_num, angler_final) |> 
    arrange(event_date)
}