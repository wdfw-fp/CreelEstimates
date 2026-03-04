# Migrated from rstan to cmdstanr
# No direct rstan API calls in this function.
# Input is a draws_summary tibble from posterior::summarize_draws() which
# is compatible with both rstan and cmdstanr backends.
# Function 1: create_convergence_table.R
create_convergence_table <- function(draws_summary, ecg) {
  draws_problems <- draws_summary |> 
    filter(rhat > 1.09 | ess_bulk < 100 | ess_tail < 100) 
  
  if (nrow(draws_problems) == 0) {
    return(tibble(
      variable = character(),
      rhat = numeric(),
      rhat_display = numeric(),
      ess_bulk = numeric(),
      ess_tail = numeric(),
      Mean = numeric(),
      SD = numeric(),
      MAD = numeric(),
      `5%` = numeric(),
      `95%` = numeric()
    ))
  }
  
  draws_problems |> 
    mutate(
      rhat_display = round(rhat, 2),
      ess_bulk = round(ess_bulk, 0),
      ess_tail = round(ess_tail, 0),
      mean = round(mean, 2),
      sd = round(sd, 2),
      mad = round(mad, 2),
      q5 = round(q5, 2),
      q95 = round(q95, 2)
    ) |> 
    rename(
      "Mean" = mean,
      "SD" = sd,
      "MAD" = mad,
      "5%" = q5,
      "95%" = q95
    ) |> 
    arrange(desc(rhat))
}