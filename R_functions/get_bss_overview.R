# Migrated from rstan to cmdstanr
# API substitutions:
#   - summary(bss_fit, pars = c(...))$summary -> bss_fit$summary(variables = c(...))
#   - pluck("summary") removed; $summary() returns a tibble directly
#   - rstan::get_sampler_params() -> bss_fit$sampler_diagnostics(format = "df")
#   - Divergence counting: extract "divergent__" column and sum
#   - NaN-safe: generated quantities can overflow with short warmup, producing NaN draws.
#     rstan::summary() silently excluded NaN; posterior::summarise_draws() propagates them.
#     This version detects NaN and computes stats on valid draws only.
get_bss_overview <- function(bss_fit, ecg, ...){
  
  # Get raw draws to check for NaN/Inf (common with short warmup runs)
  draws_mat <- bss_fit$draws(variables = c("E_sum", "C_sum"), format = "matrix")
  n_nan <- sum(!is.finite(draws_mat))
  
  if (n_nan > 0) {
    warning(
      "get_bss_overview: ", n_nan, " non-finite (NaN/Inf) values detected in ",
      "E_sum/C_sum draws for catch group '", ecg, "'. ",
      "This typically indicates numerical overflow in generated quantities ",
      "(e.g., poisson_rng receiving Inf rate). ",
      "Summary statistics computed on ", sum(is.finite(draws_mat[,1])), 
      " / ", nrow(draws_mat), " valid draws."
    )
    
    # Build summary manually, excluding non-finite draws
    overview <- tibble::tibble(
      variable = colnames(draws_mat)
    )
    for (v in colnames(draws_mat)) {
      vals <- draws_mat[, v]
      vals_clean <- vals[is.finite(vals)]
      row_idx <- which(overview$variable == v)
      if (length(vals_clean) > 1) {
        overview$mean[row_idx]      <- mean(vals_clean)
        overview$median[row_idx]    <- median(vals_clean)
        overview$sd[row_idx]        <- sd(vals_clean)
        overview$mad[row_idx]       <- mad(vals_clean)
        overview$`2.5%`[row_idx]    <- quantile(vals_clean, 0.025)
        overview$`25%`[row_idx]     <- quantile(vals_clean, 0.25)
        overview$`50%`[row_idx]     <- quantile(vals_clean, 0.50)
        overview$`75%`[row_idx]     <- quantile(vals_clean, 0.75)
        overview$`97.5%`[row_idx]   <- quantile(vals_clean, 0.975)
        overview$rhat[row_idx]      <- NA_real_  # unreliable with excluded draws
        overview$ess_bulk[row_idx]  <- NA_real_
        overview$ess_tail[row_idx]  <- NA_real_
      } else {
        overview$mean[row_idx]      <- if (length(vals_clean) == 1) vals_clean else NA_real_
        overview$median[row_idx]    <- overview$mean[row_idx]
        overview$sd[row_idx]        <- NA_real_
        overview$mad[row_idx]       <- NA_real_
        overview$`2.5%`[row_idx]    <- NA_real_
        overview$`25%`[row_idx]     <- NA_real_
        overview$`50%`[row_idx]     <- NA_real_
        overview$`75%`[row_idx]     <- NA_real_
        overview$`97.5%`[row_idx]   <- NA_real_
        overview$rhat[row_idx]      <- NA_real_
        overview$ess_bulk[row_idx]  <- NA_real_
        overview$ess_tail[row_idx]  <- NA_real_
      }
    }
  } else {
    # No NaN issues — get default stats and full quantiles in separate calls then join.
    # Passing a custom function to $summary() replaces the defaults entirely, so we
    # call twice and join on "variable" to get both stat columns and quantile columns.
    overview_stats <- bss_fit$summary(variables = c("E_sum", "C_sum"))
    overview_qtile <- bss_fit$summary(
      variables = c("E_sum", "C_sum"),
      ~quantile(.x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
    )
    overview <- dplyr::left_join(overview_stats, overview_qtile, by = "variable")

    # Fallback: if $summary() still returns NA (cmdstanr/posterior version issue),
    # compute manually from the draws
    if (any(is.na(overview$mean))) {
      warning(
        "get_bss_overview: $summary() returned NA for E_sum/C_sum despite valid draws. ",
        "Computing summary statistics manually."
      )
      for (v in colnames(draws_mat)) {
        vals <- draws_mat[, v]
        row_idx <- which(overview$variable == v)
        overview$mean[row_idx]    <- mean(vals)
        overview$median[row_idx]  <- median(vals)
        overview$sd[row_idx]      <- sd(vals)
        overview$mad[row_idx]     <- mad(vals)
        overview$`2.5%`[row_idx]  <- quantile(vals, 0.025)
        overview$`25%`[row_idx]   <- quantile(vals, 0.25)
        overview$`50%`[row_idx]   <- quantile(vals, 0.50)
        overview$`75%`[row_idx]   <- quantile(vals, 0.75)
        overview$`97.5%`[row_idx] <- quantile(vals, 0.975)
      }
    }
  }
  
  # Extract divergence count from sampler diagnostics
  sampler_diag <- bss_fit$sampler_diagnostics(format = "df")
  n_div <- sum(sampler_diag$divergent__)
  
  overview |>
    mutate(
      n_div = n_div,
      est_cg = ecg
    ) |>
    relocate(variable, est_cg)
}