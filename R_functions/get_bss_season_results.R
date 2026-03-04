# Migrated from rstan to cmdstanr
# API substitutions:
#   - ecg_draws$C_sum / ecg_draws$E_sum were previously vectors from rstan::extract()
#   - With cmdstanr, the caller passes:
#     ecg_draws$C_sum = as.vector(fit$draws(variables = "C_sum", format = "matrix"))
#     ecg_draws$E_sum = as.vector(fit$draws(variables = "E_sum", format = "matrix"))
#   - These are plain numeric vectors, so as.data.frame() creates a single-column df
#   - Updated to use tibble(C_sum = ...) directly instead of as.data.frame + pivot_longer
#' Prepare season-level results for a single catch group
#'
#' @param ecg_draws Named list with C_sum and E_sum numeric vectors of posterior draws
#' @param ecg_name Name of the catch group
#'
#' @return List containing catch and effort results and quantiles, or NULL if error occurs
get_bss_season_results <- function(ecg_draws, ecg_name) {
  
  tryCatch({
    # Prepare catch data — ecg_draws$C_sum is a numeric vector of posterior draws
    catch_results <- tibble(
      C_sum = as.numeric(ecg_draws$C_sum),
      iter = seq_along(ecg_draws$C_sum),
      ecg = ecg_name
    )
    
    # Check for missing values in catch
    if (any(is.na(catch_results$C_sum)) || any(is.nan(catch_results$C_sum))) {
      cli::cli_alert_warning("Missing or NaN values detected in C_sum for catch group: {ecg_name}")
    }
    
    catch_quantiles <- catch_results |>
      reframe(C_sum = quantile(C_sum, c(0.025, 0.5, 0.975), na.rm = TRUE), 
              q = c(0.025, 0.5, 0.975))
    
    # Prepare effort data — ecg_draws$E_sum is a numeric vector of posterior draws
    effort_results <- tibble(
      E_sum = as.numeric(ecg_draws$E_sum),
      iter = seq_along(ecg_draws$E_sum),
      ecg = ecg_name
    )
    
    # Check for missing values in effort
    if (any(is.na(effort_results$E_sum)) || any(is.nan(effort_results$E_sum))) {
      cli::cli_alert_warning("Missing or NaN values detected in E_sum for catch group: {ecg_name}")
    }
    
    effort_quantiles <- effort_results |>
      reframe(E_sum = quantile(E_sum, c(0.025, 0.5, 0.975), na.rm = TRUE), 
              q = c(0.025, 0.5, 0.975))
    
    return(list(
      catch_results = catch_results,
      catch_quantiles = catch_quantiles,
      effort_results = effort_results,
      effort_quantiles = effort_quantiles
    ))
    
  }, error = function(e) {
    cli::cli_alert_danger("Error processing season results for catch group {ecg_name}: {e$message}")
    cli::cli_alert_warning("Skipping density plot generation for catch group: {ecg_name}")
    return(NULL)
  })
}
