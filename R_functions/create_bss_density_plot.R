#' Create density plot for catch and effort
#'
#' @param season_results Output from get_bss_season_results()
#' @param ecg_name Name of the catch group for the plot title
#'
#' @return Combined patchwork plot object, or NULL if error occurs
create_bss_density_plot <- function(season_results, ecg_name) {
  
  # Return NULL if season_results is NULL (error in previous step)
  if (is.null(season_results)) {
    cli::cli_alert_warning("Cannot create density plot for {ecg_name} - season results are NULL")
    return(NULL)
  }
  
  tryCatch({
    # Check that required data exists
    if (is.null(season_results$catch_results) || is.null(season_results$effort_results)) {
      cli::cli_alert_warning("Missing catch or effort results for {ecg_name}")
      return(NULL)
    }
    
    if (nrow(season_results$catch_results) == 0 || nrow(season_results$effort_results) == 0) {
      cli::cli_alert_warning("Empty catch or effort results for {ecg_name}")
      return(NULL)
    }
    
    # Catch plot
    plot_catch <- ggplot(season_results$catch_results, aes(x = C_sum)) +
      geom_density(fill = "steelblue", alpha = 0.6) +
      geom_vline(data = season_results$catch_quantiles, 
                 aes(xintercept = C_sum, group = q), 
                 linetype = "dashed") +
      labs(x = "Catch (number of fish)", y = "Kernel Density", 
           title = glue("{ecg_name}")) +
      theme_bw() +
      theme(legend.position = "none") 
    
    # Effort plot
    plot_effort <- ggplot(season_results$effort_results, aes(x = E_sum)) +
      geom_density(fill = "steelblue", alpha = 0.6) +
      geom_vline(data = season_results$effort_quantiles, 
                 aes(xintercept = E_sum, group = q), 
                 linetype = "dashed") +
      labs(x = "Effort (angler-hours)", y = "Kernel Density") +
      theme_bw() +
      theme(legend.position = "none") 
    
    # Combine catch and effort plots
    combined_plot <- plot_catch / plot_effort +
      plot_layout(heights = c(1, 1))
    
    return(combined_plot)
    
  }, error = function(e) {
    cli::cli_alert_danger("Error creating density plot for catch group {ecg_name}: {e$message}")
    cli::cli_alert_warning("Skipping density plot for catch group: {ecg_name}")
    return(NULL)
  })
}
