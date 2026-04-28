#' Save combined density plots for all catch groups
#'
#' @param density_plots List of density plot objects
#' @param outputs_folders List containing output folder paths
#'
#' @return Combined patchwork figure, or NULL if error occurs
save_bss_density_plots <- function(density_plots, outputs_folders) {
  
  tryCatch({
    # Check that inputs are valid
    if (is.null(density_plots) || length(density_plots) == 0) {
      cli::cli_alert_warning("No density plots provided to save")
      return(NULL)
    }
    
    # Filter out NULL plots (from failed catch groups)
    valid_plots <- density_plots[!sapply(density_plots, is.null)]
    
    # Check if there are any valid plots
    if (length(valid_plots) == 0) {
      cli::cli_alert_warning("No valid density plots to save - all catch groups failed")
      return(NULL)
    }
    
    if (length(valid_plots) < length(density_plots)) {
      cli::cli_alert_warning("Skipped {length(density_plots) - length(valid_plots)} catch group(s) due to errors")
    }
    
    # Combine all valid plots vertically
    combined_figure <- wrap_plots(valid_plots, ncol = 1) +
      plot_annotation(
        title = "Posterior Probability Densities of Catch and Effort by Catch Group"
      ) &
      theme(strip.text = element_text(size = 10))
    
    # Save the combined figure
    save_plot(
      combined_figure,
      "posterior_probability_densities.png",
      width = 10,
      height = 12
    )
    
    cli::cli_alert_success("Successfully saved density plots for {length(valid_plots)} catch group(s)")
    
    return(combined_figure)
    
  }, error = function(e) {
    cli::cli_alert_danger("Error saving density plots: {e$message}")
    cli::cli_alert_warning("Density plots could not be saved")
    return(NULL)
  })
}
