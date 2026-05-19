# plot paired season-long counts from census and index angler effort count surveys
plot_inputs_pe_census_vs_index <- function(
    census_TI_expan,
    outputs_folders = NULL,
    save = TRUE,
    filename = "census_vs_index_counts",
    width = 10,
    height = 8,
    ...
){
  # Create the plot
  p <- census_TI_expan |>
    ggplot(aes(count_index, count_census, fill = angler_final)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_continuous(limits = c(0, NA)) +
    geom_point(color = "black", pch = 21, size = 3.25) +
    labs(fill = "Angler type") +
    scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
    geom_smooth(method = "lm", se = FALSE) +
    guides(fill = guide_legend(override.aes = list(linetype = 0))) 
  # +
    # facet_wrap(~paste("Section:",section_num), labeller = label_wrap_gen(multi_line = F))
  
  # Save if requested
  if (save) {
    # Check for outputs_folders - try parameter first, then global environment
    if (is.null(outputs_folders)) {
      if (!exists("outputs_folders", envir = parent.frame())) {
        warning("outputs_folders not found. Plot not saved. Provide outputs_folders argument or ensure it exists in calling environment.")
      } else {
        outputs_folders <- get("outputs_folders", envir = parent.frame())
      }
    }
    
    # Only save if we have outputs_folders
    if (!is.null(outputs_folders) && "figures" %in% names(outputs_folders)) {
      # Ensure .png extension
      if (!grepl("\\.(png|pdf)$", filename)) {
        filename <- paste0(filename, ".png")
      }
      
      filepath <- file.path(outputs_folders$figures, filename)
      
      # Save as PNG
      ggplot2::ggsave(
        filename = filepath,
        plot = p,
        width = width,
        height = height,
        dpi = 300,
        ...
      )
      
      # Also save as PDF
      pdf_path <- sub("\\.png$", ".pdf", filepath)
      ggplot2::ggsave(
        filename = pdf_path,
        plot = p,
        width = width,
        height = height,
        ...
      )
      
      cli::cli_alert_success("Plot saved: {.file {basename(filepath)}}")
    }
  }
  
  return(p)
}