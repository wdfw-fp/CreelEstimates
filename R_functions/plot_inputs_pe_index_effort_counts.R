# plot paired season-long counts from census and index angler effort count surveys

plot_inputs_pe_index_effort_counts <- function(
    params,
    effort_index,
    outputs_folders = NULL,
    save = TRUE,
    filename = "index_effort_counts",
    width = 10,
    height = 8,
    ...
){

  p <- effort_index |>
    mutate(count_sequence = factor(count_sequence)) |> 
    ggplot(aes(event_date, count_index, fill = count_sequence)) +
    #geom_point() + geom_text(aes(label = count_index), nudge_y = 1, check_overlap = T) +
    geom_col(position = position_dodge(width = 0.7)) +
    scale_x_date("", date_breaks = "7 days", date_labels =  "%m-%d") + scale_y_continuous("") +
    scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
    facet_wrap(~section_num + angler_final, scales = "fixed", ncol = 1, labeller = label_wrap_gen(multi_line = F))
  
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