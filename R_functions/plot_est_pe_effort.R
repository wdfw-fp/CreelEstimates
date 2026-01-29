# plot PE effort estimates by period (time stratum) and section
plot_est_pe_effort <- function(
    estimates_pe_effort,
    period_pe,
    outputs_folders = NULL,
    save = TRUE,
    filename = NULL,
    width = 10,
    height = 8,
    ...
){
  
  # Use the passed parameter, not estimates_pe$effort
  if(period_pe == "week"){
    p <- estimates_pe_effort |> 
      ggplot(aes(min_event_date, est, fill = interaction(day_type, angler_final))) +
      scale_x_date(date_breaks = "1 week", labels = scales::date_format("%W"),
                   sec.axis = dup_axis(name = "", breaks = waiver(), labels = scales::date_format("%b"))) +
      labs(fill = "Angler and day type groups") +
      ylab("Effort (angler hours)") +
      xlab("Date (week)") +
      geom_col(position = position_stack(), color = "black", width = 3.5) +
      scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
      facet_wrap(~section_num, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2) 
  }
  else if(period_pe == "month"){
    p <- estimates_pe_effort |> 
      ggplot(aes(min_event_date, est, fill = interaction(day_type, angler_final))) +
      scale_x_date(date_breaks = "1 month", labels = scales::date_format("%b")) +
      labs(fill = "Angler and day type groups") +
      ylab("Effort (angler hours)") +
      xlab("Date (month)") +
      geom_col(position = position_stack(), color = "black", width = 12) +
      scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
      facet_wrap(~section_num, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2) 
  }
  else if(period_pe == "duration"){
    p <- estimates_pe_effort |> 
      ggplot(aes(min_event_date, est, fill = interaction(day_type, angler_final))) +
      scale_x_date() +
      labs(fill = "Angler and day type groups") +
      ylab("Effort (angler hours)") +
      xlab("Date") +
      geom_col(position = position_stack(), color = "black", width = 12) +
      scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
      facet_wrap(~section_num, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2) 
  }
  
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