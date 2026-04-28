# Function 2: save_convergence_diagnostics.R
save_convergence_diagnostics <- function(convergence_table, ecg, outputs_folders) {
  
  gt_table <- convergence_table |> 
    dplyr::select(-rhat) |> 
    dplyr::relocate(rhat_display, .after = "variable") |> 
    gt::gt() |> 
    gt::cols_label(
      variable = "Parameter", 
      rhat_display = "Rhat", 
      ess_bulk = "ESS_bulk", 
      ess_tail = "ESS_tail"
    ) |> 
    gt::tab_header(
      title = glue::glue("Catch Group: {ecg}"),
      subtitle = "Parameters shown meet at least one criteria: Rhat >= 1.09 or ESS < 100"
    ) |> 
    gt::tab_style(
      style = gt::cell_text(align = "left"),
      locations = gt::cells_title(groups = "subtitle")
    ) |> 
    gt::tab_footnote(
      footnote = "Filtered to 15 rows, sorted by descending Rhat",
      locations = gt::cells_column_labels(columns = c("rhat_display"))
    )
  
  # Save to figures folder (HTML only - no Chrome needed)
  safe_name <- stringr::str_replace_all(ecg, "[^[:alnum:]]", "_")
  filename_base <- paste0("convergence_diagnostics_", safe_name)
  html_path <- file.path(outputs_folders$figures, paste0(filename_base, ".html"))
  
  gt::gtsave(gt_table, filename = html_path)
  cli::cli_alert_success("Table saved as HTML: {.file {basename(html_path)}}")
  
  # Return the gt object for display in document
  return(gt_table)
}