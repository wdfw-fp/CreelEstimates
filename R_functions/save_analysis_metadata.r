# Save analysis metadata files (Rmd, analysis_lut, params) to analysis folder

#' Copy Rmd file and save metadata to analysis folder
#' 
#' @param params List of parameters from Rmd YAML header
#' @param analysis_lut Analysis lookup table
#' @param analysis_folder_path Path to analysis folder
#' @param current_file_path Path to current Rmd file (optional)
#' 
#' @return Invisibly returns NULL
save_analysis_metadata <- function(params, analysis_lut, analysis_folder_path, 
                                   current_file_path = NULL) {
  
  tryCatch({
    # Try multiple methods to get the current file path if not provided
    if (is.null(current_file_path)) {
      current_file <- if (!is.null(knitr::current_input()) && knitr::current_input() != "") {
        here::here("template_scripts", knitr::current_input())
      } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        rstudioapi::getSourceEditorContext()$path
      } else {
        NULL
      }
    } else {
      current_file <- current_file_path
    }
    
    # Copy Rmd file to analysis folder
    if (!is.null(current_file) && current_file != "" && file.exists(current_file)) {
      # Use fw_creel prefix with analysis folder name as the new filename
      analysis_folder_name <- basename(analysis_folder_path)
      new_filename <- paste0("fw_creel_", analysis_folder_name, ".Rmd")
      destination_path <- file.path(analysis_folder_path, new_filename)
      
      # Only copy if file doesn't exist yet
      if (!file.exists(destination_path)) {
        file.copy(
          from = current_file,
          to = destination_path,
          overwrite = FALSE
        )
        cli::cli_alert_success("Analysis file copied to {.path {destination_path}}")
      } else {
        cli::cli_alert_info("Analysis file already exists: {.path {destination_path}}")
      }
    } else {
      cli::cli_alert_warning("Could not determine current file path - manual copy may be required")
    }
    
    # Save analysis_lut.rds to analysis folder root with folder name
    analysis_lut_filename <- paste0("analysis_lut_", basename(analysis_folder_path), ".rds")
    analysis_lut_path <- file.path(analysis_folder_path, analysis_lut_filename)
    
    # Only save if file doesn't already exist
    if (!file.exists(analysis_lut_path)) {
      saveRDS(analysis_lut, analysis_lut_path)
      cli::cli_alert_success("Analysis metadata saved to {.path {analysis_lut_path}}")
    } else {
      cli::cli_alert_info("Analysis metadata already exists: {.path {analysis_lut_path}}")
    }
    
    # Save params yml file to analysis folder root with folder name
    params_filename <- paste0("params_", basename(analysis_folder_path), ".yml")
    params_path <- file.path(analysis_folder_path, params_filename)
    
    # Only save if file doesn't already exist
    if (!file.exists(params_path)) {
      yaml::write_yaml(params, params_path)
      cli::cli_alert_success("Params metadata saved to {.path {params_path}}")
    } else {
      cli::cli_alert_info("Params metadata already exists: {.path {params_path}}")
    }
    
  }, error = function(e) {
    cli::cli_alert_warning("Error in file operations: {e$message}")
  })
  
  return(invisible(NULL))
}
