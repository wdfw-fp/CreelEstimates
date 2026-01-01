# Create folder structure for analysis outputs and configure knitr settings

#' Set up analysis folder structure and knitr cache settings
#' 
#' @param params List of parameters from Rmd YAML header
#' @param analysis_lut Analysis lookup table with folder information
#' 
#' @return List with paths to outputs folders
setup_analysis_structure <- function(params, analysis_lut) {
  
  # Define analysis folder path
  analysis_folder_path <- here::here(
    "fishery_analyses", 
    params$project_name, 
    params$fishery_name, 
    analysis_lut$analysis_folder
  )
  
  # Create analysis folder
  if (!dir.exists(analysis_folder_path)) {
    dir.create(analysis_folder_path, recursive = TRUE)
    cli::cli_alert_success("Analysis folder created at {.path {analysis_folder_path}}")
  } else {
    cli::cli_alert_info("Using existing analysis folder at {.path {analysis_folder_path}}")
  }
  
  # Create organized subfolders for outputs
  outputs_folders <- list(
    inputs = file.path(analysis_folder_path, "inputs"),
    outputs = file.path(analysis_folder_path, "outputs"),
    figures = file.path(analysis_folder_path, "figures")
  )
  
  purrr::walk(outputs_folders, ~ {
    if (!dir.exists(.x)) {
      dir.create(.x, recursive = TRUE)
    }
  })
  
  # Define and create separate cache location
  cache_path <- here::here(
    "fishery_analyses", 
    params$project_name, 
    params$fishery_name, 
    analysis_lut$analysis_folder, 
    ".cache"
  )
  
  if (!dir.exists(cache_path)) {
    dir.create(cache_path, recursive = TRUE)
  }
  
  # Set knitr cache to SEPARATE location from analysis outputs
  knitr::opts_chunk$set(
    cache = TRUE,
    cache.path = file.path(cache_path, "/"),
    fig.path = file.path(outputs_folders$figures, "/"),
    fig.keep = "all",
    dev = c("png", "pdf"),
    dpi = 300,
    cache.extra = list(params, analysis_lut$analysis_id),
    cache.lazy = FALSE
  )
  
  # Set output directory for knitted HTML
  knitr::opts_chunk$set(output_dir = analysis_folder_path)
  
  cli::cli_alert_success("Output structure created:")
  cli::cli_alert_info("  Inputs: {.path {outputs_folders$inputs}}")
  cli::cli_alert_info("  Outputs: {.path {outputs_folders$outputs}}")
  cli::cli_alert_info("  Figures: {.path {outputs_folders$figures}}")
  cli::cli_alert_info("  Cache: {.path {cache_path}}")
  
  # Return outputs folders for use in subsequent code
  return(list(
    analysis_folder_path = analysis_folder_path,
    outputs_folders = outputs_folders,
    cache_path = cache_path
  ))
}
