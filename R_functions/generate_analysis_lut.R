# Function to generate or load analysis lookup table with session-specific uuid
generate_analysis_lut <- function(params, current_file_path = NULL) {
  
  # Try to get current file path if not provided
  if (is.null(current_file_path)) {
    current_file_path <- tryCatch({
      if (!is.null(knitr::current_input()) && knitr::current_input() != "") {
        here::here("template_scripts", knitr::current_input())
      } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        rstudioapi::getSourceEditorContext()$path
      } else {
        NULL
      }
    }, error = function(e) NULL)
  }
  
  # Check if we're running from an existing analysis folder
  existing_analysis_lut <- NULL
  if (!is.null(current_file_path) && current_file_path != "" && file.exists(current_file_path)) {
    current_dir <- dirname(current_file_path)
    
    # Check if current directory matches analysis folder pattern
    # Pattern: IDENTIFIER_XXXX_YYYYMMDD (e.g., SFS25_ABCD_20241222)
    folder_name <- basename(current_dir)
    analysis_pattern <- "^[A-Z0-9]+_[A-Z0-9]{4}_\\d{8}$"
    
    if (grepl(analysis_pattern, folder_name)) {
      # Look for existing analysis_lut RDS file
      lut_file <- file.path(current_dir, paste0("analysis_lut_", folder_name, ".rds"))
      
      if (file.exists(lut_file)) {
        existing_analysis_lut <- readRDS(lut_file)
        cli::cli_alert_success("Loaded existing analysis_lut from {.path {lut_file}}")
        cli::cli_alert_info("Using analysis_id: {.val {existing_analysis_lut$analysis_id}}")
        assign("analysis_lut", existing_analysis_lut, envir = .GlobalEnv)
        return(invisible(existing_analysis_lut))
      }
    }
  }
  
  # If no existing analysis found, create new one
  if (!exists("analysis_lut", envir = .GlobalEnv)) {
    
    # Generate session-specific analysis uuid
    analysis_id <- uuid::UUIDgenerate()
    analysis_id <- toupper(analysis_id)  # Capitalize to match database format
    cli::cli_alert_success("New analysis_id created: {.val {substr(analysis_id, 1, 8)}...}")
    
    # Create fishery identifier
    fishery_name <- params$fishery_name
    year_format <- dplyr::case_when(
      stringr::str_detect(fishery_name, "\\d{4}-\\d{2}") ~ "yyyy-yy",
      stringr::str_detect(fishery_name, "\\b\\d{4}\\b") ~ "yyyy",
      TRUE ~ "other"
    )
    
    last_two_digits <- dplyr::case_when(
      year_format == "yyyy-yy" ~ stringr::str_extract(fishery_name, "(?<=-)\\d{2}"),
      year_format == "yyyy" ~ stringr::str_sub(stringr::str_extract(fishery_name, "\\b\\d{4}\\b"), -2),
      TRUE ~ NA_character_
    )
    
    # Extract initials from fishery name
    initials <- fishery_name |>
      stringr::str_replace_all("\\band\\b|\\d{4}-\\d{2}|\\b\\d{4}\\b", "") |>
      stringr::str_extract_all("\\b\\w") |>
      purrr::map_chr(~ paste(.x, collapse = "")) |>
      stringr::str_to_upper()
    
    identifier <- paste0(initials, last_two_digits)
    
    # Create analysis folder name: IDENTIFIER_LAST4_YYYYMMDD
    analysis_folder <- paste(
      identifier,
      stringr::str_sub(analysis_id, -4),
      format(Sys.Date(), "%Y%m%d"),
      sep = "_"
    )
    
    # Create analysis_lut data frame
    analysis_lut <- data.frame(
      analysis_id = analysis_id,
      analysis_name = paste0(
        params$fishery_name, "_", params$data_grade, "_",
        format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ")
      ),
      fishery_name = params$fishery_name,
      identifier = identifier,
      analysis_folder = analysis_folder,
      repo_version = tryCatch({
        paste0("https://github.com/wdfw-fp/CreelEstimates/tree/",
               system("git rev-parse HEAD", intern = TRUE))
      }, error = function(e) "Git version unavailable"),
      created_datetime = format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ"),
      stringsAsFactors = FALSE
    )
    
    assign("analysis_lut", analysis_lut, envir = .GlobalEnv)
    
  } else {
    cli::cli_alert_info("analysis_lut with unique 'analysis_id' already exists in this R session.")
    analysis_lut <- get("analysis_lut", envir = .GlobalEnv)
  }
  
  return(invisible(analysis_lut))
}