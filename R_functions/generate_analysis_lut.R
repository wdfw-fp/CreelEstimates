# Generate or load the analysis lookup table for a creel analysis session.
#
# Session resolution is driven by params$session_mode and the on-disk contents
# of the fishery folder. The script's own file location is not consulted.

# Helper: scan a fishery folder for a session folder whose date suffix is today.
# Returns the folder name (basename), or NULL if none found.
# Warns when multiple same-day folders exist and resolves by most recent mtime.
.find_todays_session <- function(fishery_folder) {
  if (!dir.exists(fishery_folder)) {
    return(NULL)
  }
  today_suffix <- format(Sys.Date(), "%Y%m%d")
  pattern <- paste0("^[A-Z0-9]+_[A-Z0-9]{4}_", today_suffix, "$")
  candidates <- list.dirs(fishery_folder, recursive = FALSE, full.names = FALSE)
  hits <- candidates[grepl(pattern, candidates)]
  
  if (length(hits) == 0) {
    return(NULL)
  }
  if (length(hits) > 1) {
    cli::cli_warn(c(
      "Multiple session folders match today's date ({.val {today_suffix}}):",
      "*" = paste(hits, collapse = ", "),
      "i" = "Resuming the most recently modified one. To override, set {.code session_mode} to {.val new} or to a specific folder name."
    ))
  }
  mtimes <- file.info(file.path(fishery_folder, hits))$mtime
  hits[which.max(mtimes)]
}

# Helper: confirm that an explicitly named session folder exists under the
# fishery folder. Returns the folder name if it exists, NULL otherwise.
# Does not validate folder contents; the caller checks for the LUT file.
.locate_session_folder <- function(fishery_folder, folder_name) {
  target <- file.path(fishery_folder, folder_name)
  if (dir.exists(target)) folder_name else NULL
}


#' Generate or load the analysis lookup table for a creel analysis session
#'
#' Resolves the analysis session for a given fishery based on params$session_mode:
#'   - "auto" (default): resume today's session under the fishery folder if one
#'     exists with a valid LUT; otherwise mint a new session.
#'   - "new": mint a new session unconditionally, leaving any existing same-day
#'     folder untouched.
#'   - "<folder_name>" matching ^[A-Z0-9]+_[A-Z0-9]{4}_\\d{8}$: resume the named
#'     session. Aborts if the folder does not exist or contains no LUT file.
#'
#' @param params List of parameters from Rmd YAML header. Must contain
#'   project_name, fishery_name, data_grade. May contain session_mode.
#' @param current_file_path Retained for backward compatibility; unused.
#'
#' @return A one-row data.frame with columns analysis_id, analysis_name,
#'   fishery_name, identifier, analysis_folder, repo_version, created_datetime.
generate_analysis_lut <- function(params, current_file_path = NULL) {
  
  # ---- Validate required params ----
  if (is.null(params$project_name) || params$project_name == "") {
    cli::cli_abort("params$project_name is required but missing or empty.")
  }
  if (is.null(params$fishery_name) || params$fishery_name == "") {
    cli::cli_abort("params$fishery_name is required but missing or empty.")
  }
  
  # ---- Resolve session_mode ----
  # If params$session_mode is missing, assign auto handling
  session_mode <- if (is.null(params$session_mode)) "auto" else params$session_mode
  
  folder_name_pattern <- "^[A-Z0-9]+_[A-Z0-9]{4}_\\d{8}$"
  valid_mode <- session_mode %in% c("auto", "new") ||
    grepl(folder_name_pattern, session_mode)
  
  if (!valid_mode) {
    cli::cli_abort(c(
      "Invalid {.code session_mode}: {.val {session_mode}}",
      "i" = "Must be {.val auto}, {.val new}, or a session folder name matching {.code IDENTIFIER_XXXX_YYYYMMDD} (e.g., {.val SFS24_AB12_20260519})."
    ))
  }
  
  fishery_folder <- here::here(
    "fishery_analyses", params$project_name, params$fishery_name
  )
  
  # ---- Resolve which existing session (if any) to load ----
  existing_folder <- switch(
    session_mode,
    "new"  = NULL,
    "auto" = .find_todays_session(fishery_folder),
    .locate_session_folder(fishery_folder, session_mode)
  )
  
  # For explicit folder-name mode, a missing folder is a hard error.
  if (!session_mode %in% c("auto", "new") && is.null(existing_folder)) {
    cli::cli_abort(c(
      "Session folder {.val {session_mode}} not found under {.path {fishery_folder}}.",
      "i" = "To start a new session, set {.code session_mode} to {.val new} or {.val auto}."
    ))
  }
  
  # ---- Attempt to load the existing LUT ----
  if (!is.null(existing_folder)) {
    lut_file <- file.path(
      fishery_folder, existing_folder,
      paste0("analysis_lut_", existing_folder, ".rds")
    )
    
    if (file.exists(lut_file)) {
      existing_lut <- readRDS(lut_file)
      cli::cli_alert_success("Resumed session {.val {existing_folder}}")
      cli::cli_alert_info("analysis_id: {.val {existing_lut$analysis_id}}")
      # Early return: a successfully resumed session bypasses the mint-new
      # block below. New-session minting is reachable only when no LUT loads.
      return(invisible(existing_lut))
    }
    
    # Folder exists but no LUT.
    # Under "auto", treat as an orphan (e.g., crashed mid-setup) and mint new.
    # Under explicit folder mode, abort: the user named a session that cannot be resumed.
    if (session_mode == "auto") {
      cli::cli_warn(c(
        "Session folder {.path {existing_folder}} exists but has no analysis_lut RDS file.",
        "i" = "Treating as orphaned and creating a new session."
      ))
    } else {
      cli::cli_abort(c(
        "Session folder {.val {existing_folder}} exists but contains no analysis_lut RDS file.",
        "i" = "Cannot resume. Remove or rename the folder, or use {.code session_mode = \"new\"}."
      ))
    }
  }
  
  # ---- Mint a new session ----
  # Reached when no session was resumed: either no existing folder was found,
  # or an orphaned folder (auto mode only) triggered fall-through.
  analysis_id <- toupper(uuid::UUIDgenerate())
  cli::cli_alert_success("New analysis_id created: {.val {substr(analysis_id, 1, 8)}...}")
  
  fishery_name <- params$fishery_name
  year_format <- dplyr::case_when(
    stringr::str_detect(fishery_name, "\\d{4}-\\d{2}") ~ "yyyy-yy",
    stringr::str_detect(fishery_name, "\\b\\d{4}\\b")  ~ "yyyy",
    TRUE ~ "other"
  )
  last_two_digits <- dplyr::case_when(
    year_format == "yyyy-yy" ~ stringr::str_extract(fishery_name, "(?<=-)\\d{2}"),
    year_format == "yyyy"    ~ stringr::str_sub(stringr::str_extract(fishery_name, "\\b\\d{4}\\b"), -2),
    TRUE ~ NA_character_
  )
  
  initials <- fishery_name |>
    stringr::str_replace_all("\\band\\b|\\d{4}-\\d{2}|\\b\\d{4}\\b", "") |>
    stringr::str_extract_all("\\b\\w") |>
    purrr::map_chr(~ paste(.x, collapse = "")) |>
    stringr::str_to_upper()
  
  identifier <- paste0(initials, last_two_digits)
  
  analysis_folder <- paste(
    identifier,
    stringr::str_sub(analysis_id, -4),
    format(Sys.Date(), "%Y%m%d"),
    sep = "_"
  )
  
  analysis_lut <- data.frame(
    analysis_id      = analysis_id,
    analysis_name    = paste0(
      params$fishery_name, "_", params$data_grade, "_",
      format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ")
    ),
    fishery_name     = params$fishery_name,
    identifier       = identifier,
    analysis_folder  = analysis_folder,
    repo_version     = tryCatch(
      paste0(
        "https://github.com/wdfw-fp/CreelEstimates/tree/",
        system("git rev-parse HEAD", intern = TRUE)
      ),
      error = function(e) "Git version unavailable"
    ),
    created_datetime = format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ"),
    stringsAsFactors = FALSE
  )
  
  return(invisible(analysis_lut))
}






# # Function to generate or load analysis lookup table with session-specific uuid
# generate_analysis_lut <- function(params, current_file_path = NULL) {
#   
#   # Try to get current file path if not provided
#   if (is.null(current_file_path)) {
#     current_file_path <- tryCatch({
#       if (!is.null(knitr::current_input()) && knitr::current_input() != "") {
#         here::here("template_scripts", knitr::current_input())
#       } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
#         rstudioapi::getSourceEditorContext()$path
#       } else {
#         NULL
#       }
#     }, error = function(e) NULL)
#   }
#   
#   # Check if we're running from an existing analysis folder
#   existing_analysis_lut <- NULL
#   if (!is.null(current_file_path) && current_file_path != "" && file.exists(current_file_path)) {
#     current_dir <- dirname(current_file_path)
#     
#     # Check if current directory matches analysis folder pattern
#     # Pattern: IDENTIFIER_XXXX_YYYYMMDD (e.g., SFS25_ABCD_20241222)
#     folder_name <- basename(current_dir)
#     analysis_pattern <- "^[A-Z0-9]+_[A-Z0-9]{4}_\\d{8}$"
#     
#     if (grepl(analysis_pattern, folder_name)) {
#       # Look for existing analysis_lut RDS file
#       lut_file <- file.path(current_dir, paste0("analysis_lut_", folder_name, ".rds"))
#       
#       if (file.exists(lut_file)) {
#         existing_analysis_lut <- readRDS(lut_file)
#         cli::cli_alert_success("Loaded existing analysis_lut from {.path {lut_file}}")
#         cli::cli_alert_info("Using analysis_id: {.val {existing_analysis_lut$analysis_id}}")
#         assign("analysis_lut", existing_analysis_lut, envir = .GlobalEnv)
#         return(invisible(existing_analysis_lut))
#       }
#     }
#   }
#   
#   # If no existing analysis found, create new one
#   if (!exists("analysis_lut", envir = .GlobalEnv)) {
#     
#     # Generate session-specific analysis uuid
#     analysis_id <- uuid::UUIDgenerate()
#     analysis_id <- toupper(analysis_id)  # Capitalize to match database format
#     cli::cli_alert_success("New analysis_id created: {.val {substr(analysis_id, 1, 8)}...}")
#     
#     # Create fishery identifier
#     fishery_name <- params$fishery_name
#     year_format <- dplyr::case_when(
#       stringr::str_detect(fishery_name, "\\d{4}-\\d{2}") ~ "yyyy-yy",
#       stringr::str_detect(fishery_name, "\\b\\d{4}\\b") ~ "yyyy",
#       TRUE ~ "other"
#     )
#     
#     last_two_digits <- dplyr::case_when(
#       year_format == "yyyy-yy" ~ stringr::str_extract(fishery_name, "(?<=-)\\d{2}"),
#       year_format == "yyyy" ~ stringr::str_sub(stringr::str_extract(fishery_name, "\\b\\d{4}\\b"), -2),
#       TRUE ~ NA_character_
#     )
#     
#     # Extract initials from fishery name
#     initials <- fishery_name |>
#       stringr::str_replace_all("\\band\\b|\\d{4}-\\d{2}|\\b\\d{4}\\b", "") |>
#       stringr::str_extract_all("\\b\\w") |>
#       purrr::map_chr(~ paste(.x, collapse = "")) |>
#       stringr::str_to_upper()
#     
#     identifier <- paste0(initials, last_two_digits)
#     
#     # Create analysis folder name: IDENTIFIER_LAST4_YYYYMMDD
#     analysis_folder <- paste(
#       identifier,
#       stringr::str_sub(analysis_id, -4),
#       format(Sys.Date(), "%Y%m%d"),
#       sep = "_"
#     )
#     
#     # Create analysis_lut data frame
#     analysis_lut <- data.frame(
#       analysis_id = analysis_id,
#       analysis_name = paste0(
#         params$fishery_name, "_", params$data_grade, "_",
#         format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ")
#       ),
#       fishery_name = params$fishery_name,
#       identifier = identifier,
#       analysis_folder = analysis_folder,
#       repo_version = tryCatch({
#         paste0("https://github.com/wdfw-fp/CreelEstimates/tree/",
#                system("git rev-parse HEAD", intern = TRUE))
#       }, error = function(e) "Git version unavailable"),
#       created_datetime = format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ"),
#       stringsAsFactors = FALSE
#     )
#     
#     assign("analysis_lut", analysis_lut, envir = .GlobalEnv)
#     
#   } else {
#     cli::cli_alert_info("analysis_lut with unique 'analysis_id' already exists in this R session.")
#     analysis_lut <- get("analysis_lut", envir = .GlobalEnv)
#   }
#   
#   return(invisible(analysis_lut))
# }