# Save analysis metadata files (Rmd, analysis_lut, params, css) to analysis folder,
# and relocate rendered HTML into the session folder after render completion.

# Helper: ensure a copy of the WDFW CSS file exists in the fishery folder.
# Pandoc resolves the YAML `css:` reference relative to the source Rmd; under
# the per-fishery-script structure, this means the fishery folder must contain
# the CSS file for the rendered HTML to pick up styling.
# This helper is intentionally isolated so it can be removed cleanly once
# init_fishery() is the authoritative source of fishery folder setup.
.ensure_fishery_css <- function(fishery_folder) {
  css_filename <- "styleRmd_WDFW.css"
  css_source <- here::here("template_scripts", css_filename)
  css_dest <- file.path(fishery_folder, css_filename)
  
  if (!file.exists(css_source)) {
    cli::cli_alert_warning("Source CSS not found at {.path {css_source}}; skipping fishery-level copy.")
    return(invisible(NULL))
  }
  if (file.exists(css_dest)) {
    return(invisible(NULL))
  }
  file.copy(from = css_source, to = css_dest, overwrite = FALSE)
  cli::cli_alert_success("CSS file copied to fishery folder: {.path {css_dest}}")
  invisible(NULL)
}


# Helper: locate the most recently modified session folder under a fishery folder.
# Used by move_render_to_session() to identify where a render's HTML should go.
# Returns the full path of the latest session folder, or NULL if none exist.
.find_latest_session <- function(fishery_folder) {
  if (!dir.exists(fishery_folder)) {
    return(NULL)
  }
  pattern <- "^[A-Z0-9]+_[A-Z0-9]{4}_\\d{8}$"
  candidates <- list.dirs(fishery_folder, recursive = FALSE, full.names = TRUE)
  hits <- candidates[grepl(pattern, basename(candidates))]
  if (length(hits) == 0) {
    return(NULL)
  }
  mtimes <- file.info(hits)$mtime
  hits[which.max(mtimes)]
}


#' Save analysis metadata files to the session folder
#'
#' Copies a snapshot of the source Rmd into the session folder, ensures CSS
#' exists at both the fishery and session levels, and writes analysis_lut and
#' params metadata. Each side effect is wrapped in its own tryCatch so that a
#' failure in one operation does not silently prevent others.
#'
#' @param params List of parameters from Rmd YAML header.
#' @param analysis_lut Analysis lookup table from generate_analysis_lut().
#' @param analysis_folder_path Path to the session folder.
#' @param current_file_path Path to the source Rmd file. Required; abort if NULL.
#'
#' @return Invisibly returns NULL.
save_analysis_metadata <- function(params, analysis_lut, analysis_folder_path,
                                   current_file_path = NULL) {
  
  # ---- Validate required inputs ----
  if (is.null(current_file_path) || current_file_path == "" || !file.exists(current_file_path)) {
    cli::cli_abort(c(
      "current_file_path is required and must point to an existing file.",
      "i" = "Got: {.val {current_file_path %||% 'NULL'}}",
      "i" = "The caller should resolve this via {.code knitr::current_input(dir = TRUE)} during a knit, or {.code rstudioapi::getSourceEditorContext()$path} interactively."
    ))
  }
  
  fishery_folder <- dirname(analysis_folder_path)
  analysis_folder_name <- basename(analysis_folder_path)
  
  # ---- Copy source Rmd into the session folder (script snapshot) ----
  tryCatch({
    new_filename <- paste0("fw_creel_", analysis_folder_name, ".Rmd")
    destination_path <- file.path(analysis_folder_path, new_filename)
    
    if (!file.exists(destination_path)) {
      file.copy(from = current_file_path, to = destination_path, overwrite = FALSE)
      cli::cli_alert_success("Source Rmd snapshot copied to {.path {destination_path}}")
    } else {
      cli::cli_alert_info("Source Rmd snapshot already exists: {.path {destination_path}}")
    }
  }, error = function(e) {
    cli::cli_alert_warning("Failed to copy source Rmd snapshot: {e$message}")
  })
  
  # ---- Ensure CSS exists at the fishery folder level (self-healing during transition) ----
  tryCatch({
    .ensure_fishery_css(fishery_folder)
  }, error = function(e) {
    cli::cli_alert_warning("Failed to ensure fishery-level CSS: {e$message}")
  })
  
  # ---- Copy CSS into the session folder for snapshot portability ----
  tryCatch({
    css_filename <- "styleRmd_WDFW.css"
    css_source <- here::here("template_scripts", css_filename)
    css_dest <- file.path(analysis_folder_path, css_filename)
    
    if (!file.exists(css_source)) {
      cli::cli_alert_warning("Source CSS not found at {.path {css_source}}; session snapshot will not include CSS.")
    } else if (!file.exists(css_dest)) {
      file.copy(from = css_source, to = css_dest, overwrite = FALSE)
      cli::cli_alert_success("CSS snapshot copied to {.path {css_dest}}")
    } else {
      cli::cli_alert_info("CSS snapshot already exists: {.path {css_dest}}")
    }
  }, error = function(e) {
    cli::cli_alert_warning("Failed to copy CSS snapshot: {e$message}")
  })
  
  # ---- Save analysis_lut RDS (load-bearing for session resumption) ----
  tryCatch({
    analysis_lut_filename <- paste0("analysis_lut_", analysis_folder_name, ".rds")
    analysis_lut_path <- file.path(analysis_folder_path, analysis_lut_filename)
    
    if (!file.exists(analysis_lut_path)) {
      saveRDS(analysis_lut, analysis_lut_path)
      cli::cli_alert_success("analysis_lut saved to {.path {analysis_lut_path}}")
    } else {
      cli::cli_alert_info("analysis_lut already exists: {.path {analysis_lut_path}}")
    }
  }, error = function(e) {
    cli::cli_alert_warning("Failed to save analysis_lut: {e$message}")
  })
  
  # ---- Save params YAML ----
  tryCatch({
    params_filename <- paste0("params_", analysis_folder_name, ".yml")
    params_path <- file.path(analysis_folder_path, params_filename)
    
    if (!file.exists(params_path)) {
      yaml::write_yaml(params, params_path)
      cli::cli_alert_success("params saved to {.path {params_path}}")
    } else {
      cli::cli_alert_info("params already exists: {.path {params_path}}")
    }
  }, error = function(e) {
    cli::cli_alert_warning("Failed to save params: {e$message}")
  })
  
  invisible(NULL)
}


#' Move a rendered HTML file from the fishery folder into the session folder
#'
#' rmarkdown::render() writes the output HTML next to the source Rmd by default.
#' Under the per-fishery-script structure, this means the HTML lands in the
#' fishery folder rather than the session folder. This helper relocates it.
#'
#' Intended to be called by the render driver (render.R, dev_render()) after
#' rmarkdown::render() returns. Interactive knits via the RStudio Knit button
#' have no caller-side hook and will leave the HTML in the fishery folder.
#'
#' @param rendered_path Path returned by rmarkdown::render() (the location
#'   where the HTML was actually written).
#' @param session_folder Path to the session folder where the HTML should be
#'   moved. If NULL, the function attempts to infer it as the most recently
#'   modified session folder under dirname(rendered_path).
#'
#' @return Invisibly returns the new path of the HTML, or NULL if the move
#'   was skipped.
move_render_to_session <- function(rendered_path, session_folder = NULL) {
  
  if (is.null(rendered_path) || !file.exists(rendered_path)) {
    cli::cli_alert_warning("Rendered file not found at {.path {rendered_path %||% 'NULL'}}; nothing to move.")
    return(invisible(NULL))
  }
  
  if (is.null(session_folder)) {
    session_folder <- .find_latest_session(dirname(rendered_path))
    if (is.null(session_folder)) {
      cli::cli_alert_warning("No session folder found under {.path {dirname(rendered_path)}}; leaving rendered HTML in place.")
      return(invisible(NULL))
    }
  }
  
  destination <- file.path(session_folder, basename(rendered_path))
  
  ok <- file.rename(from = rendered_path, to = destination)
  if (!ok) {
    cli::cli_alert_warning("Failed to move rendered HTML to {.path {destination}}.")
    return(invisible(NULL))
  }
  
  cli::cli_alert_success("Rendered HTML moved to {.path {destination}}")
  invisible(destination)
}