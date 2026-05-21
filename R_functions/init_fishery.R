# Initialize a fishery 

#' Initialize a fishery folder structure with copy of analysis template
#'
#' Creates the project and fishery folders if they do not exist, copies the
#' template script into the fishery folder as a per-fishery analysis script,
#' and copies the CSS file into the fishery folder. Optionally opens the
#' resulting script in RStudio.
#'
#' Intended to be run once per fishery at the start of analysis work. For
#' iterative template development against an existing fishery, use
#' `dev_render()` or call this function with overwrite_script = TRUE.
#'
#' @param project_name Project name (string).
#' @param fishery_name Fishery name (string).
#' @param template Template name without extension. Defaults to "fw_creel".
#'   The function expects {template_scripts}/{template}.Rmd to exist.
#' @param css CSS filename to copy from template_scripts/ to the fishery
#'   folder. Defaults to "styleRmd_WDFW.css".
#' @param overwrite_script If TRUE and a fishery script already exists, replace
#'   it with a fresh copy of the template. WARNING: this destroys any
#'   fishery-level changes from the template script. Default FALSE.
#' @param open If TRUE and RStudio is available, open the resulting script in
#'   the editor. Defaults to interactive().
#'
#' @return Invisibly returns the path to the fishery script.
init_fishery <- function(project_name,
                         fishery_name,
                         template = "fw_creel",
                         css = "styleRmd_WDFW.css",
                         overwrite_script = FALSE,
                         open = interactive()) {
  
  # ---- Validate inputs ----
  if (missing(project_name) || is.null(project_name) || project_name == "") {
    cli::cli_abort("project_name is required.")
  }
  if (missing(fishery_name) || is.null(fishery_name) || fishery_name == "") {
    cli::cli_abort("fishery_name is required.")
  }
  
  template_path <- here::here("template_scripts", paste0(template, ".Rmd"))
  if (!file.exists(template_path)) {
    cli::cli_abort("Template {.val {template}} not found at {.path {template_path}}.")
  }
  
  css_path <- here::here("template_scripts", css)
  if (!file.exists(css_path)) {
    cli::cli_abort("CSS file {.val {css}} not found at {.path {css_path}}.")
  }
  
  # ---- Create folder structure ----
  fishery_folder <- here::here("fishery_analyses", project_name, fishery_name)
  if (!dir.exists(fishery_folder)) {
    dir.create(fishery_folder, recursive = TRUE)
    cli::cli_alert_success("Created fishery folder: {.path {fishery_folder}}")
  }
  
  # ---- Copy template into fishery folder ----
  fishery_script <- file.path(
    fishery_folder, paste0(template, "_", fishery_name, ".Rmd")
  )
  
  if (!file.exists(fishery_script)) {
    file.copy(from = template_path, to = fishery_script, overwrite = FALSE)
    cli::cli_alert_success("Copied template to {.path {fishery_script}}")
  } else if (overwrite_script) {
    cli::cli_alert_warning("Overwriting existing fishery script at {.path {fishery_script}}.")
    file.copy(from = template_path, to = fishery_script, overwrite = TRUE)
  } else {
    cli::cli_alert_info(c(
      "Fishery script already exists: {.path {fishery_script}}",
      "i" = " To replace it with a fresh copy of the template, call with {.code overwrite_script = TRUE}. This will erase any changes to that script from the template."
    ))
  }
  
  # ---- Copy CSS into fishery folder ----
  css_dest <- file.path(fishery_folder, css)
  if (!file.exists(css_dest)) {
    file.copy(from = css_path, to = css_dest, overwrite = FALSE)
    cli::cli_alert_success("Copied CSS to {.path {css_dest}}")
  } else {
    cli::cli_alert_info("CSS already exists at {.path {css_dest}}")
  }
  
  # ---- Open in editor if interactive ----
  if (isTRUE(open) &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    rstudioapi::documentOpen(fishery_script)
  }
  
  invisible(fishery_script)
}
