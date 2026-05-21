# Render the current template against a fishery during template development

#' Render a fishery script after refreshing it from the template
#'
#' Convenience function for iterating on the master template against a real
#' fishery. Saves any open documents, verifies the fishery script either does
#' not exist or matches the template (to protect in-season tweaks), refreshes
#' the fishery script from the template, and renders it in a fresh R
#' environment with session_mode forced to "new" so dev iterations never
#' collide with production renders.
#'
#' Not for production use. Production renders should call rmarkdown::render()
#' directly on stable per-fishery scripts.
#'
#' @param project_name Name of the project (string).
#' @param fishery_name Name of the fishery (string).
#' @param template Template name without extension. Defaults to "fw_creel".
#'
#' @return Invisibly returns the path to the rendered output.
dev_render <- function(project_name, fishery_name, template = "fw_creel") {
  
  # ---- Save any in-progress edits across the RStudio editor ----
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::documentSaveAll()
  }
  
  # ---- Resolve paths ----
  fishery_folder <- here::here("fishery_analyses", project_name, fishery_name)
  fishery_script <- file.path(
    fishery_folder, paste0(template, "_", fishery_name, ".Rmd")
  )
  template_path <- here::here("template_scripts", paste0(template, ".Rmd"))
  
  if (!file.exists(template_path)) {
    cli::cli_abort("Template not found at {.path {template_path}}.")
  }
  
  # ---- Protect against silently overwriting in-season tweaks ----
  # If a fishery script exists and differs from the template, abort. The user
  # must explicitly opt in via init_fishery(..., overwrite_script = TRUE) and
  # then render directly. Matching scripts are safe to refresh (no-op effect).
  if (file.exists(fishery_script)) {
    fishery_lines <- readLines(fishery_script, warn = FALSE)
    template_lines <- readLines(template_path, warn = FALSE)
    if (!identical(fishery_lines, template_lines)) {
      cli::cli_abort(c(
        "Fishery script at {.path {fishery_script}} differs from the template.",
        "i" = "It may contain in-season edits. dev_render() will not overwrite it.",
        "i" = "To overwrite explicitly, run:",
        " " = "{.code init_fishery({.val {project_name}}, {.val {fishery_name}}, overwrite_script = TRUE)}",
        "i" = "Then knit the fishery script directly, or call dev_render() again."
      ))
    }
  }
  
  # ---- Refresh the fishery script from the template ----
  init_fishery(
    project_name = project_name,
    fishery_name = fishery_name,
    template = template,
    overwrite_script = TRUE,
    open = FALSE
  )
  
  # ---- Render with forced new session in an isolated environment ----
  cli::cli_alert_info("Rendering {.path {fishery_script}} (session_mode = new)")
  rendered <- rmarkdown::render(
    input = fishery_script,
    params = list(session_mode = "new"),
    envir = new.env(),
    quiet = FALSE
  )
  
  relocated <- move_render_to_session(
    rendered_path = rendered,
    session_folder = .find_latest_session(fishery_folder)
  )
  # Returns the relocated path on success or the original render path if the move failed
  invisible(relocated %||% rendered)
}
