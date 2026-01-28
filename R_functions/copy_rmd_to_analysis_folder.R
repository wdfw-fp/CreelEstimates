copy_rmd_to_analysis_folder <- function(analysis_folder) {
  # Ensure required packages are available
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    message("rstudioapi not available; relying on knitr::current_input()")
  }
  
  # 1. Determine the path of the current Rmd file
  current_rmd <- tryCatch(
    {
      # Works when knitting via render()
      knitr::current_input()
    },
    error = function(e) {
      # Works when running interactively in RStudio
      if (requireNamespace("rstudioapi", quietly = TRUE)) {
        rstudioapi::getActiveDocumentContext()$path
      } else {
        stop("Unable to determine the current Rmd path.")
      }
    }
  )
  
  if (is.null(current_rmd) || current_rmd == "") {
    stop("Could not determine the path of the current Rmd file.")
  }
  
  # 2. Create the analysis folder if it doesn't exist
  if (!dir.exists(analysis_folder)) {
    dir.create(analysis_folder, recursive = TRUE, showWarnings = FALSE)
  }
  
  # 3. Build destination path
  dest_rmd <- file.path(analysis_folder, basename(current_rmd))
  
  # 4. Copy the file
  success <- file.copy(from = current_rmd, to = dest_rmd, overwrite = TRUE)
  
  if (!success) {
    stop("Failed to copy Rmd file to analysis folder.")
  }
  
  # Return the path invisibly for downstream use
  invisible(dest_rmd)
}