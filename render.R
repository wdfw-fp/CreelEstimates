# ==============================================================================
# GLOBAL RENDER SCRIPT - render.R
# This script gets called by scheduled tasks
# ==============================================================================

# Setup temporary file location
Sys.setenv(TMPDIR = "C:/temp")
Sys.setenv(TMP = "C:/temp") 
Sys.setenv(TEMP = "C:/temp")
dir.create("C:/temp", showWarnings = FALSE, recursive = TRUE)

# Quietly load packages
suppressMessages(suppressWarnings({
  library(cli)
  library(here)
  library(tidyverse)
  library(glue)
  
  devtools::install_github("wdfw-fp/creelutils@patch_etl")
  library(creelutils)
}))

# Set pandoc path for Task Scheduler access
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")

# Start logging
logs_dir <- file.path(here(), "logs")
if (!dir.exists(logs_dir)) {
  dir.create(logs_dir, recursive = TRUE)
}

log_filename <- paste0("render_", format(Sys.time(), "%d%b%Y_%I.%M.%S%p"), ".log")
log_file <- file.path(logs_dir, log_filename)
file.create(log_file)

# Record start time
time1 <- Sys.time()

# Logging function to standardize messaging
log_msg <- function(msg) {
  # print to console
  cat(msg, "\n")
  
  # try to write to log
  tryCatch({
    cat(msg, "\n", file = log_file, append = TRUE)
  }, error = function(e) {
    cat("WARNING: Could not write to log file:", conditionMessage(e), "\n")
  })
}

# Active Fisheries #####################################################################################################
# List of fishery names and analysis script locations. The rendering process will loop over each in this list
Active <- list(
  ## Snohomish fall salmon 2025 ####
  list(
    fishery = "Snohomish fall salmon 2025",
    file_path = "fishery_analyses/District 13/Snohomish fall salmon 2025/fw_creel_Snohomish fall salmon 2025.Rmd"
  )
)

# Record high level information for session
log_msg(paste0("\n\n====================== New script run ",format(Sys.Date(), "%m-%d-%Y")," ======================"))
log_msg("CreelEstimates - BATCH RENDERING REPORT")
log_msg(paste("Timestamp:",format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
log_msg(paste("Working directory:", getwd()))
log_msg(paste("Processing", length(Active), "fisheries"))

# MAIN EXECUTION ####

# Process each fishery in Active list ####
for (i in seq_along(Active)) {
  fishery_info <- Active[[i]]
  fishery_name <- fishery_info$fishery
  rmd_path <- fishery_info$file_path
  
  log_msg(paste("---------- Starting:", fishery_name, "----------"))
  
  # Check if file exists
  if (!file.exists(rmd_path)) {
    log_msg(paste("File not found:", rmd_path))
    next # continue to next fishery in loop
  }
  
  # Set output_dir as the directory containing the Rmd
  output_dir <- dirname(rmd_path)
  
  # Render ####
  tryCatch({
    log_msg("(1) Starting render process...")
    rmarkdown::render(
      input = rmd_path,
      output_dir = output_dir,
      quiet = TRUE
    )
    log_msg(paste("Successfully completed:", paste0(fishery_name, ".html")))
    log_msg(paste("Output created at:", output_dir))
  
    # Reset environment except essentials
    rm(list = setdiff(ls(), c("Active", "i", "log_file", "log_msg", "time1")))
  },
  # Record error messages
  error = function(e) {
    log_msg(paste("!! Failed to render", fishery_name, ":", conditionMessage(e)))
  },
  # Record warning messages
  warning = function(w) {
    log_msg(paste("Warning in", fishery_name, ":", conditionMessage(w)))
  })
  
########################################################################
  # Copy files within each fishery folder into the CreelEstimates clone on the Shared Teams drive
  # Performs action when files in destination folder do not exist or are out of date
  
  log_msg("(2) Copying files to shared drive...")
  
  copy_to_teams <- function( ####
    dirs, # Source file locations, assumes within CreelEstimates/ folder
    dest_root = "C:/Users/holc2477/Washington State Executive Branch Agencies/DFW-Team FP FW Creel Monitoring Program - General/CreelEstimates/fishery_analyses"
    ) {

    # Initialize file counts
    copied <- 0
    skipped <- 0
    failed <- 0
    
    # Loop through fishery folders
    for (d in dirs) {
      
      # Check that this folder exists as expected
      if (!dir.exists(d)) {
        log_msg(paste("WARNING: Source directory not found:", d))
        next
      }

      # List files to copy, but exclude any inside *_files/ folders. These are temporary files that may not be cleaned up after a failed rendering.
      files <- list.files(d, recursive = TRUE, full.names = TRUE)
      files <- files[!grepl("_files/", files)]
      
      # Loop through each file and copy to destination folder
      for (f in files) {
        
        if (!file.exists(f)) {
          log_msg(paste("WARNING: File not found:", f))
          failed <- failed + 1
          next
        }
        # Minor modification of path
        rel_path <- sub(".*fishery_analyses/", "", f)
        dest <- file.path(dest_root, rel_path)
        
        # Create fishery level folder in destination as necessary
        dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
        
        # Check timestamps to see if destination file is out of date
        if (file.exists(dest)) {
          src_time  <- file.info(f)$mtime # new local
          dest_time <- file.info(dest)$mtime # existing in shared
          
          if (dest_time >= src_time) { # Skip copying file if the timestamps are the same
            log_msg(paste("    Skipping (up to date):", rel_path))
            skipped <- skipped + 1
            next
          }
        }
        
        # Copy file if new/updated from the version in shared drive
        ok <- file.copy(f, dest, overwrite = TRUE)
        if (ok) {
          log_msg(paste("    Copied:", rel_path))
          copied <- copied + 1
        } else {
          log_msg(paste("WARNING: Failed to copy:", f))
          failed <- failed + 1
        }
      }
    }
    # Record copying summary
    log_msg(paste("File copy summary:", copied, "copied,", skipped, "skipped,", failed, "failed."))
  }
  
  # Call the function to copy files
  copy_to_teams(
    dirs = c(
    "C:/Repos/CreelEstimates/fishery_analyses/District 13/Snohomish fall salmon 2025"
    )
  )

  # Calculate and report model run time
  log_msg("(3) Logging run time...")
  time2 <- Sys.time()
  runtime <- time2 - time1
  secs <- as.numeric(runtime, units = "secs")
  
  # Return run time in readable format
  if (secs < 60) {
    log_msg(paste("Model run time =", round(secs, 1), "seconds"))
  } else if (secs < 3600) {
    log_msg(paste("Model run time =", round(secs/60, 1), "minutes"))
  } else {
    log_msg(paste("Model run time =", round(secs/3600, 2), "hours"))
  }
  
  log_msg("BATCH JOB COMPLETED")

}

# When this run programmatically, close R after finishing loop
# This occurs when run in headless mode by Task Scheduler program
if (!interactive()) {
  quit(status = 0)
}
