# This function creates the necessary directory structure for a fishery and
# copies template files for both "fw_creel" and "QAQC" scripts to the new folder
establish_dir <- function(params) {
  #fishery-specific destination
  path <- file.path("fishery_analyses", params$project_name, params$fishery_name)
  
  #create sub directories
  if (!dir.exists(here(path))) {
    dir.create(here(path), recursive = TRUE)
  }
  
  #destination of new files
  analysis <- here::here(path, paste0("fw_creel_", params$fishery_name, ".Rmd"))
  qaqc <- here::here(path, paste0("QAQC_", params$fishery_name, ".Qmd"))
  
  #copy and open new files as necessary
  if (file.exists(analysis) || file.exists(qaqc)) {
    cat("File(s) within fishery-specific subfolders already exist.")
  } else {
    #save template with user-input params
    rstudioapi::documentSave(rstudioapi::getActiveDocumentContext()$id)
    
    #copy template files
    file.copy(here::here("template_scripts/fw_creel.Rmd"), analysis)
    file.copy(here::here("template_scripts/QAQC_script.Qmd"), qaqc)
    
    #open new copies
    rstudioapi::documentOpen(analysis)
    rstudioapi::documentOpen(qaqc)
  }
}