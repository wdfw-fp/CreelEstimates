# function for "first time run" of fw_creel.Rmd that copies, renames, and saves a new analysis script in it's respective fishery_analyses/project/fishery_name location

prep_analysis_script <- function(
    project,
    fishery,
    ...){
  
  
  if (!dir.exists(here("fishery_analyses"))) {
    dir.create(here("fishery_analyses")); "fishery_analyses folder created"
  }
  ## project level
  if (!dir.exists(here("fishery_analyses", params$project_name))) {
    dir.create(here("fishery_analyses", params$project_name))
    paste(here("fishery_analyses", params$project_name), "folder created")
  }
  ## fishery-specific
  if (!dir.exists(here("fishery_analyses", params$project_name, params$fishery_name))) {
    dir.create(here("fishery_analyses", params$project_name, params$fishery_name))
    paste(here("fishery_analyses", params$project_name, params$fishery_name), "folder created")
  }
  
  #save the master template with new params arguments for the fishery
  rstudioapi::documentSave(rstudioapi::getActiveDocumentContext()$id)
  #make an appropriately renamed copy
  file.copy(
    here("template_scripts/fw_creel.Rmd"),
    here("fishery_analyses", params$project_name, params$fishery_name, 
         paste0("fw_creel_", params$fishery_name, ".Rmd"))
  )
  #open the new copy
  rstudioapi::documentOpen(
    here("fishery_analyses", params$project_name, params$fishery_name, 
         paste0("fw_creel_", params$fishery_name, ".Rmd"))
  )
  
}
