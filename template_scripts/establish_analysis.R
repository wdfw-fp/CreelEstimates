## Establish a new analysis project ##

## This script:
## 1) specifies the folder location ("project") where 
## 2) a copy of the fw_creel.Rmd script for a specific analysis is saved and 
## 3) creates an analysis specific folder for analysis outputs

library(here)

# parameters used to specify the analysis 
project_name <- "District 14"
fishery_name <- "Skagit fall salmon 2022" #needs to match what is in database
est_date_start <- "2022-09-01"
est_date_end <- "2022-11-30"
output_location_filepath <- "local"
output_teams_name <- "DFW-Team FP FW Creel Monitoring Program - General"


analysis_name <- paste(fishery_name, est_date_start, est_date_end, sep = "_")
## character string for analysis used to name new .Rmd script file
analysis_script_name <-paste0("fw_creel_", analysis_name, ".Rmd")
## character string for analysis used to name new .R render script file
render_script_name <-paste0("render_analysis_", analysis_name, ".R")

# create folder for project_name and script

## create a folder for the project, if it doesn't already exist
ifelse(!dir.exists(here::here("project_scripts", project_name)),
       {dir.create(here::here("project_scripts", project_name)); "Output sub-folder created"},
       "Output sub-folder exists already")

##create a folder for the analysis
ifelse(!dir.exists(here::here("project_scripts", project_name, fishery_name)),
       {dir.create(here::here("project_scripts", project_name, fishery_name)); "Output sub-folder created"},
       "Output sub-folder exists already")

## copies fw_creel.Rmd template into specified project folder and renames to match specified analysis
file.copy(from = here::here("template_scripts", "fw_creel.Rmd"),
          to = here("project_scripts", project_name, fishery_name, analysis_script_name))

# copies render_analysis.R script to specified project folder and renames to match specified analysis
file.copy(from = here::here("template_scripts", "render_analysis.R"),
          to = here::here("project_scripts", project_name, fishery_name, render_script_name))

# !! under development / futher consideration 
# write out analysis parameters to rds file used to auto-fill project specific render_analysis.R script 

params_list <- list(
  project_name = project_name,
  fishery_name = fishery_name,
  est_date_start = est_date_start,
  est_date_end = est_date_end,
  output_location_filepath = output_location_filepath,
  output_teams_name = output_teams_name
)

saveRDS(params_list, file = here::here("input_files","params_list.rds"))
