## Establish a new analysis project ##

## This script:
## 1) specifies the folder location ("project") where 
## 2) a copy of the fw_creel.Rmd script for a specific analysis is saved and 
## 3) creates an analysis specific folder for analysis outputs

library(here)

# parameters used to specify the analysis 
project_name <- "District 14"
fishery_name <- "Skagit fall salmon 2022"
est_date_start <- "2022-09-01"
est_date_end <- "2022-11-30"


# create folder for project_name and script

## create a folder for the project, if it doesn't already exist
ifelse(!dir.exists(paste(here(), "project_scripts", project_name, sep = "/")),
       {dir.create(paste(here(), "project_scripts", project_name, sep = "/")); "Output sub-folder created"},
       "Output sub-folder exists already")

## character string for analysis used to name new .Rmd script file
script_name <-paste(paste(fishery_name, est_date_start, est_date_end, sep = "_"), ".Rmd", sep = "")

## copies fw_creel.Rmd template into specified project folder and renames to match specified analysis
file.copy(from = file.path(paste(here(), "fw_creel.Rmd", sep = "/")),
          to = file.path(paste(here(),"project_scripts", project_name, script_name,
                               sep = "/")))

# create analysis output folders ## move to .Rmd and add params control for local or Teams 

# create a folder for the project, if it doesn't already exist
# ifelse(!dir.exists(paste(here(), "project_outputs", project_name, sep = "/")),
#        {dir.create(paste(here(), "project_outputs", project_name, sep = "/")); "Output sub-folder created"},
#        "Output sub-folder exists already")
# 
# 
# # character string analysis used to name the folder containing outputs 
# analysis_name <- paste(fishery_name, est_date_start, est_date_end, sep = "_")
# 
# ifelse(!dir.exists(paste(here(), "project_outputs", project_name, analysis_name, sep = "/")),
#        {dir.create(paste(here(), "project_outputs", project_name, analysis_name, sep = "/")); "Output sub-folder created"},
#        "Output sub-folder exists already")
