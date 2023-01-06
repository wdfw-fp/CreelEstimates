## Establish a new analysis project ##
wd <- getwd()

project <- "District 14"
fishery_name <- "Cascade fall salmon 2022"
est_date_start <- "2022-09-01"
est_date_end <- "2022-11-30"

script_name <- paste(fishery_name, est_date_start, est_date_end, ".Rmd", sep = "_") # character string for analysis used to name new folder and script

ifelse(!dir.exists(paste("project_scripts", project, sep = "/")),
       {dir.create(paste("project_scripts", project, sep = "/")); "Output sub-folder created"},
       "Output sub-folder exists already")

file.copy(from = file.path(paste(wd, "fw_creel.Rmd", sep = "/")), 
          to = file.path(paste(wd,"project_scripts", project, script_name, 
                     sep = "/")))

file.rename(from = file.path(paste(wd, "projects", project, sep = "/"), "fw_creel.Rmd"), to = file.path(paste(wd, "projects", project, sep = "/"),
                                                                                                        paste("fw_creel_", project, ".Rmd", sep = "")))
