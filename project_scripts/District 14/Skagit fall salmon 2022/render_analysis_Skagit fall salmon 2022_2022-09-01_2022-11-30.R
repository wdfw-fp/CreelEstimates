library(rmarkdown)
library(here)
library(tidyverse)

analysis_params <- readRDS(here::here("input_files", "params_list.rds"))
analysis_name <- paste(analysis_params$fishery_name, analysis_params$est_date_start, analysis_params$est_date_end, sep = "_")

input_file <- paste0("fw_creel_", analysis_name, ".Rmd")

wd_output <- if_else(analysis_params$output_location_filepath == "local", here::here("project_outputs", analysis_params$project_name, analysis_name),
                                   paste("T:", analysis_params$output_teams_name, "project_outputs", analysis_params$project_name, analysis_name, sep = "/"))

# add control for output to either local or teams 

Sys.setenv(RSTUDIO_PANDOC = 'C:/Program Files/RStudio/bin/pandoc')
rmarkdown::render(
  input = here::here("project_scripts", analysis_params$project_name, analysis_name, paste0("fw_creel_", analysis_name, ".Rmd")),
  output_file = paste0("report_", analysis_name, ".html"),
  output_dir = wd_output)
