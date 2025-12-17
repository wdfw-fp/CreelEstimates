# ==============================================================================
# SCHEDULED TASK DEFINER - Run once at beginning of season
# ==============================================================================

library(taskscheduleR)
library(here)
library(cli)
library(dplyr)
library(stringr)

fishery_list <- list(
  # list(
  #   project = "District 99",
  #   fishery = "Humptulips salmon 2025",
  #   schedule = "DAILY",
  #   time = "19:00"
  # ),
  # list(
  #   project = "District 99", 
  #   fishery = "Skagit winter gamefish 2025",
  #   schedule = "DAILY", 
  #   time = "19:30",
  #   day = 2 # Tuesday (1=Mon, 2=Tue, ..., 7=Sun)
  # )
  list(
    project = "District 16",
    fishery = "Hoh winter steelhead 2022-23",
    schedule = "DAILY",
    time = "19:00"
  )
)

# ==============================================================================
# TASK CREATION FUNCTION
# ==============================================================================

created_tasks <- character(0)

create_fishery_task <- function(fishery_info) {
  
  # Generate unique task name by combining project and fishery, removing special chars
  task_name <- paste0("creel_", 
                      gsub("[^A-Za-z0-9]", "_", fishery_info$project), "_", 
                      gsub("[^A-Za-z0-9]", "_", fishery_info$fishery))
  
  # Path to the global render script that will be called by Windows Task Scheduler
  render_script_path <- here("render.R")
  
  # Base parameters for Windows Task Scheduler
  task_params <- list(
    taskname = task_name,                                    # Unique name in Task Scheduler
    rscript = normalizePath(render_script_path, winslash = "/"),  # Path to render script
    schedule = fishery_info$schedule,                        # DAILY, WEEKLY, or MONTHLY
    starttime = fishery_info$time,                          # 24-hour format (e.g., "19:00")
    startdate = format(Sys.Date(), "%m/%d/%Y"),             # Today's date
    Rexe = file.path(R.home("bin"), "Rscript.exe")         # Path to Rscript executable
  )
  
  # Add day-specific parameter for weekly schedules
  if (fishery_info$schedule == "WEEKLY" && !is.null(fishery_info$day)) {
    days_lookup <- c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN")
    task_params$days <- days_lookup[fishery_info$day]
  }
  
  # Create the Windows scheduled task using taskscheduleR
  do.call(taskscheduler_create, task_params)
  
  return(task_name)
}

# ==============================================================================
# EXECUTION - Process all fisheries in the list
# ==============================================================================

# Loop through each fishery configuration and create its scheduled task
for (fishery in fishery_list) {
  task_name <- create_fishery_task(fishery)
  created_tasks <- c(created_tasks, task_name)
}

# Final summary with structured output
cli({
  cli_alert_success("Finished creating {length(created_tasks)} scheduled task{?s}:")
  cli_ul(created_tasks)
})

# ==============================================================================
# VERIFICATION - Review list of creel tasks scheduled
# ==============================================================================

check <- taskscheduler_ls() |>
  filter(str_detect(TaskName, "creel_"))

nrow(check)

cat(check$TaskName, sep = "\n")

View(check)
