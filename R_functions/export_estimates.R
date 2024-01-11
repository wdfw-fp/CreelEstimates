######################################################################################################################################## #
### Function structure ####
######################################################################################################################################## #
#add roxygen2-style description comments

export_estimates <- function(params, estimates_pe=NULL, estimates_bss=NULL) {
  
  # Extract parameters
  data_grade <- params$data_grade
  export_data <- params$export

  #Check that at least one model output is provided
  if (is.null(estimates_pe) && is.null(estimates_bss)) {
    stop("At least one of 'estimates_pe' or 'estimates_bss' must be supplied.")
  }
  
  # Process PE estimates to common format with internal function
  if (!is.null(estimates_pe)) {
    
    #call function
    transformed_pe_data <- process_estimates_pe(estimates_pe)
    
    #Get PE and BSS dataframes to match before binding rows
    
    #table 1, stratum_catch
    transformed_pe_data$pe_stratum_catch <- transformed_pe_data$pe_stratum_catch %>% 
      mutate(event_date = as.Date(NA))
    
    #table 2, stratum_effort
    transformed_pe_data$pe_stratum_effort <- transformed_pe_data$pe_stratum_effort %>% 
      mutate(est_cg = NA,
             event_date = as.Date(NA))
    
    #table 3, summarized_catch
    transformed_pe_data$pe_summarized_catch <- transformed_pe_data$pe_summarized_catch
    
    #table 4, summarized_effort
    transformed_pe_data$pe_summarized_effort <- transformed_pe_data$pe_summarized_effort %>% 
      mutate(est_cg = NA)
  }

  # Process BSS estimates to common format with internal function
  if (!is.null(estimates_bss)) {
    
    #call function
    transformed_bss_data <- process_estimates_bss(estimates_bss)
    
    #Get PE and BSS dataframes to match before binding rows
    
    #table 1, stratum_catch
    transformed_bss_data$bss_stratum_catch <- transformed_bss_data$bss_stratum_catch %>% 
      rename(period = "week", estimate_category = "estimate") %>% 
      select(-c("month", "estimate_index", "day_index")) %>% 
      mutate(day_type = NA,
             event_date = as.Date(event_date)) #matching date format pre-bind
    
    #table 2, stratum_effort
    transformed_bss_data$bss_stratum_effort <- transformed_bss_data$bss_stratum_effort %>% 
      rename(period = "week", estimate_category = "estimate") %>% 
      select(-c("month", "estimate_index", "day_index")) %>% 
      mutate(day_type = NA,
             event_date = as.Date(event_date)) #matching date format pre-bind
    
    #table 3, summarized_catch
    transformed_bss_data$bss_summarized_catch <- transformed_bss_data$bss_summarized_catch %>% 
      rename(estimate_category = "estimate") 
    
    #table 4, summarized_effort
    transformed_bss_data$bss_summarized_effort <- transformed_bss_data$bss_summarized_effort %>% 
      rename(estimate_category = "estimate")
    
  }
  
  # ------------------------------------------------------------------------#
  
  #Map data_grade column to every table
  data_grade_lower <- tolower(data_grade) #accept capitalization

  if (data_grade_lower == "approved") {

    transformed_bss_data <- map(transformed_bss_data,
                                ~{.$data_grade <- rep("Approved", nrow(.)); .})
    transformed_pe_data <- map(transformed_pe_data,
                               ~{.$data_grade <- rep("Approved", nrow(.)); .})

  } else if (data_grade_lower == "provisional") {

    transformed_bss_data <- map(transformed_bss_data,
                               ~{.$data_grade <- rep("Provisional", nrow(.)); .})
    transformed_pe_data <- map(transformed_pe_data,
                               ~{.$data_grade <- rep("Provisional", nrow(.)); .})

  } else {
    stop("Invalid value for data_grade. Use 'approved' or 'provisional'.")
  }
  # ------------------------------------------------------------------------#
  #Combine PE and BSS before exporting
  
  creel_estimates <- list(
    #table 1
    stratum_catch = rbind(transformed_pe_data$pe_stratum_catch, transformed_bss_data$bss_stratum_catch),
    #table 2
    stratum_effort = rbind(transformed_pe_data$pe_stratum_effort, transformed_bss_data$bss_stratum_effort),
    #table 3
    summarized_catch = rbind(transformed_pe_data$pe_summarized_catch, transformed_bss_data$bss_summarized_catch),
    #table 4
    summarized_effort = rbind(transformed_pe_data$pe_summarized_effort, transformed_bss_data$bss_summarized_effort)
  )
  
  #combine catch and effort data
  creel_estimates$stratum <- rbind(creel_estimates$stratum_catch,
                                   creel_estimates$stratum_effort)
  
  creel_estimates$total <- rbind(creel_estimates$summarized_catch,
                                 creel_estimates$summarized_effort)

  #assign to global env
  creel_estimates <<- creel_estimates

  # ------------------------------------------------------------------------#
  # Connect to database and conditionally export
  if(export_data) {
    
    # FOR DEMO WRITE TO CSV FILES
    write.csv(creel_estimates$stratum, file = paste0(params$fishery_name, "_creel estimates_stratum.csv"), row.names = F)
    write.csv(creel_estimates$total, file = paste0(params$fishery_name, "_creel estimates_total.csv"))  
    write.csv(analysis_lut, file = "creel_analysis_lut.csv", row.names = F)
    
    # --------------------------------------------------#
    # This internal function copies a given fishery analysis script to a shared network drive
    # as a way to archive any programming deviations from the template fw_creel.Rmd analysis
    
    archive_analysis_script <- function(params) {
      #define location of local analysis script
      local_path <- file.path(here(), "fishery_analyses", 
                              params$project_name, params$fishery_name,
                              paste0(
                                "fw_creel_", params$fishery_name, ".Rmd"
                              ))
      
      #define path for shared network drive
      # teams_path <- file.path(path.expand(
      #   "~/OneDrive - Washington State Executive Branch Agencies/General - DFW-Team FP FW Creel Monitoring Program/Analysis/catch_estimation_scripts"
      # ))
      
      teams_path <- file.path(
        paste0("C:/Users/", Sys.getenv("USERNAME"),
               "/OneDrive - Washington State Executive Branch Agencies",
               "/General - DFW-Team FP FW Creel Monitoring Program",
               "/Analysis/catch_estimation_scripts")
      )
      
      #fishery-specific file structure
      folder_str <- file.path(params$project_name, params$fishery_name)
      
      #apply that file structure within Teams folder
      archived_str <- paste0(teams_path, "/", folder_str)
      
        # create folder structure if it does not already exist
      if (!dir.exists(archived_str)) {
          dir.create(archived_str, recursive = TRUE)
        }
        
        #copy analysis file to shared drive
        ## might need to build in error handling for file.path nchar() > 260
        file.copy(from=local_path, to=file.path(archived_str,
                                        paste0(params$fishery_name, "_",
                                               params$data_grade, "_", Sys.Date(), ".Rmd")), 
                  overwrite = TRUE)
        
        
      }

    
    #call internal function
    archive_analysis_script(params)
    
    # --------------------------------------------------#
    
    # #Establish connection with database
    # con <- DBI::dbConnect(RPostgres::Postgres(),
    #                       dbname = "",
    #                       host = "",
    #                       port = 1234,
    #                       user = "",
    #                       #interactively ask user for password
    #                       password = rstudioapi::askForPassword("Database password"))
    # 
    # #View tables and fields?
    # #dbListTables(con)
    # #dbListFields(con, "")
    # #dbExistsTable returns logical
    # 
    # #Write data to database, https://rpostgres.r-dbi.org/reference/postgres-tables.html
    # RPostgres::dbWritetable(
    #   conn = con,
    #   name = "",
    #   value = "",
    #   row.names = FALSE,
    #   overwrite = FALSE,
    #   append = TRUE,
    #   #if append = TRUE, field types can be defined, or be interpreted automatically from db via DBI::dbDataType()
    #   field.types = ""
    # )
    # 
    # #Write data to analysis_lut
    # RPostgres::dbWriteTable(
    #   conn = con,
    #   name = "",
    #   value = "",
    #   row.names = FALSE,
    #   overwrite = FALSE,
    #   append = TRUE,
    #   #if append = TRUE, field types can be defined, or be interpreted automatically from db via DBI::dbDataType()
    #   field.types = ""    
    # )
    # 
    # #Confirm upload?
    # #dbReadTable(con, "") #recent batch only?
    # print("Data sucessfully exported.")
    # 
    # #Disconnect from database
    # dbDisconnect(con)
    
    cat("Data sucessfully exported.")
  } else {
    cat("Data not exported to database.")
  }
}
