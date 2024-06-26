#This function is the primary control of creel model estimate ETL process
#It calls upon PE and BSS outputs to independently reformat to a standardized format
#The resulting objects are either output locally as a csv or uploaded to the creel database

export_estimates <- function(params, analysis_lut, creel_estimates) {
  
  # Connect to database and conditionally export
  if(params$export == tolower("database")) {
    
    estimate_reviewers <- c("holc2477", "booe1477", "bentlktb") #please don't manually modify this list :) 
    
    if (!Sys.info()["user"] %in% estimate_reviewers && params$data_grade == tolower("provisional")) {
      stop("Creel project leads may only upload estimates with a data_grade of 'provisional'.")
    }
    
    #convert metadata to json. Automatically added to analysis_lut
    json_conversion(type = "script")
    json_conversion(type = "r_session")
    
    #connect to database
    con <- establish_db_con()

    #query database for UUIDs and reformat
    creel_estimates <- prep_export(con, creel_estimates)
        
    ask_for_confirmation <- function() { #bug: repeats prompt twice in console upon call
        response <- ""
        repeat {
          if (response != "") {
            cat("Please enter 'Y' for Yes or 'N' for No.\n")
          }
          response <- toupper(trimws(readline("\nWould you like to proceed with upload? [Y/N]: ")))
          if (response %in% c("Y", "N")) {
            return(response == "Y")
          }
        }
    }
    
    #define functions for writing tables
    #model_analysis_lut
    write_lut <- function() {
      dbWriteTable(
        conn = con, 
        name = Id(schema = "creel", table = "model_analysis_lut"),
        value = analysis_lut,
        row.names = FALSE,
        overwrite = FALSE,
        append = TRUE)
    }
    
    #model_estimates_total
    write_total <- function() {
      DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = "creel", table = "model_estimates_total"),
      value = creel_estimates$total,
      row.names = FALSE,
      overwrite = FALSE,
      append = TRUE)
    }
    
    #model_estimates_stratum
    write_stratum <- function() {
      DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = "creel", table = "model_estimates_stratum"),
      value = creel_estimates$stratum,
      row.names = FALSE,
      overwrite = FALSE,
      append = TRUE)
    }
    
    ### write estimates to database ####

    #model_analysis_lut
    #determine if session analysis_id already exists in database model_analysis_lut table
    cat("\nVerifying that session 'analysis_id' does not exist in database before upload.")
    analysis_id_check <- fetch_db_table(con, "creel", "model_analysis_lut") |> select(analysis_id, analysis_name)

    if (analysis_lut$analysis_id %in% analysis_id_check$analysis_id) {
      cat("\n")
      stop("\nAnalysis uuid already exists in the creel database. Review before proceeding.")
    } else { #analysis_id not already in database
      
      #a pause, option to abort process
      proceed <- ask_for_confirmation()
  
      if (proceed) { #Y = TRUE
        cat("Continuing with upload...\n")
        Sys.sleep(3)
        
        #evaluate export_tables parameter
        if (params$export_tables == "total") {
          
          #write lut and total
          cat(paste0("Writing to model_analysis_lut table...  ","\u2713", "\n"))
          write_lut()
          
          cat(paste0("Writing to model_estimates_total table...  ","\u2713", "\n"))
          write_total()
          
        } else if (params$export_tables == "stratum") {
          
          #write lut and stratum
          cat(paste0("Writing to model_analysis_lut table...  ","\u2713", "\n"))
          write_lut()
          
          cat(paste0("Writing to model_estimates_stratum table...  ","\u2713", "\n"))
          write_stratum()
          
        } else if (params$export_tables == "both") {
          
          #write lut, total, and stratum
          cat(paste0("Writing to model_analysis_lut table...  ","\u2713", "\n"))
          write_lut()
          
          cat(paste0("Writing to model_estimates_total table...  ","\u2713", "\n"))
          write_total()
          
          cat(paste0("Writing to model_estimates_stratum table...  ","\u2713", "\n"))
          write_stratum()
          
        } else {
          cat("\nParameter export_tables must be either 'total', 'stratum', or 'both'.")
        }
        
      } else {
      # If confirmation = No
      cat("Writing to database tables aborted.\n")
      return(NULL)
      }
    }

    cat("Uploading complete. Verifying session 'analysis_id' in database analysis look up table.\n")

    #verify data has been sent to database
    # this could be made more comprehensive
    confirm_upload <- fetch_db_table(con, "creel", "model_analysis_lut") |> select(analysis_id, analysis_name)

    if (analysis_lut$analysis_id %in% confirm_upload$analysis_id) {

      DBI::dbDisconnect(con)
      cat(paste("Data sucessfully exported.", "\u2713","\n"))
      cat("Disconnecting from database.\n")
      
    } else {
      #what to do if analysis_id is not in analysis_lut (partial/failed export)
      cat("\n")
      message("Unable to confirm upload by checking database for session analysis_id.")
      
      cat("\n")
      message(paste("writing",crayon::red$bgYellow("FAILED_UPLOAD_LOG_analysis_lut.csv") , "to CreelEstimates folder so that analysis_id for partial data upload can be investigated."))
      
      readr::write_csv(analysis_lut, file = paste0("FAILED_UPLOAD_LOG_","analysis_lut.csv"), append = TRUE)
      
      DBI::dbDisconnect(con)
      stop("\nDisconnecting from database.")
    }
    
  } else if (params$export == tolower("local")) {
    #process for exporting ETL output tables locally for inspection prior to uploading to database
    
    #convert metadata to json. Automatically added to analysis_lut
    json_conversion(type = "script")
    json_conversion(type = "r_session")
    
    #project- and fishery-specific folder from CreelEstimates
    #could be more flexible and make folders where needed? for case of recreation of script on computer that did run analysis
    write_directory <- paste0(getwd(), "/fishery_analyses/", params$project_name, "/", params$fishery_name,"/")
    
    #write csv files to local working directory
    write_csv(analysis_lut, file = paste0(write_directory,"analysis_lut.csv"))
    write_csv(creel_estimates$stratum, file = paste0(write_directory,"model_estimates_stratum.csv"))
    write_csv(creel_estimates$total, file = paste0(write_directory, "model_estimates_total.csv"))  
    
    cat("\n\n")
    cat("Standardized model estimate tables and analysis_lut saved to fishery folder on local computer.")
    
  } else if (params$export == tolower("No")) {
    #send message to user that no ETL actions were taken
    cat("\n\n")
    cat("Catch and effort estimates not exported.")
    cat("\nStandardized model estimates can be viewed in output list object 'creel_estimates'.")

  } else {
    #send message to user with correct export parameter options
    cat("Export parameter must be either 'no', 'local', or 'database'.")
  }
}
