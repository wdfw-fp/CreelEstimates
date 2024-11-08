#This function is the primary control of creel model estimate ETL process
#It calls upon PE and BSS outputs to independently reformat to a standardized format
#The resulting objects are either output locally as a csv or uploaded to the creel database

export_estimates <- function(params, analysis_lut, creel_estimates) {
  
  # Connect to database and conditionally export
  if(params$export == tolower("database")) {
    
    estimate_reviewers <- c("holc2477", "booe1477", "bentlktb") #please don't manually modify this list :) 
    
    if (!Sys.info()["user"] %in% estimate_reviewers && params$data_grade == tolower("approved")) {
      stop("Creel project leads may only upload estimates with a data_grade of 'provisional'.")
    }
    
    #convert metadata to json. Automatically added to analysis_lut
    json_conversion(type = "script")
    json_conversion(type = "r_session")
    
    #connect to database
    con <- establish_db_con()
    assign("con", con, envir = .GlobalEnv)

    #query database for UUIDs and reformat
    creel_estimates_db <- prep_export(con, creel_estimates)
    assign("creel_estimates_db", creel_estimates_db, envir = .GlobalEnv)
        
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
    
    ### write estimates to database ####

    #call function that defines how tables are written to database
    write_db_tables(con, analysis_lut, creel_estimates_db)
    
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
          cat(paste0("Writing to model_analysis_lut table...  ", "\n"))
          write_lut(con, analysis_lut)
          
          cat(paste0("Writing to model_estimates_total table...  ", "\n"))
          write_total(con)
          
        } else if (params$export_tables == "stratum") {
          
          #write lut and stratum
          cat(paste0("Writing to model_analysis_lut table...  ", "\n"))
          write_lut(con, analysis_lut)
          
          cat(paste0("Writing to model_estimates_stratum table...  ", "\n"))
          write_stratum(con)
          
        } else if (params$export_tables == "both") {
          
          #write lut, total, and stratum
          cat(paste0("Writing to model_analysis_lut table...  ", "\n"))
          write_lut(con, analysis_lut)
          
          cat(paste0("Writing to model_estimates_total table...  ", "\n"))
          write_total(con)
          
          cat(paste0("Writing to model_estimates_stratum table...  ", "\n"))
          write_stratum(con)
          
        } else {
          cat("\nParameter export_tables must be either 'total', 'stratum', or 'both'.")
        }
        
      } else {
      # If confirmation = No
      cat("Writing to database tables aborted.\n")
      return(NULL)
      }
    }
  
  #verify that estimates have been written as expected 
  cat("\nUploading complete. Verifying session 'analysis_id' in database analysis look up table.")
  confirm_db_upload(con, analysis_lut)
    
  #local export option
  } else if (params$export == tolower("local")) {
    #process for exporting ETL output tables locally for inspection prior to uploading to database
    
    #convert metadata to json. Automatically added to analysis_lut
    json_conversion(type = "script")
    json_conversion(type = "r_session")
    
    #project- and fishery-specific folder from CreelEstimates
    #could be more flexible and make folders where needed? for case of recreation of script on computer that did run analysis
    write_directory <- paste0(getwd(), "/fishery_analyses/", params$project_name, "/", params$fishery_name,"/")
    
    #write csv files to local working directory
    readr::write_csv(analysis_lut, file = paste0(write_directory,"analysis_lut.csv"))
    readr::write_csv(creel_estimates$stratum, file = paste0(write_directory,"model_estimates_stratum.csv"))
    readr::write_csv(creel_estimates$total, file = paste0(write_directory, "model_estimates_total.csv"))  
    
    cat("\n\n")
    cat("Standardized model estimate tables and analysis_lut saved to fishery folder on local computer.")
  
  #do not write out estimates    
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
