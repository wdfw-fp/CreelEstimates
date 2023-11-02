#Function to generate a universally unique identification (uuid) number for each CreelEstimates session
generate_analysis_uuid <- function(params) {
  # Check if analysis_id or analysis_lut exist for session and create new if necessary
  if (!exists("analysis_id", envir = .GlobalEnv) | !exists("analysis_lut", envir = .GlobalEnv)) {
    
    # Initalize analysis look up table in Global Env
    analysis_lut <<- data.frame(analysis_id = character(0), 
                                analysis_name = character(0),
                                analysis_params = character(0),
                                session_info = character(0),
                                repo_version = character(0))
    
    #-------------------------------------------------------------------#
    #Internal function to process session info
    capture_session_info <- function() {
      #Get info
      r_version <- R.version$version.string
      os_info <- paste(Sys.info()["sysname"], Sys.info()["release"], Sys.info()["version"]) 
      user <- Sys.info()["user"]
      # loaded_packages <- as.character(sessionInfo()$loadedOnly)
      
      session_info <- paste(
        r_version,
        "Operating System:",  os_info,
        "User:", user,
        # "Loaded Packages:", paste(loaded_packages, collapse = ", "),
        sep = " ")
      
      return(session_info)
    }
    #-------------------------------------------------------------------#
    
    #Generate uuid and assign to object in Global Env
    analysis_id <- uuid::UUIDgenerate()
    assign("analysis_id", analysis_id, envir = .GlobalEnv)
    
    #Populate analysis look up table
    analysis_lut <<- rbind(analysis_lut, data.frame(analysis_id, 
                                                    analysis_name = paste0(
                                                      params$fishery_name, "_" ,params$model_type, "_", params$data_grade, "_", 
                                                      #ISO 8601 datetime format
                                                      format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ")
                                                    ),
                                                    session_info = capture_session_info(), #apply internal function
                                                    repo_version = paste0("https://github.com/wdfw-fp/CreelEstimates/tree/",
                                                                          system("git rev-parse HEAD", intern = TRUE))
    )
    )
  } else {
    cat("'analysis_id' already generated in this R session.")
  }
}