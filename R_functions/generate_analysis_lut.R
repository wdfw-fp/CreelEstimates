#Function to generate an analysis look up table with session-specific uuid and session info
generate_analysis_lut <- function(params) {
  #check that analysis uuid
  if (!exists("analysis_lut", envir = .GlobalEnv)) {
 
    #initialize look up table that records metadata for each analysis session
    analysis_lut <<- data.frame(analysis_id = character(0), 
                                analysis_name = character(0),
                                repo_version = character(0),
                                r_session_json = character(0)
                                ) #fishery_regulations_json and analysis_json fields added by other processes later in ETL

    # record R session info and convert to JSON
    user <- paste("User:", Sys.info()["user"])
    analysis_session_info <- sessioninfo::session_info() |> capture.output()
    analysis_session_info[1] <- paste(user, analysis_session_info[1])
    analysis_session_info <- analysis_session_info |> 
      jsonlite::toJSON(pretty = TRUE)
    
    #Generate uuid and put in analysis_lut
    analysis_id <- uuid::UUIDgenerate()
    
    analysis_lut <<- rbind(analysis_lut, 
                           data.frame(analysis_id,
                                      analysis_name = paste0(
                                                      params$fishery_name, "_", params$data_grade, "_",
                                                      #ISO 8601 datetime format
                                                      format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ")
                                                    ),
                                      r_session_json = analysis_session_info,
                                      repo_version = paste0("https://github.com/wdfw-fp/CreelEstimates/tree/",
                                                            system("git rev-parse HEAD", intern = TRUE))
                                      )
                           )
  } else {
    cat("analysis_lut with unique 'analysis_id' already generated in this R session.")
  }
}
