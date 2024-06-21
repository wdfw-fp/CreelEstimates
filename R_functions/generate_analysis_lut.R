#Function to generate base analysis look up table with session-specific uuid
generate_analysis_lut <- function(params) {
  #check that analysis uuid
  if (!exists("analysis_lut", envir = .GlobalEnv)) {
    
    #initialize look up table
    analysis_lut <- NULL
    
    #generate session-specific analysis uuid
    analysis_id <- uuid::UUIDgenerate()
    cat("\nUnique analysis_id created for this R session.")
    
    analysis_lut <- rbind(analysis_lut, 
                           data.frame(analysis_id,
                                      analysis_name = paste0(
                                                      params$fishery_name, "_", params$data_grade, "_",
                                                      #ISO 8601 datetime format
                                                      format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ")
                                                    ),
                                      repo_version = paste0("https://github.com/wdfw-fp/CreelEstimates/tree/",
                                                            system("git rev-parse HEAD", intern = TRUE))
                                      )
                           )
    return(analysis_lut)
    
    } else {
    cat("\nanalysis_lut with unique 'analysis_id' already generated in this R session.")
  }
}
