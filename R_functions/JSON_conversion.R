# This function converts model estimate metadata objects to json format

json_conversion <- function(type, ...) {
  
#check type
  valid_types <- c("script", "regulations", "r_session")
  if (!type %in% valid_types) {
    stop("Invalid type argument. Select 'script', 'regulations', or 'r_session'")
  }
  
#convert items to json format based on type
  if (type == "script") {

    #save local script to archive any alterations from template script
    rstudioapi::documentSave()
    
    cat("\nLocal analysis script saved.")
    
    #locate script location on CreelEstimates local repository clone
    analysis_script <- readLines(paste0(
      getwd(), "/fishery_analyses/", params$project_name, "/", params$fishery_name,
      "/", "fw_creel_", params$fishery_name, ".Rmd"
    ))
    
    #convert to JSON format and validate
    json_script <- jsonlite::toJSON(analysis_script, pretty = TRUE)
    
    cat("\nLocal script read and converted to JSON format.")
    
    valid <- jsonlite::validate(json_script)
    
    if(valid) { #if TRUE
      
      cat("\nJSON valid... adding `analysis_json` to analysis_lut.")
      
      #add to analysis look up table 
      analysis_lut <<- analysis_lut |>  dplyr::mutate(analysis_json = json_script)
      
    } else {
      warning("\nJSON format not valid! JSON representation of local script not added to analysis_lut.")
    }
  }
  if (type == "regulations") {
    
    # #requires an established ODBC connection to the FISH database named "con"
    # if (!DBI::dbIsValid(con)) {
    #   stop("Must be connected to the FISH database via ODBC connection named 'con'.")
    # }
    # 
    # #query database for regulations
    # 
    # #convert to json and validate
    # 
    # if (valid) {
    #   #add to analysis look up table      
    # }
  } 
  if (type == "r_session") {
    
    cat("\nGathering User and R session information.")
    
    #record user and R session info
    user <- paste("User:", Sys.info()["user"])
    r_session <- sessioninfo::session_info() |> capture.output()
    r_session[1] <- paste(user, r_session[1])
    
    #convert to json and validate
    r_session_json <- r_session |> jsonlite::toJSON(pretty = TRUE)
    
    cat("\nUser and R session information converted to JSON format.")
    
    valid <- jsonlite::validate(r_session_json)
    
    if (valid) {
      
      cat("\nJSON valid... adding `r_session_json` to analysis_lut.")
      
      #add to analysis look up table
      analysis_lut <<- analysis_lut |>  dplyr::mutate(r_session_json = r_session_json)
      
    } else {
      warning("\nJSON format not valid! JSON representation of R session information not added to analysis_lut.")
    }
  } 
}

#examples
# json_conversion(type = "script")
# json_conversion(type = "r_session")
