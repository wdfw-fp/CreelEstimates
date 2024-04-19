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
    
    #locate script location on CreelEstimates local repository clone
    analysis_script <- readLines(paste0(
      getwd(), "/fishery_analyses/", params$project_name, "/", params$fishery_name,
      "/", "fw_creel_", params$fishery_name, ".Rmd"
    ))
    
    #convert to JSON format and validate
    json_script <- jsonlite::toJSON(analysis_script, pretty = TRUE)
    valid <- jsonlite::validate(json_script)
    
    if(valid) { #if TRUE
      
      #add to analysis look up table 
      analysis_lut <<- analysis_lut |>  dplyr::mutate(analysis_json = json_script)
    }
  }
  if (type == "regulations") {
    
    #requires an established ODBC connection to the FISH database named "con"
    if (!DBI::dbIsValid(con)) {
      stop("Must be connected to the FISH database via ODBC connection named 'con'.")
    }
    
    #query database for regulations
    
    #convert to json and validate
    
    if (valid) {
      #add to analysis look up table      
    }
  } 
  if (type == "r_session") {
    
    #record user and R session info
    user <- paste("User:", Sys.info()["user"])
    r_session <- sessioninfo::session_info() |> capture.output()
    r_session[1] <- paste(user, r_session[1])
    
    #convert to json and validate
    r_session_json <- r_session |> jsonlite::toJSON(pretty = TRUE)
    valid <- jsonlite::validate(r_session_json)
    
    if (valid) {
      #add to analysis look up table
      analysis_lut <<- analysis_lut |>  dplyr::mutate(r_session_json = r_session_json)
    }
  } 
}

#examples
# json_conversion(type = "script")
# json_conversion(type = "r_session")



# ORIGINAL VERSION OF FUNCTION, 4/18/24 C.H.
# ### ----------------------------------------------------------------------- ####
# 
# JSON_conversion <- function(params, direction) {
#   
#   if (direction == "toJSON") {
#     #save local script to archive any alterations from template script
#     rstudioapi::documentSave()
#     
#     #locate script location on local repository clone
#     analysis_script <- readLines(paste0(
#       getwd(), "/fishery_analyses/", params$project_name, "/", params$fishery_name,
#       "/", "fw_creel_", params$fishery_name, ".Rmd"
#       ))
#     
#     #convert to JSON format and validate
#     JSON_script <- jsonlite::toJSON(analysis_script, pretty = TRUE)
#     valid <- jsonlite::validate(JSON_script)
#     
#     if(valid) { #if TRUE
#       
#       #add JSON string into creel analysis look up table as new column
#       analysis_lut <<- analysis_lut %>% 
#         mutate(analysis_json = JSON_script)
#       
#     } else {
#         stop("Not a valid JSON string")
#     }
#   
#   } else if (direction == "fromJSON") {
#     
#     #convert back from JSON format
#     parse_JSON <- jsonlite::fromJSON(analysis_lut$analysis_json)
#     reconstructed_script <- paste(parse_JSON, collapse = "\n")
#     
#     #export reconstructed analysis file
#     writeLines(reconstructed_script, paste0(params$project_name, "_", params$fishery_name,
#                                             "_", "reconstructed_analysis.Rmd"))
#     
#     #open file for viewing
#     file.edit(paste0(getwd(),"/", params$project_name, "_", params$fishery_name,
#                      "_", "reconstructed_analysis.Rmd"))
#     
#   } else {
#     stop("Invalid direction argument. Choose 'toJSON' or 'fromJSON'.")
#   }
# }