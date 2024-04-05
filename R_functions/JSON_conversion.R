#R function to convert CreelEstimates script to .rmd to .json file format and back
library(jsonlite)

### ----------------------------------------------------------------------- ####

JSON_conversion <- function(params, direction) {
  
  if (direction == "toJSON") {
    #save local script to archive any alterations from template script
    rstudioapi::documentSave()
    
    #locate script location on local repository clone
    analysis_script <- readLines(paste0(
      getwd(), "/fishery_analyses/", params$project_name, "/", params$fishery_name,
      "/", "fw_creel_", params$fishery_name, ".Rmd"
      ))
    
    #convert to JSON format and validate
    JSON_script <- jsonlite::toJSON(analysis_script, pretty = TRUE)
    valid <- jsonlite::validate(JSON_script)
    
    if(valid) { #if TRUE
      
      #add JSON string into creel analysis look up table as new column
      analysis_lut <<- analysis_lut %>% 
        mutate(analysis_json = JSON_script)
      
    } else {
        stop("Not a valid JSON string")
    }
  
  } else if (direction == "fromJSON") {
    
    #convert back from JSON format
    parse_JSON <- jsonlite::fromJSON(analysis_lut$analysis_json)
    reconstructed_script <- paste(parse_JSON, collapse = "\n")
    
    #export reconstructed analysis file
    writeLines(reconstructed_script, paste0(params$project_name, "_", params$fishery_name,
                                            "_", "reconstructed_analysis.Rmd"))
    
    #open file for viewing
    file.edit(paste0(getwd(),"/", params$project_name, "_", params$fishery_name,
                     "_", "reconstructed_analysis.Rmd"))
    
  } else {
    stop("Invalid direction argument. Choose 'toJSON' or 'fromJSON'.")
  }
}
