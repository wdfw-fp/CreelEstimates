#This function attempts to verify that the model estimates have been written as expected and that the same number of
#rows, columns, and values have been written. Currently this is in draft form and needs to be more comprehensive.

confirm_db_upload <- function(con, analysis_lut) {

  #query database for records that match the estimates that were just written
  verification_table <- fetch_db_table(con, "creel", "model_analysis_lut") |> select(analysis_id, analysis_name)

  if (analysis_lut$analysis_id %in% verification_table$analysis_id) {
    
    DBI::dbDisconnect(con)
    cat(paste("\nData sucessfully exported.", "\u2713"))
    cat("\nDisconnecting from database.")
    
  } else {
    #what to do if analysis_id is not in analysis_lut (partial/failed export)
    message("\nUnable to confirm upload by checking database for session analysis_id.")
    
    message(paste("\nWriting",crayon::red$bgYellow("FAILED_UPLOAD_LOG_analysis_lut.csv") , "to CreelEstimates folder so that analysis_id for partial data upload can be investigated."))
    
    readr::write_csv(analysis_lut, file = paste0("FAILED_UPLOAD_LOG_","analysis_lut.csv"), append = TRUE)
    
    DBI::dbDisconnect(con)
    stop("\nDisconnecting from database.")
  }
}