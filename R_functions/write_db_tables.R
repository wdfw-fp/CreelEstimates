#This script validates that tables are ready to uploading and then defines functions to write to each table.
#Currently write_lut includes robust error handling to ensure all fields write to the database, but this could be expanded.

#define function to check for NOT NULL constraints in analysis_lut
validate_lut <- function(data) {
  required_columns <- c("analysis_id", "analysis_name", "r_session_json", "analysis_json", "repo_version")
  missing_values <- sapply(required_columns, function(col) any(is.na(data[[col]])))
  
  if (any(missing_values)) {
    stop("The following columns contain NA values: ", paste(names(missing_values)[missing_values], collapse = ", "))
  }
  return(data)
}

#define functions for writing tables
#model_analysis_lut
write_lut <- function(con, table, max_retries = 10) {
  #ensure data adheres to NOT NULL constraints
  analysis_lut <- validate_lut(table)
  
  attempt <- 1
  success <- FALSE
  
  #attempt to write table, retrying if any NOT NULL constraints are violated
  while (attempt <= max_retries && !success) {
    tryCatch({
      dbWriteTable(
        conn = con, 
        name = Id(schema = "creel", table = "model_analysis_lut"),
        value = analysis_lut,
        row.names = FALSE,
        overwrite = FALSE,
        append = TRUE
      )
      
      success <- TRUE
      message("Data written successfully on attempt ", attempt)
      
    }, error = function(e) {
      message("Attempt ", attempt, " failed: ", e$message)
      attempt <- attempt + 1
      
      if (attempt > max_retries) {
        stop("Failed to write data after ", max_retries, " attempts.")
      }
    })
  }
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
