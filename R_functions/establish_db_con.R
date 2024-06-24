#function to establish database connection
#requires a local config file that is on gitignore list
establish_db_con<- function(max_attempts = 5, delay_seconds = 3) {
  
  #load config file
  config <- yaml::read_yaml("config.yml")
  
  #retry mechanism
  attempt <- 1
  con <- NULL
  
  while (attempt <= max_attempts && is.null(con)) {
    # Attempt to establish connection
    con <- tryCatch({
      DBI::dbConnect(
        RPostgres::Postgres(),
        host = config$server$host,
        port = config$server$port,
        dbname = config$server$database_FISH,
        user = config$user$username,
        password = config$user$password
      )
    }, error = function(e) {
      cat("\n")
      message(paste("Attempt", attempt, "failed:", conditionMessage(e)))
      
      attempt <<- attempt + 1
      
      Sys.sleep(delay_seconds)
      # Return NULL to retry
      NULL
    })
  }
  
  #check if connection was established
  if (!DBI::dbIsValid(con)) {
    stop("Failed to establish connection after ", max_attempts, " attempts.")
  } else {
    cat("\n\n")
    cat("Database connection established.")
  }
  
  # #show con in RStudio Connections pane
  # odbc:::on_connection_opened(con,
  #                             paste(
  #                               c(paste("con <-", gsub(", ", ",\n\t", c(match.call()))
  #                                       )
  #                                 ), collapse = "\n"
  #                               )
  #                             )
  
  return(con)
}
