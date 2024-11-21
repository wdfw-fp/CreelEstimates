#function to establish database connection
#requires a local config file that is on gitignore list
establish_db_con<- function(max_attempts = 5, delay_seconds = 3) {
  
  #load config file
  config <- tryCatch({
    yaml::read_yaml("config.yml")
  }, error = function(e) {
    stop("Failed to read config file: ", conditionMessage(e))
  })
  
  #get username and check credentials
  username <- unname(Sys.info()["user"])
  
  if(!username %in% config$users) {
    cli::cli_alert_danger("System username not found in database credentials configuration.")
  }
  
  #retry mechanism
  con <- NULL
  for (attempt in 1:max_attempts) {
    
    con <- tryCatch({
      DBI::dbConnect(
        RPostgres::Postgres(),
        host = config$server$host,
        port = config$server$port,
        dbname = config$server$database_FISH,
        #use system username and prompt user for their password
        user = username,
        password = rstudioapi::askForPassword("Please enter your password.")
      )
    }, error = function(e) {
      message(paste("Attempt", attempt, "failed:", conditionMessage(e)))
      Sys.sleep(delay_seconds)
      NULL
    })
    if (!is.null(con)) break
  }
  
  #check if connection was established
  if (!DBI::dbIsValid(con)) {
    stop("Failed to establish connection after ", max_attempts, " attempts.")
  } else {
    cat("\nDatabase connection established.\n")
  }
  
  return(con)
}
