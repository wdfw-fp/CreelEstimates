#This function is the primary control of creel model estimate ETL process
#It calls upon PE and BSS outputs to indepedently reformat to a standardized format
#The resulting objects are either output locally as a csv or uploaded to the creel database

export_estimates <- function(params, estimates_pe=NULL, estimates_bss=NULL) {
  
  # Extract parameters
  data_grade <- params$data_grade
  export_data <- params$export
  
  #Look up tables (LUTs) from environment
  analysis_lut <- analysis_lut
  # estimate_extent <- estimate_extent
  
  #format for joining with database table later
  dwg$fishery_manager$catch_area_code <- as.character(dwg$fishery_manager$catch_area_code)

  #Check that at least one model output is provided
  if (is.null(estimates_pe) && is.null(estimates_bss)) {
    stop("At least one of 'estimates_pe' or 'estimates_bss' must be supplied.")
  }
  
  # Process PE estimates to common format with internal function
  if (!is.null(estimates_pe)) {
    
    #call function
    transformed_pe_data <- process_estimates_pe(estimates_pe)
    
    #Get PE and BSS dataframes to match before binding rows
    
    #table 1, stratum_catch
    transformed_pe_data$pe_stratum_catch <- transformed_pe_data$pe_stratum_catch
    
    #table 2, stratum_effort
    transformed_pe_data$pe_stratum_effort <- transformed_pe_data$pe_stratum_effort |> 
      mutate(est_cg = NA)
    
    #table 3, summarized_catch
    transformed_pe_data$pe_summarized_catch <- transformed_pe_data$pe_summarized_catch
    
    #table 4, summarized_effort
    transformed_pe_data$pe_summarized_effort <- transformed_pe_data$pe_summarized_effort |> 
      mutate(est_cg = NA)
  }

  # Process BSS estimates to common format with internal function
  if (!is.null(estimates_bss)) {
    
    #call function
    transformed_bss_data <- process_estimates_bss(estimates_bss)
    
    #Get PE and BSS dataframes to match before binding rows
    
    #table 1, stratum_catch
    transformed_bss_data$bss_stratum_catch <- transformed_bss_data$bss_stratum_catch |> 
      rename(period = "week", estimate_category = "estimate") |> 
      select(-c("month", "estimate_index", "day_index", "event_date")) |> 
      #add day_type to match PE format
      mutate(day_type = ifelse(weekdays(min_event_date) %in% params$days_wkend, "weekend", "weekday"))
    
    #table 2, stratum_effort
    transformed_bss_data$bss_stratum_effort <- transformed_bss_data$bss_stratum_effort |> 
      rename(period = "week", estimate_category = "estimate") |> 
      select(-c("month", "estimate_index", "day_index", "event_date")) |> 
      #add day_type to match PE format
      mutate(day_type = ifelse(weekdays(min_event_date) %in% params$days_wkend, "weekend", "weekday"))
    
    #table 3, summarized_catch
    transformed_bss_data$bss_summarized_catch <- transformed_bss_data$bss_summarized_catch |> 
      rename(estimate_category = "estimate") 
    
    #table 4, summarized_effort
    transformed_bss_data$bss_summarized_effort <- transformed_bss_data$bss_summarized_effort |> 
      rename(estimate_category = "estimate")
    
  }
  
  #-----------------------------------------------------------------------------------------------------------------#
  
  #Map data_grade column to every table
  data_grade_lower <- tolower(data_grade) #accept capitalization

  if (data_grade_lower == "approved") {

    transformed_bss_data <- map(transformed_bss_data,
                                ~{.$data_grade <- rep("Approved", nrow(.)); .})
    transformed_pe_data <- map(transformed_pe_data,
                               ~{.$data_grade <- rep("Approved", nrow(.)); .})

  } else if (data_grade_lower == "provisional") {

    transformed_bss_data <- map(transformed_bss_data,
                               ~{.$data_grade <- rep("Provisional", nrow(.)); .})
    transformed_pe_data <- map(transformed_pe_data,
                               ~{.$data_grade <- rep("Provisional", nrow(.)); .})

  } else {
    stop("Invalid value for data_grade. Use 'approved' or 'provisional'.")
  }
  #-----------------------------------------------------------------------------------------------------------------#
  #Combine PE and BSS standardized objects
  
  creel_estimates <- list(
    #table 1
    stratum_catch = rbind(transformed_pe_data$pe_stratum_catch, transformed_bss_data$bss_stratum_catch),
    #table 2
    stratum_effort = rbind(transformed_pe_data$pe_stratum_effort, transformed_bss_data$bss_stratum_effort),
    #table 3
    summarized_catch = rbind(transformed_pe_data$pe_summarized_catch, transformed_bss_data$bss_summarized_catch),
    #table 4
    summarized_effort = rbind(transformed_pe_data$pe_summarized_effort, transformed_bss_data$bss_summarized_effort)
  )
  
  #combine catch and effort data
  creel_estimates$stratum <- rbind(creel_estimates$stratum_catch, creel_estimates$stratum_effort)
  
  creel_estimates$total <- rbind(creel_estimates$summarized_catch, creel_estimates$summarized_effort)
  
  #bring in fishery manager table
  fishery_manager_trim <- dwg$fishery_manager |>
    filter(location_type == "Section") |> 
    select(section_num, catch_area_code, catch_area_description) |>
    arrange(section_num) |> 
    distinct() #assumes no duplicate section_num in fishery manager table
  
  #join CRC from fishery manager table to estimate tables
  creel_estimates$stratum <- left_join(creel_estimates$stratum, fishery_manager_trim, by ="section_num")
   
  # fishery_manager_total <- as.vector(fishery_manager_trim$catch_area_code)
  # fishery_manager_total <- paste(fishery_manager_total, collapse = ",")
  # #join CRCs from all sections for total strata
  # creel_estimates$total <- creel_estimates$total |> mutate(catch_area_code = fishery_manager_total)
  
  #rename value column to estimate_value
  creel_estimates$stratum <- creel_estimates$stratum |> rename(estimate_value = value)
  creel_estimates$total <- creel_estimates$total |> rename(estimate_value = value)  
  
  # change angler_final capitalization to match creel database lut
  creel_estimates$stratum <- creel_estimates$stratum |> 
    mutate(angler_final = case_when(
      angler_final == "bank" ~ "Bank",
      angler_final == "boat" ~ "Boat"
    ))
  
  # add period_timestep field to denote yaml model parameters
  creel_estimates$stratum <- creel_estimates$stratum |> 
    mutate(period_timestep = case_when(
      model_type == "PE" ~ params$period_pe,
      model_type == "BSS" ~ params$period_bss
      ))
  
  creel_estimates$total <- creel_estimates$total |> 
    mutate(period_timestep = case_when(
      model_type == "PE" ~ params$period_pe,
      model_type == "BSS" ~ params$period_bss
    ))
  
  #assign to global env
  creel_estimates <<- creel_estimates
  
  #-------------------------------------------------------------------------------------------------------------------#
  ## EXPORTING ESTIMATES ##
  
  # Connect to database and conditionally export
  if(export_data == tolower("database")) {
    
    estimate_reviewers <- c("holc2477", "bentlktb") #please don't manually modify this list :) 
    
    if (!Sys.info()["user"] %in% estimate_reviewers && params$data_grade == tolower("provisional")) {
      stop("Creel project leads may only upload estimates with a data_grade of 'provisional'.")
    }
    
    #convert metadata to json. Automatically added to analysis_lut
    json_conversion(type = "script")
    json_conversion(type = "r_session")
    
    #-----------------------------------------------------------------------------------------------------------------#
    
    #function to establish database connection
    establish_db_con<- function(max_attempts = 5, delay_seconds = 3) {
      # initialize objects
      config <- yaml::read_yaml("config.yml")
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
    
    #connect to database
    con <- establish_db_con()
    
    #-----------------------------------------------------------------------------------------------------------------#
    
    #define function to query database tables
    fetch_db_table <- function(con = NULL, schema, table) {
      
      if(!DBI::dbIsValid(con)) {
        stop("No database connection provided.")
      }
      
      table <- dplyr::tbl(con, 
                          dbplyr::in_schema(dbplyr::sql(schema), 
                                            dbplyr::sql(table))) |> 
        dplyr::collect()
      
      return(table)
    }
    
    #query database tables necessary to get UUIDS
    cat("\nFetching database uuids.")
    project_lut <- fetch_db_table(con, "creel", "project_lut") |> select(project_name, project_id)
    fishery_lut <- fetch_db_table(con, "creel", "fishery_lut") |> select(fishery_name, fishery_id)
    species_lut <- fetch_db_table(con, "creel", "species_lut") |> select(species_name, species_id)
    life_stage_lut <- fetch_db_table(con, "creel", "life_stage_lut") |> select(life_stage_name, life_stage_id)
    fin_mark_lut <- fetch_db_table(con, "creel", "fin_mark_lut") |> select(fin_mark_code, fin_mark_id)
    fate_lut <- fetch_db_table(con, "creel", "fate_lut") |> select(fate_name, fate_id)
    angler_type_lut <- fetch_db_table(con, "creel", "angler_type_lut") |> select(angler_type_code, angler_type_id)
    crc_area_lut <- fetch_db_table(con, "creel", "crc_area_lut") |> select(catch_area_code, crc_area_id)
    
    #parse out catch group column into component fields to match with database format and use of UUIDs
    
    ##total UUIDs ----------------------------------------------------------------------------------------------
    creel_estimates$total <- creel_estimates$total |>
      tidyr::separate(col = est_cg, 
                      into = c("species_name", "life_stage_name", "fin_mark_code", "fate_name"),
                      sep = "_")
      
    #join UUIDs to appropriate fields
    creel_estimates$total <- creel_estimates$total |> left_join(project_lut, by = "project_name")
    creel_estimates$total <- creel_estimates$total |> left_join(fishery_lut, by = "fishery_name")
    creel_estimates$total <- creel_estimates$total |> left_join(species_lut, by = "species_name")
    creel_estimates$total <- creel_estimates$total |> left_join(life_stage_lut, by = "life_stage_name")
    creel_estimates$total <- creel_estimates$total |> left_join(fin_mark_lut, by = "fin_mark_code")
    creel_estimates$total <- creel_estimates$total |> left_join(fate_lut, by = "fate_name")
    
    #reformat, remove common name fields in favor of UUID fields
    creel_estimates$total <- creel_estimates$total |> 
      select(-c("project_name", "fishery_name", "species_name", "life_stage_name","fin_mark_code", "fate_name")) |>
      relocate(c("project_id", "fishery_id")) |> 
      relocate(c("species_id", "life_stage_id", "fin_mark_id", "fate_id"), .after = "model_type")
    
    ##stratum UUIDS --------------------------------------------------------------------------------------------
    creel_estimates$stratum <- creel_estimates$stratum |>
      tidyr::separate(col = est_cg, 
                      into = c("species_name", "life_stage_name", "fin_mark_code", "fate_name"),
                      sep = "_")
    
    #join UUIDs to appropriate fields
    creel_estimates$stratum <- creel_estimates$stratum |> left_join(project_lut, by = "project_name")
    creel_estimates$stratum <- creel_estimates$stratum |> left_join(fishery_lut, by = "fishery_name")
    creel_estimates$stratum <- creel_estimates$stratum |> left_join(species_lut, by = "species_name")
    creel_estimates$stratum <- creel_estimates$stratum |> left_join(life_stage_lut, by = "life_stage_name")
    creel_estimates$stratum <- creel_estimates$stratum |> left_join(fin_mark_lut, by = "fin_mark_code")
    creel_estimates$stratum <- creel_estimates$stratum |> left_join(fate_lut, by = "fate_name")
    creel_estimates$stratum <- creel_estimates$stratum |> left_join(angler_type_lut, by = c("angler_final" = "angler_type_code"))
    creel_estimates$stratum <- creel_estimates$stratum |> left_join(crc_area_lut, by = "catch_area_code")
    
    #reformat, remove common name fields in favor of UUID fields
    creel_estimates$stratum <- creel_estimates$stratum |> 
      select(-c("project_name", "fishery_name","species_name", "life_stage_name","fin_mark_code", "fate_name", 
                "angler_final", "catch_area_code", "catch_area_description")) |>
      relocate(c("project_id", "fishery_id")) |> 
      relocate(c("species_id", "life_stage_id", "fin_mark_id", "fate_id", "angler_type_id"), .after = "model_type") 

    #reformat NaN estimate values in stratum scale to 0 values
    creel_estimates$stratum <- creel_estimates$stratum |>  
      mutate(estimate_value = case_when(
        is.nan(estimate_value) ~ 0,
        TRUE ~ estimate_value
      ))
    
    creel_estimates <<- creel_estimates

    #function to ask for confirmation
    # ask_for_confirmation <- function() {
    #   response <- ""
    #   while (response != "Y" && response != "N") {
    #     response <- readline("Would you like to proceed with upload? [Y/N]: ") |> toupper()
    #     if (response != "Y" && response != "N") {
    #       cat("Please enter 'Y' for Yes or 'N' for No.\n")
    #     }
    #   }
    #   return(response == "Y")
    # }
    
    
    ask_for_confirmation <- function() { #could be moved outside to use elsewhere
        response <- ""
        repeat {
          if (response != "") {
            cat("Please enter 'Y' for Yes or 'N' for No.\n")
          }
          response <- toupper(trimws(readline("Would you like to proceed with upload? [Y/N]: ")))
          if (response %in% c("Y", "N")) {
            return(response == "Y")
          }
        }
    }
    
    #define functions for writting tables
    #model_analysis_lut
    write_lut <- function() {
      dbWriteTable(
        conn = con, 
        name = Id(schema = "creel", table = "model_analysis_lut"),
        value = analysis_lut,
        row.names = FALSE,
        overwrite = FALSE,
        append = TRUE)
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
    
    ### write estimates to database ####

    #model_analysis_lut
    #determine if session analysis_id already exists in database model_analysis_lut table
    cat("\nVerifying that session 'analysis_id' does not exist in database before upload.")
    analysis_id_check <- fetch_db_table(con, "creel", "model_analysis_lut") |> select(analysis_id, analysis_name)

    if (analysis_lut$analysis_id %in% analysis_id_check$analysis_id) {
      cat("\n")
      stop("\nAnalysis uuid already exists in the creel database. Review before proceeding.")
    } else { #analysis_id not already in database
      
      #a pause, option to abort process
      proceed <- ask_for_confirmation()
  
      if (proceed) {
        cat("Continuing with upload...\n")
        Sys.sleep(3)
        
        #evaluate export_tables parameter
        if (params$export_tables == "total") {
          
          #write lut and total
          cat(paste0("Writing to model_analysis_lut table...  ","\u2713", "\n"))
          write_lut()
          
          cat(paste0("Writing to model_estimates_total table...  ","\u2713", "\n"))
          write_total()
          
        } else if (params$export_tables == "stratum") {
          
          #write lut and stratum
          cat(paste0("Writing to model_analysis_lut table...  ","\u2713", "\n"))
          write_lut()
          
          cat(paste0("Writing to model_estimates_stratum table...  ","\u2713", "\n"))
          write_stratum()
          
        } else if (params$export_tables == "both") {
          
          #write lut, total, and stratum
          cat(paste0("Writing to model_analysis_lut table...  ","\u2713", "\n"))
          write_lut()
          
          cat(paste0("Writing to model_estimates_total table...  ","\u2713", "\n"))
          write_total()
          
          cat(paste0("Writing to model_estimates_stratum table...  ","\u2713", "\n"))
          write_stratum()
          
        } else {
          cat("\nParameter export_tables must be either 'total', 'stratum', or 'both'.")
        }
        
      } else {
      # If confirmation = No
      cat("Writing to database tables aborted.\n")
      return(NULL)
      }
    }

    cat("Uploading complete. Verifying session 'analysis_id' in database analysis look up table.\n")

    #verify data has been sent to database
    # this could be made more comprehensive
    confirm_upload <- fetch_db_table(con, "creel", "model_analysis_lut") |> select(analysis_id, analysis_name)

    if (analysis_lut$analysis_id %in% confirm_upload$analysis_id) {

      DBI::dbDisconnect(con)
      cat(paste("Data sucessfully exported.", "\u2713","\n"))
      cat("Disconnecting from database.\n")
      
    } else {
      #what to do if analysis_id is not in analysis_lut (partial/failed export)
      cat("\n")
      message("Unable to confirm upload by checking database for session analysis_id.")
      
      cat("\n")
      message(paste("writing",crayon::red$bgYellow("FAILED_UPLOAD_LOG_analysis_lut.csv") , "to CreelEstimates folder so that analysis_id for partial data upload can be investigated."))
      
      readr::write_csv(analysis_lut, file = paste0("FAILED_UPLOAD_LOG_","analysis_lut.csv"), append = TRUE)
      
      DBI::dbDisconnect(con)
      stop("\nDisconnecting from database.")
    }
    
  } else if (export_data == tolower("local")) {
    #process for exporting ETL output tables locally for inspection prior to uploading to database
    
    #convert metadata to json. Automatically added to analysis_lut
    json_conversion(type = "script")
    json_conversion(type = "r_session")
    
    #project- and fishery-specific folder from CreelEstimates
    #could be more flexible and make folders where needed? for case of recreation of script on computer that did run analysis
    write_directory <- paste0(getwd(), "/fishery_analyses/", params$project_name, "/", params$fishery_name,"/")
    
    #write csv files to local working directory
    write_csv(analysis_lut, file = paste0(write_directory,"analysis_lut.csv"))
    write_csv(creel_estimates$stratum, file = paste0(write_directory,"model_estimates_stratum.csv"))
    write_csv(creel_estimates$total, file = paste0(write_directory, "model_estimates_total.csv"))  
    
    cat("\n\n")
    cat("Standardized model estimate tables and analysis_lut saved to fishery folder on local computer.")
    
  } else if (export_data == tolower("No")) {
    #send message to user that no ETL actions were taken
    cat("\n\n")
    cat("Catch and effort estimates not exported.")
    cat("\nStandardized model estimates can be viewed in output list object 'creel_estimates'.")

  } else {
    #send message to user with correct export parameter options
    cat("Export parameter must be either 'no', 'local', or 'database'.")
  }
}
