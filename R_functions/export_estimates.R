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
    transformed_pe_data$pe_stratum_effort <- transformed_pe_data$pe_stratum_effort %>% 
      mutate(est_cg = NA)
    
    #table 3, summarized_catch
    transformed_pe_data$pe_summarized_catch <- transformed_pe_data$pe_summarized_catch
    
    #table 4, summarized_effort
    transformed_pe_data$pe_summarized_effort <- transformed_pe_data$pe_summarized_effort %>% 
      mutate(est_cg = NA)
  }

  # Process BSS estimates to common format with internal function
  if (!is.null(estimates_bss)) {
    
    #call function
    transformed_bss_data <- process_estimates_bss(estimates_bss)
    
    #Get PE and BSS dataframes to match before binding rows
    
    #table 1, stratum_catch
    transformed_bss_data$bss_stratum_catch <- transformed_bss_data$bss_stratum_catch %>% 
      rename(period = "week", estimate_category = "estimate") %>% 
      select(-c("month", "estimate_index", "day_index", "event_date")) %>% 
      mutate(day_type = NA)
    
    #table 2, stratum_effort
    transformed_bss_data$bss_stratum_effort <- transformed_bss_data$bss_stratum_effort %>% 
      rename(period = "week", estimate_category = "estimate") %>% 
      select(-c("month", "estimate_index", "day_index", "event_date")) %>% 
      mutate(day_type = NA)
    
    #table 3, summarized_catch
    transformed_bss_data$bss_summarized_catch <- transformed_bss_data$bss_summarized_catch %>% 
      rename(estimate_category = "estimate") 
    
    #table 4, summarized_effort
    transformed_bss_data$bss_summarized_effort <- transformed_bss_data$bss_summarized_effort %>% 
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
  #Combine PE and BSS before exporting
  
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
  
  #assign to global env
  creel_estimates <<- creel_estimates

  #-----------------------------------------------------------------------------------------------------------------#
  # Connect to database and conditionally export
  if(export_data == tolower("database")) {
    
    estimate_reviewers <- c("holc2477") #please don't manually modify this list :) 
    
    if (!Sys.info()["user"] %in% estimate_reviewers && params$data_grade == tolower("provisional")) {
      stop("Creel project leads may only upload estimates with a data_grade of 'provisional'.")
    }
    
    #Convert analysis script to JSON string which is added as a column to analysis_lut
    JSON_conversion(params, direction = "toJSON")
    
    #-----------------------------------------------------------------------------------------------------------------#
    
    #function to establish database connection with retry and delay
    establish_db_con<- function(dsn, max_attempts = 5, delay_seconds = 3) {
      attempt <- 1
      con <- NULL
      
      while (attempt <= max_attempts && is.null(con)) {
        # Attempt to establish connection
        con <- tryCatch({
          DBI::dbConnect(odbc::odbc(), dsn = dsn)
        }, error = function(e) {
          # Print error message
          message(paste("Attempt", attempt, "failed:", conditionMessage(e)))
          # Increment attempt count
          attempt <<- attempt + 1
          # Pause for delay before next attempt
          Sys.sleep(delay_seconds)
          # Return NULL to retry
          NULL
        })
      }
      
      #check if connection was established
      if (!DBI::dbIsValid(con)) {
        stop("Failed to establish connection after ", max_attempts, " attempts.")
      } else {
        message("Connection sucessfully established.")
      }
      
      #show con in RStudio Connections pane
      odbc:::on_connection_opened(con, 
                                  paste(
                                    c(paste("con <-", gsub(", ", ",\n\t", c(match.call()))
                                            )
                                      ), collapse = "\n"
                                    )
                                  )
      
      return(con)
    }
    
    #connect to database
    con <- establish_db_con(dsn = "creel_estimates_test")
    
    #-----------------------------------------------------------------------------------------------------------------#
    
    #define function to fetch tables from database
    fetch_table <- function(con = NULL, schema, table) {
      
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
    project_lut <- fetch_table(con, "creel", "project_lut") |> select(project_name, project_id)
    fishery_lut <- fetch_table(con, "creel", "fishery_lut") |> select(fishery_name, fishery_id)
    species_lut <- fetch_table(con, "creel", "species_lut") |> select(species_name, species_id)
    life_stage_lut <- fetch_table(con, "creel", "life_stage_lut") |> select(life_stage_name, life_stage_id)
    fin_mark_lut <- fetch_table(con, "creel", "fin_mark_lut") |> select(fin_mark_code, fin_mark_id)
    fate_lut <- fetch_table(con, "creel", "fate_lut") |> select(fate_name, fate_id)
    angler_type_lut <- fetch_table(con, "creel", "angler_type_lut") |> select(angler_type_code, angler_type_id)
    crc_area_lut <- fetch_table(con, "creel", "crc_area_lut") |> select(catch_area_code, crc_area_id)
    
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
    #reformat
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
    
    ### write estimates to database ####
    
    #model_analysis_lut
    #determine if session analysis_id already exists in database model_analysis_lut table
    analysis_id_check <- fetch_table(con, "creel", "model_analysis_lut") |> select("analysis_id", "analysis_name")
    
    if (analysis_lut$analysis_id %in% analysis_id_check$analysis_id) {
      stop("Analysis uuid already exists in the creel database. Review before proceeding.")
    } else { 
      
      #if session analysis uuid does not already exist, write to database analysis lut
      DBI::dbWriteTable(
        conn = con,
        name = DBI::Id(schema = "creel", table = "model_analysis_lut"),
        value =  analysis_lut,
        row.names = FALSE,
        overwrite = FALSE,
        append = TRUE
      )
    
      #model_estimates_total
      DBI::dbWriteTable(
        conn = con,
        name = DBI::Id(schema = "creel", table = "model_estimates_total"),
        value = creel_estimates$total,
        row.names = FALSE,
        overwrite = FALSE,
        append = TRUE
      )
    
      #model_estimates_stratum
      DBI::dbWriteTable(
        conn = con,
        name = DBI::Id(schema = "creel", table = "model_estimates_stratum"),
        value = creel_estimates$stratum,
        row.names = FALSE,
        overwrite = FALSE,
        append = TRUE
      )
    }
    
    #verify data has been sent to database
    confirm_upload <- fetch_table(con, "creel", "model_analysis_lut") |> 
      filter(analysis_id %in% analysis_lut$analysis_id)
    
    if (nrow(confirm_upload) == 1) {
      dbDisconnect(con)
      cat("Data sucessfully exported to database. Disconnected from database.")
    } else {
      stop("Unable to confirm upload by checking database for session analysis_id.")
    }
    
  } else if (export_data == tolower("local")) {
    
    #Convert analysis script to JSON string which is added as a column to analysis_lut
    JSON_conversion(params, direction = "toJSON")
    
    #project- and fishery-specific folder from CreelEstimates
    #could be more flexible and made folders where needed? for case of recreation of script on computer that did run anaylsis
    write_directory <- paste0(getwd(), "/fishery_analyses/", params$project_name, "/", params$fishery_name,"/")
    
    #write csv files to local working directory
    write_csv(analysis_lut, file = paste0(write_directory,"analysis_lut.csv"))
    write_csv(creel_estimates$stratum, file = paste0(write_directory,"model_estimates_stratum.csv"))
    write_csv(creel_estimates$total, file = paste0(write_directory, "model_estimates_total.csv"))  
    
    cat("\nModel estimates and analysis_lut saved to local computer.")
    
  } else if (export_data == tolower("No")) {
    cat("Catch and effort estimates not exported.")

  } else {
    cat("Export parameter must be either 'database', 'local', or 'no'.")
  }
}
