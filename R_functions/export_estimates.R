export_estimates <- function(params, estimates_pe=NULL, estimates_bss=NULL) {
  
  # Extract parameters
  data_grade <- params$data_grade
  export_data <- params$export
  
  #Look up tables (LUTs) from environment
  analysis_lut <- analysis_lut
  # estimate_extent <- estimate_extent

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
  
  #assign to global env
  creel_estimates <<- creel_estimates

  #-----------------------------------------------------------------------------------------------------------------#
  # Connect to database and conditionally export
  if(export_data == tolower("database")) {
    
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
    
    #query tables necessary to get UUIDS
    project_lut <- fetch_table(con, "creel", "project_lut") |> select(project_name, project_id)
    fishery_lut <- fetch_table(con, "creel", "fishery_lut") |> select(fishery_name, fishery_id)
    species_lut <- fetch_table(con, "creel", "species_lut") |> select(species_name, species_id)
    life_stage_lut <- fetch_table(con, "creel", "life_stage_lut") |> select(life_stage_name, life_stage_id)
    fin_mark_lut <- fetch_table(con, "creel", "fin_mark_lut") |> select(fin_mark_code, fin_mark_id)
    fate_lut <- fetch_table(con, "creel", "fate_lut") |> select(fate_name, fate_id)
    angler_type_lut <- fetch_table(con, "creel", "angler_type_lut") |> select(angler_type_code, angler_type_id)
    
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
    #reformat
    creel_estimates$stratum <- creel_estimates$stratum |> 
      select(-c("project_name", "fishery_name","species_name", "life_stage_name","fin_mark_code", "fate_name", "angler_final",
                -"catch_area_description")) |> #remove crc desc column, not needed for db table. Retained for interpretation if params$export == "local"
      relocate(c("project_id", "fishery_id")) |> 
      relocate(c("species_id", "life_stage_id", "fin_mark_id", "fate_id", "angler_type_id"), .after = "model_type")    
    
    ### write estimates to database ###
    
    #model_analysis_lut
    field_types_analysis_lut <- c(analysis_id = "uuid", analysis_name = "varchar", session_info = "varchar", estimate_json = "json")
    
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = "creel", table = "model_analysis_lut"),
      value =  analysis_lut,
      row.names = FALSE,
      overwrite = FALSE,
      append = TRUE,
      field.types = field_types_total
    )
    
    #model_estimates_total
    field_types_total <- c(analysis_id = "uuid", project_id = "uuid", fishery_id = "uuid", model_type = "varchar", 
                           species_id = "uuid", life_stage_id = "uuid", fin_mark_id = "uuid", fate_id = "uuid", 
                           estimate_type = "varchar", estimate_value = "int4", min_event_date = "date", 
                           max_event_date = "date", data_grade = "varchar", catch_area_code = "int4")
    
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = "creel", table = "model_estimates_total"),
      value = creel_estimates$total,
      row.names = FALSE,
      overwrite = FALSE,
      append = TRUE,
      field.types = field_types_total
    )
    
    #model_estimates_stratum
    field_types_stratum <- c(analysis_id = "uuid", project_id = "uuid", fishery_id = "uuid", section_num = "int4", 
                             min_event_date = "date", max_event_date = "date", period = "int4", day_type = "varchar",
                             angler_type_id = "uuid", species_id = "uuid", life_stage_id = "uuid", fin_mark_id = "uuid", 
                             fate_id = "uuid", model_type = "varchar", estimate_type = "varchar", 
                             estimate_value = "int4", data_grate = "varchar", catch_area_code = "int4")
    
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = "creel", table = "model_estimates_total"),
      value = creel_estimates$total,
      row.names = FALSE,
      overwrite = FALSE,
      append = TRUE,
      field.types = field_types_total
    )
    
   #Confirm upload
    #dbReadTable(con, "") #recent batch only? - confirm session analysis_id exists
    # print("Data sucessfully exported.")
    dbDisconnect(con)
    
    cat("Data sucessfully exported to database.")
    
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
    
  } else if (export_data == tolower("No")) {
    cat("Catch and effort estimates not exported.")

  } else {
    cat("Parameter export_data must be either 'database', 'local', or 'no'.")
  }
}
