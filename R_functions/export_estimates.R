######################################################################################################################################## #
### Function structure ####
######################################################################################################################################## #
#add roxygen2-style description comments

export_estimates <- function(params, estimates_pe, estimates_bss) {
  #Initialize objects for storing processed data
  transformed_pe_data <- as.list(NULL)
  transformed_bss_data <- as.list(NULL)
  
  #Internal function to process estimates_pe if supplied
  process_pe <- function(estimates_pe) {
    ####################################################################################################### #
    # PE wrangling ####
    ####################################################################################################### #
    
    # Incorporate model_type-specific outputs
    #PE
    transformed_pe_data$pe_effort <- estimates_pe$effort
    transformed_pe_data$pe_catch <- estimates_pe$catch
    
    # Add UUID and model_type columns
    #PE effort
    transformed_pe_data$pe_effort <- transformed_pe_data$pe_effort %>%
      mutate(analysis_id = analysis_id,
             model_type = "PE") %>%
      relocate(analysis_id)
    
    #PE catch
    transformed_pe_data$pe_catch <- transformed_pe_data$pe_catch %>%
      mutate(analysis_id = analysis_id,
             model_type = "PE") %>%
      relocate(analysis_id)
    ############################ Stratified table ##################################
    
    #create tables for stratified table intermediates
    transformed_pe_data$pe_stratum_effort <- transformed_pe_data$pe_effort
    transformed_pe_data$pe_stratum_catch <- transformed_pe_data$pe_catch
    
    # Effort transformation
    transformed_pe_data$pe_stratum_effort <- transformed_pe_data$pe_stratum_effort %>%
      select(-c(var, l95,u95)) %>%
      pivot_longer(cols = c(n_obs:est),
                   names_to = "estimate_type",
                   values_to = "value")
    
    #tidy output
    transformed_pe_data$pe_stratum_effort <- transformed_pe_data$pe_stratum_effort %>%
      drop_na() %>%
      ungroup() %>%
      mutate(estimate_category = "effort") %>%
      relocate("estimate_category", .after = "model_type")
    
    # Catch transformation
    transformed_pe_data$pe_stratum_catch <- transformed_pe_data$pe_stratum_catch %>%
      select(-c(var,l95,u95)) %>%
      pivot_longer(cols = c(n_obs:est),
                   names_to = "estimate_type",
                   values_to = "value")
    
    #tidy output
    transformed_pe_data$pe_stratum_catch <- transformed_pe_data$pe_stratum_catch %>%
      drop_na() %>%
      ungroup() %>%
      mutate(estimate_category = "catch") %>%
      relocate("estimate_category", .after = "model_type")
    
    #Join catch and effort intermediate tables
    # creel_estimates$pe_strat <- bind_rows(transformed_pe_data$pe_stratum_catch, transformed_pe_data$pe_stratum_effort)
    
    ############################# Rolled up table ##################################
    
    #create tables for rolled up table intermediates
    transformed_pe_data$pe_summarized_effort <- transformed_pe_data$pe_effort
    transformed_pe_data$pe_summarized_catch <- transformed_pe_data$pe_catch
    
    # Effort transformation
    transformed_pe_data$pe_summarized_effort <- transformed_pe_data$pe_summarized_effort %>%
      group_by(analysis_id, project_name, fishery_name, min_event_date,
               max_event_date, model_type) %>%
      summarise(est_sum = sum(est)
      ) %>%
      pivot_longer(cols = c(est_sum),
                   names_to = "estimate_type",
                   values_to = "value")
    
    #tidy output
    transformed_pe_data$pe_summarized_effort <- transformed_pe_data$pe_summarized_effort %>%
      drop_na() %>%
      ungroup() %>%
      mutate(estimate_category = "effort") %>%
      relocate("estimate_category", .after = "model_type")
    
    # Catch transformation
    transformed_pe_data$pe_summarized_catch <- transformed_pe_data$pe_summarized_catch %>%
      group_by(analysis_id, project_name, fishery_name, min_event_date,
               max_event_date, model_type, est_cg) %>% #includes catch group col
      summarise(n_obs_sum = sum(n_obs),
                N_days_open_sum = sum(N_days_open),
                est_sum = sum(est)
      ) %>%
      pivot_longer(cols = c(n_obs_sum:est_sum),
                   names_to = "estimate_type",
                   values_to = "value")
    
    #tidy output
    transformed_pe_data$pe_summarized_catch <- transformed_pe_data$pe_summarized_catch %>%
      drop_na() %>%
      ungroup() %>%
      mutate(estimate_category = "catch") %>%
      relocate("estimate_category", .after = "model_type")
    
    # Join catch and effort intermediate tables
    # creel_estimates$pe_total <- bind_rows(transformed_pe_data$pe_summarized_catch, transformed_pe_data$pe_summarized_effort)
    # assign("transformed_pe_data", transformed_pe_data, envir = .GlobalEnv)
    return(transformed_pe_data)
  }
  
  #Internal function to process estimates_bss if supplied  
  process_bss <- function(estimates_bss) {
    ####################################################################################################### #
    # BSS wrangling ####
    ####################################################################################################### #
    
    ############################ Stratified table ##################################
    # 1 - process the daily, strata-specific BSS estimates
    # convert the list of lists into a single dataframe in a for loop
    all_frames <- list()
    catch_groups <- names(estimates_bss)
    
    for(i in catch_groups){
      sub_list <- estimates_bss[[i]]
      table_bind <- bind_rows(sub_list[2:4])
      all_frames[[i]] <- table_bind
    }
    
    all_data <- do.call(rbind, all_frames)
    
    #Extract fishery-specific creel metadata
    analysis_id <- analysis_id
    project_name <- params$project_name
    fishery_name <- params$fishery_name
    min_event_date <- params$est_date_start
    max_event_date <- params$est_date_end
    model_type <- "BSS"
    
    # perform the data wrangling 
    transformed_bss_data$bss_stratum <- all_data |> 
      pivot_longer(
        cols = c(mean:Rhat), 
        names_to = "estimate_type", values_to = "value") %>% 
      mutate(
        analysis_id = analysis_id,
        project_name = project_name,
        fishery_name = fishery_name,
        min_event_date = min_event_date,
        max_event_date = max_event_date,
        model_type = model_type,
      ) %>% 
      relocate(analysis_id, project_name, fishery_name, min_event_date,
               max_event_date, model_type, est_cg)
    
    ############################# Rolled up table ##################################    
    # 2 - process the "overview" results from the BSS
    # convert the list of lists into a single dataframe in a for loop
    for (i in catch_groups){
      sub_list <- estimates_bss[[i]]
      table_bind <- bind_rows(sub_list[1])
      all_frames[[i]] <- table_bind
    }
    
    all_data <- do.call(rbind, all_frames)
    
    transformed_bss_data$bss_summarized <- all_data |> 
      pivot_longer(
        cols = c(mean:n_div), 
        names_to = "estimate_type", values_to = "value") %>% 
      mutate(
        analysis_id = analysis_id,
        project_name = project_name,
        fishery_name = fishery_name,
        min_event_date = min_event_date,
        max_event_date = max_event_date,
        model_type = model_type,
      ) %>% 
      relocate(analysis_id, project_name, fishery_name, min_event_date,
               max_event_date, model_type, est_cg)
    
    #divide bss into catch and effort tables to match PE
    transformed_bss_data$bss_stratum_catch <- transformed_bss_data$bss_stratum %>% 
      filter(estimate %in% c("C_daily","CPUE_daily"))
    
    transformed_bss_data$bss_stratum_effort <- transformed_bss_data$bss_stratum %>% 
      filter(estimate %in% "E_daily")   
    
    
    transformed_bss_data$bss_summarized_catch <- transformed_bss_data$bss_summarized %>% 
      filter(estimate %in% "C_sum")
    
    transformed_bss_data$bss_summarized_effort <- transformed_bss_data$bss_summarized %>% 
      filter(estimate %in% "E_sum")
    
    # assign("transformed_bss_data", transformed_bss_data, envir = .GlobalEnv)
    return(transformed_bss_data)
  }
    
  # Process PE estimates to common format with internal function
  if (!is.null(estimates_pe)) {
    
    transformed_pe_data <- process_pe(estimates_pe)
  } 

  # Process BSS estimates to common format with internal function
  if (!is.null(estimates_bss)) {
    
    transformed_bss_data <- process_bss(estimates_bss)
  } 
  
  #Error handling
  if (is.null(estimates_pe) && is.null(estimates_bss)) {
    stop("At least one of 'estimates_pe' or 'estimates_bss' must be supplied.")
  }
  
  # ------------------------------------------------------------------------#
  # Extract parameters
  data_grade <- params$data_grade
  export_data <- params$export
  
  # Initialize final output
  creel_estimates <- as.list(NULL)
  
  #Map data_grade column to every table
  data_grade_lower <- tolower(params$data_grade) #accept capitalization
  
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
  # ------------------------------------------------------------------------#
  #Combine PE and BSS before exporting
  ##Get data frames to match and bind rows to creel_estimates
  
  #table 1, stratum_catch
  transformed_bss_data$bss_stratum_catch <- transformed_bss_data$bss_stratum_catch %>% 
    rename(period = "week", estimate_category = "estimate") %>% 
    select(-c("month", "estimate_index", "day_index")) %>% 
    mutate(day_type = "",
           event_date = as.Date(event_date)) #matching date format pre-bind

  
  transformed_pe_data$pe_stratum_catch <- transformed_pe_data$pe_stratum_catch %>% 
    mutate(event_date = as.Date(NA))
  
  ##add to results
  creel_estimates$stratum_catch <- rbind(transformed_pe_data$pe_stratum_catch,
                                         transformed_bss_data$bss_stratum_catch)
  
  #table 2, stratum_effort
  transformed_bss_data$bss_stratum_effort <- transformed_bss_data$bss_stratum_effort %>% 
    rename(period = "week", estimate_category = "estimate") %>% 
    select(-c("month", "estimate_index", "day_index")) %>% 
    mutate(day_type = "",
           event_date = as.Date(event_date)) #matching date format pre-bind
  
  transformed_pe_data$pe_stratum_effort <- transformed_pe_data$pe_stratum_effort %>% 
    mutate(est_cg = NA,
           event_date = as.Date(NA))
  
  ##add to results
  creel_estimates$stratum_effort <- rbind(transformed_pe_data$pe_stratum_effort,
                                          transformed_bss_data$bss_stratum_effort)
  
  #table 3, summarized_catch
  transformed_bss_data$bss_summarized_catch <- transformed_bss_data$bss_summarized_catch %>% 
    rename(estimate_category = "estimate") 
  
  transformed_pe_data$pe_summarized_catch <- transformed_pe_data$pe_summarized_catch
  
  ##add to results
  creel_estimates$summarized_catch <- rbind(transformed_pe_data$pe_summarized_catch,
                                            transformed_bss_data$bss_summarized_catch)
  
  #table 4, summarized_effort
  transformed_bss_data$bss_summarized_effort <- transformed_bss_data$bss_summarized_effort %>% 
    rename(estimate_category = "estimate") 
    
  transformed_pe_data$pe_summarized_effort <- transformed_pe_data$pe_summarized_effort %>% 
    mutate(est_cg = NA)
  
  ##add to results
  creel_estimates$summarized_effort <- rbind(transformed_pe_data$pe_summarized_effort,
                                             transformed_bss_data$bss_summarized_effort)

  # ------------------------------------------------------------------------#
  # Connect to database and conditionally export
  if(export_data) {
    
    # FOR DEMO WRITE TO CSV FILES
    write.csv(creel_estimates$stratum_catch, file = paste0(params$fishery_name, " creel estimates_stratum catch.csv"), row.names = F)
    write.csv(creel_estimates$stratum_effort, file = paste0(params$fishery_name, " creel estimates_stratum effort.csv"), row.names = F)
    write.csv(creel_estimates$summarized_catch, file = paste0(params$fishery_name, " creel estimates_summarized catch.csv"), row.names = F)
    write.csv(creel_estimates$summarized_catch, file = paste0(params$fishery_name, " creel estimates_summarized catch.csv"), row.names = F)
    
    write.csv(analysis_lut, file = "creel analysis_lut.csv", row.names = F)
    
    # #Establish connection
    # con <- DBI::dbConnect(RPostgres::Postgres(),
    #                       dbname = "",
    #                       host = "",
    #                       port = 1234,
    #                       user = "",
    #                       #interactively ask user for password
    #                       password = rstudioapi::askForPassword("Database password"))
    # 
    # #View tables and fields?
    # #dbListTables(con)
    # #dbListFields(con, "")
    # #dbExistsTable returns logical
    # 
    # #Write data to database, https://rpostgres.r-dbi.org/reference/postgres-tables.html
    # RPostgres::dbWritetable(
    #   conn = con,
    #   name = "",
    #   value = "",
    #   row.names = FALSE,
    #   overwrite = FALSE,
    #   append = TRUE,
    #   #if append = TRUE, field types can be defined, or be interpreted automatically from db via DBI::dbDataType()
    #   field.types = ""
    # )
    # 
    # #Write data to analysis_lut
    # RPostgres::dbWriteTable(
    #   conn = con,
    #   name = "",
    #   value = "",
    #   row.names = FALSE,
    #   overwrite = FALSE,
    #   append = TRUE,
    #   #if append = TRUE, field types can be defined, or be interpreted automatically from db via DBI::dbDataType()
    #   field.types = ""    
    # )
    # 
    # #Confirm upload?
    # #dbReadTable(con, "") #recent batch only?
    # print("Data sucessfully exported.")
    # 
    # #Disconnect from database
    # dbDisconnect(con)
    
    cat("Data sucessfully exported.")
  } else {
    cat("Data not exported to database.")
  }
}
