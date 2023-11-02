######################################################################################################################################## #
### Function structure ####
######################################################################################################################################## #
#add roxygen2-style description comments

export_estimates <- function(params, estimates_pe, estimates_bss) {
  # Input validation
  if(!is.list(estimates_pe & estimates_bss)) {
    stop("Inputs 'estimates_pe' and 'estimates_bss' must be a list.")
  }
  # ------------------------------------------------------------------------#
  # Extract parameters
  model_type <- params$model_type
  data_grade <- params$data_grade
  export_data <- params$export_data
  
  # Initialize final output
  creel_estimates <- as.list(NULL)
  
  # Process estimates to common format
  if(model_type == "PE") {
    
    ####################################################################################################### #
    # PE wrangling ####
    ####################################################################################################### #

    # Pre-processing 
  
    # Initialize intermediate object for PE processing steps
    # Place to store tables used to produce formatted output
    transformed_pe_data <- as.list(NULL)
    
    # Incorporate model_type-specific outputs
    #PE
    transformed_pe_data$pe_effort <- estimates_pe$effort
    transformed_pe_data$pe_catch <- estimates_pe$catch
    
    # Add UUID and model_type columns
    #PE effort
    transformed_pe_data$pe_effort <- transformed_pe_data$pe_effort %>%
      mutate(analysis_id = analysis_id,
             model_type = params$model_type) %>%
      relocate(analysis_id)
    
    #PE catch
    transformed_pe_data$pe_catch <- transformed_pe_data$pe_catch %>%
      mutate(analysis_id = analysis_id,
             model_type = params$model_type) %>%
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
    
  } else if(model_type == "BSS") {
    
    ####################################################################################################### #
    # BSS wrangling ####
    ####################################################################################################### #
    
    transformed_bss_data <- as.list(NULL)
    
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
    min_event_date <- ""
    max_event_date <- ""
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
      relocate(analysis_id,
               project_name,
               fishery_name,
               min_event_date,
               max_event_date,
               model_type,
               est_cg)
    
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
    
 ##identify differences between column names and consolidate
    colnames(transformed_bss_data$bss_stratum_catch)
    colnames(transformed_pe_data$pe_stratum_catch)
    colnames(transformed_bss_data$bss_stratum_effort)
    colnames(transformed_pe_data$pe_stratum_effort)
    
    colnames(transformed_bss_data$bss_summarized_catch)
    colnames(transformed_pe_data$pe_summarized_catch)
    colnames(transformed_bss_data$bss_summarized_effort)  
    colnames(transformed_pe_data$pe_summarized_effort)   
  } else {
    stop("Invalid 'model_type'. Must be either 'PE' or 'BSS'. ")
  }
  
  # ------------------------------------------------------------------------#
  # Add data_grade column
  data_grade_lower <- tolower(params$data_grade) #accept capitalization
  
  if (data_grade_lower == "approved") {
    transformed_data$data_grade <- "Approved"
    transformed_bss_data$data_grade <-"Approved"
    
  } else if (data_grade_lower == "provisional") {
    transformed_data$data_grade <- "Provisional"
    transformed_bss_data$data_grade <-"Provisional"
    
  } else {
    stop("Invalid value for data_grade. Use 'approved' or 'provisional'.")
  }
  
  # ------------------------------------------------------------------------#
  # Connect to database and conditionally export
  if(export) {
    
    # FOR DEMO WRITE TO CSV FILES
    # write.csv(stratum_catch)
    # write.csv(stratum_effort)
    # write.csv(summarized_catch)
    # write.csv(summarized_effort)
    
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
    
    
  } else {
    warning("Data not exported to database.")
  }
}
