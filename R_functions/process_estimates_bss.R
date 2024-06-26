process_estimates_bss <- function(params, analysis_lut, estimates_bss) {
  
  transformed_bss_data <- list(
    bss_stratum = data.frame(),
    bss_summarized = data.frame(),
    bss_stratum_catch = data.frame(),
    bss_stratum_effort = data.frame(),
    bss_summarized_catch = data.frame(),
    bss_summarized_effort = data.frame()
  )
  ############################ Stratified table ##################################
  # 1 - process the daily, strata-specific BSS estimates
  # convert the list of lists into a single dataframe in a for loop
  all_frames <- list()
  catch_groups <- names(estimates_bss)
  
  for(i in catch_groups){
    sub_list <- estimates_bss[[i]]
    table_bind <- dplyr::bind_rows(sub_list[2:4]) #cpue_daily, catch_daily, effort_daily
    all_frames[[i]] <- table_bind
  }
  
  all_data <- do.call(rbind, all_frames)
  
  #prep for database export by removing '%' symbols from BSS outputs
  all_data <- all_data |> 
    rename(
      "2.5_pct" = `2.5%`,
      "25_pct" = `25%`,
      "50_pct" = `50%`,
      "75_pct" = `75%`,
      "97.5_pct" = `97.5%`
      )
  
  #Extract fishery-specific creel metadata
  analysis_id <- analysis_lut$analysis_id
  project_name <- params$project_name
  fishery_name <- params$fishery_name
  model_type <- "BSS"
  
  # perform the data wrangling 
  transformed_bss_data$bss_stratum <- all_data |> 
    pivot_longer(
      cols = c(mean:Rhat), 
      names_to = "estimate_type", values_to = "value")  |>  
    mutate(
      analysis_id = analysis_id,
      project_name = project_name,
      fishery_name = fishery_name,
      min_event_date = event_date, #Apply BSS daily event date to min and max dates
      max_event_date = event_date,
      model_type = model_type,
    )  |>  
    relocate(analysis_id, project_name, fishery_name, min_event_date,
             max_event_date, model_type, est_cg)
  
  ############################# Rolled up table ##################################    
  # 2 - process the "overview" results from the BSS
  # convert the list of lists into a single dataframe in a for loop
  for (i in catch_groups){
    sub_list <- estimates_bss[[i]]
    table_bind <- bind_rows(sub_list[1]) #overview
    all_frames[[i]] <- table_bind
  }
  
  # !!! build in error statement for catch/effort estimate column having NA values before pivoting.
  all_data_summ <- do.call(rbind, all_frames) 
  
  #prep for database export by removing '%' symbols from BSS outputs
  all_data_summ <- all_data_summ |> 
    rename(
      "2.5_pct" = `2.5%`,
      "25_pct" = `25%`,
      "50_pct" = `50%`,
      "75_pct" = `75%`,
      "97.5_pct" = `97.5%`
    )
  
  #transform into standardized table format
  transformed_bss_data$bss_summarized <- all_data_summ |> 
    pivot_longer(
      cols = c(mean:n_div), 
      names_to = "estimate_type", values_to = "value")  |>  
    mutate(
      analysis_id = analysis_id,
      project_name = project_name,
      fishery_name = fishery_name,
      min_event_date = min(all_data$event_date), #Apply BSS daily min value as start date
      max_event_date = max(all_data$event_date), #Apply Bss daily max value as end date
      model_type = model_type,
    )  |>  
    relocate(analysis_id, project_name, fishery_name, min_event_date,
             max_event_date, model_type, est_cg)
  
  #divide bss into catch and effort tables to match PE
  transformed_bss_data$bss_stratum_catch <- transformed_bss_data$bss_stratum  |>  
    filter(estimate %in% c("C_daily","CPUE_daily"))
  
  transformed_bss_data$bss_stratum_effort <- transformed_bss_data$bss_stratum  |>  
    filter(estimate %in% "E_daily")   
  
  
  transformed_bss_data$bss_summarized_catch <- transformed_bss_data$bss_summarized  |>  
    filter(estimate %in% "C_sum")
  
  transformed_bss_data$bss_summarized_effort <- transformed_bss_data$bss_summarized  |>  
    filter(estimate %in% "E_sum")
  
  # assign("transformed_bss_data", transformed_bss_data, envir = .GlobalEnv)
  
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
  
  cat("\nBSS standardization transformation complete.")
  
  return(transformed_bss_data)
}