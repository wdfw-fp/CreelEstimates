process_estimates_pe <- function(estimates_pe) {
  
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
  
  
  ############################# Rolled up table ##################################
  
  #create tables for rolled up table intermediates
  transformed_pe_data$pe_summarized_effort <- transformed_pe_data$pe_effort
  transformed_pe_data$pe_summarized_catch <- transformed_pe_data$pe_catch
  
  # Effort transformation
  transformed_pe_data$pe_summarized_effort <- transformed_pe_data$pe_summarized_effort %>%
    group_by(analysis_id, project_name, fishery_name, model_type) %>%
    summarise(est_sum = sum(est)
    ) %>%
    pivot_longer(cols = c(est_sum),
                 names_to = "estimate_type",
                 values_to = "value") %>% 
    #set min date as start of monitoring period
    mutate(min_event_date = as.Date(params$est_date_start),
    #set max_event_date as sys.date if in-season, set as est_end_date if out of monitoring period 
           max_event_date = as.Date(
             ifelse(
               Sys.Date() <= params$est_date_end, Sys.Date(),
               params$est_date_end
               ))
    )
  
  #tidy output
  transformed_pe_data$pe_summarized_effort <- transformed_pe_data$pe_summarized_effort %>%
    drop_na() %>%
    ungroup() %>%
    mutate(estimate_category = "effort") %>%
    relocate("estimate_category", .after = "model_type")
  
  # Catch transformation
  transformed_pe_data$pe_summarized_catch <- transformed_pe_data$pe_summarized_catch %>%
    group_by(analysis_id, project_name, fishery_name, model_type, est_cg) %>% #includes catch group col
    summarise(n_obs_sum = sum(n_obs),
              N_days_open_sum = sum(N_days_open),
              est_sum = sum(est)
    ) %>%
    pivot_longer(cols = c(n_obs_sum:est_sum),
                 names_to = "estimate_type",
                 values_to = "value") %>% 
    #set min date as start of monitoring period
    mutate(min_event_date = as.Date(params$est_date_start),
    #set max_event_date as sys.date if in-season, set as est_end_date if out of monitoring period 
           max_event_date = as.Date(
             ifelse(
               Sys.Date() <= params$est_date_end, Sys.Date(),
               params$est_date_end
             ))
    )
  
  #tidy output
  transformed_pe_data$pe_summarized_catch <- transformed_pe_data$pe_summarized_catch %>%
    drop_na() %>%
    ungroup() %>%
    mutate(estimate_category = "catch") %>%
    relocate("estimate_category", .after = "model_type")
  
  return(transformed_pe_data)
}