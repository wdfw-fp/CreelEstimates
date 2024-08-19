transform_estimates <- function(dwg, transformed_pe_data, transformed_bss_data) {
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
  dwg$fishery_manager$catch_area_code <- as.character(dwg$fishery_manager$catch_area_code)
  
  fishery_manager_trim <- dwg$fishery_manager |>
    dplyr::filter(location_type == "Section") |> 
    dplyr::select(section_num, catch_area_code, catch_area_description) |>
    dplyr::arrange(section_num) |> 
    dplyr::distinct() #assumes no duplicate section_num in fishery manager table
  
  #join CRC from fishery manager table to estimate tables
  creel_estimates$stratum <- dplyr::left_join(creel_estimates$stratum, fishery_manager_trim, by ="section_num")
  
  # fishery_manager_total <- as.vector(fishery_manager_trim$catch_area_code)
  # fishery_manager_total <- paste(fishery_manager_total, collapse = ",")
  # #join CRCs from all sections for total strata
  # creel_estimates$total <- creel_estimates$total |> mutate(catch_area_code = fishery_manager_total)
  
  #rename value column to estimate_value
  creel_estimates$stratum <- creel_estimates$stratum |> dplyr::rename(estimate_value = value)
  creel_estimates$total <- creel_estimates$total |> dplyr::rename(estimate_value = value)  
  
  # change angler_final capitalization to match creel database lut
  creel_estimates$stratum <- creel_estimates$stratum |> 
    dplyr::mutate(angler_final = dplyr::case_when(
      angler_final == "bank" ~ "Bank",
      angler_final == "boat" ~ "Boat"
    ))
  
  # add period_timestep field to denote yaml model parameters
  creel_estimates$stratum <- creel_estimates$stratum |> 
    dplyr::mutate(period_timestep = dplyr::case_when(
      model_type == "PE" ~ params$period_pe,
      model_type == "BSS" ~ params$period_bss
    ))
  
  creel_estimates$total <- creel_estimates$total |> 
    dplyr::mutate(period_timestep = dplyr::case_when(
      model_type == "PE" ~ params$period_pe,
      model_type == "BSS" ~ params$period_bss
    ))
  
  cat("\nTransformed output object 'creel_estimates' created.")
  
  return(creel_estimates)

}