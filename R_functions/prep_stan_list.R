#could pass single dwg_summ list rather than separate interview & effort_index & effort_census objects
#pretty simple to revise if desired...
#similarly, could pass list of priors while including current values as default argument value
prep_stan_list <- function(
    period,
    days,
    interview_cg, #if passing single dwg_summ list, would also want to pass est_catch_group to declare filtered intermediate
    effort_index,
    effort_census,
    tie_in_mat,
    ...){
  
  effort_index_vehicle <- effort_index |> filter(str_detect(count_type, "ehicle"))
  effort_index_trailer <- effort_index |> filter(str_detect(count_type, "railer"))
  effort_index_angler <- effort_index |> filter(str_detect(count_type, "ngler"))
  
  interview_cg_daily_summ <- interview_cg |> 
    group_by(event_date, section_num, angler_final_int) |> 
    summarise(across(c(fishing_time_total, fish_count), sum), .groups = "drop")
  
  stan_list <- list(
    D = nrow(days), # int; number of fishing days
    G = length(unique(interview_cg$angler_final_int)),  # int; final number of unique gear/angler types 
    S = as.integer(length(unique(effort_census$section_num))),  # int; final number of river sections 
    H = max(effort_index$count_sequence), # int; max number of index counts within a sample day
    
    P_n = case_when( #int; total number of periods
      tolower(period) == 'day' ~ max(days$day_index),
      tolower(period) == 'week' ~ max(days$week_index),
      tolower(period) == 'month' ~ max(days$month_index),
      tolower(period) == 'duration' ~ as.integer(1)
    ),
    period = case_when( # int vec; index denoting fishing day/period
      tolower(period) == 'day' ~ days$day_index,
      tolower(period) == 'week' ~ days$week_index,
      tolower(period) == 'month' ~ days$month_index,
      tolower(period) == 'duration' ~ rep(as.integer(1), nrow(days))
    ),
        
    w = days$DayType_num, # int vec; 0/1 denoting Weekday/end for model offset
    L = days$DayL,  # num vec, daylength (model offset; assumption)
    # num mat; open/closed by section; 0 defined as 1E-6 for model
    O = days |> 
      select(contains("section_")) |>
      mutate(across(everything(), ~if_else(., 1, 0.00001))) |> 
      as.matrix(),
    
    # Vehicle index effort counts 
    V_n = nrow(effort_index_vehicle), # int; total number of individual vehicle index effort counts 
    V_I = effort_index_vehicle$count_index, # num vec; observed # of vehicles 
    day_V = left_join(effort_index_vehicle, days, by = "event_date") |> pull(day_index),   # int; index for day/period 
    gear_V = as.integer(rep(1, nrow(effort_index_vehicle))), #update naming...
    section_V = as.integer(effort_index_vehicle$section_num), # int; index for section
    countnum_V = as.integer(effort_index_vehicle$count_sequence),     # int; index for count_sequence  

    # Trailer index effort counts
    T_n = nrow(effort_index_trailer), # int; total number of boat trailer index effort counts 
    T_I = effort_index_trailer$count_index, # num vec; observed # of boat trailers 
    day_T = left_join(effort_index_trailer, days, by = "event_date") |> pull(day_index), # int; index for day/period
    gear_T = as.integer(rep(2, nrow(effort_index_vehicle))), 
    section_T = as.integer(effort_index_trailer$section_num), # int vec; index for section
    countnum_T = as.integer(effort_index_trailer$count_sequence), # int vec; index for count_sequence  
    
    #10/12/22 DA temp in-function intermediate resolves to previous "all 0s" (including classes) when no "Angler" in passed count_types
    #have not yet tested on dataset with mixed vehicle+trailer+angler, but suggests that any filtered intermediates with 0 rows will behave as desired?
    #old comment: Need to circle back on how to deal with this when applicable, set all values to 0 as a placeholder, matching example standat_2021-05-28.txt
    # Angler index effort counts
    A_n = nrow(effort_index_angler), # int; total number of angler index effort counts
    A_I = effort_index_angler$count_index, #num vec; observed # of anglers
    day_A = left_join(effort_index_angler, days, by = "event_date") |> pull(day_index), # int; index for day/period
    gear_A = as.integer(rep(0, nrow(effort_index_angler))),   # int vec; index denoting "gear/angler type"
    section_A = as.integer(effort_index_angler$section_num), # int vec; index for section
    countnum_A = as.integer(effort_index_angler$count_sequence), # int vec; index for count_num
    
    # Census (tie-in) effort counts 
    E_n = nrow(effort_census), # int; total number of angler tie-in effort counts
    E_s = effort_census$count_census, # num vec; observed # of anglers
    day_E = left_join(effort_census, days, by = "event_date") |> pull(day_index), # int vec; index denoting day/period
    #10/12/22 DA temp fix - not yet assured to match interview|index
    #when angler_final takes "bank" and "boat" this resolves to 1 & 2
    gear_E = as.integer(factor(effort_census$angler_final)), # int vec; index denoting "gear/angler type"  
    section_E = as.integer(effort_census$section_num), # int vec; index for section
    countnum_E = as.integer(effort_census$count_sequence), # int vec; index for count_sequence
    

    #10/12/22 DA temp solution, pass in desired matrix built from dwg$effort
    #unclear on longer-term desired approach
    # Proportion tie-in expansion mat; proportion of section covered by tie in counts
    p_TI = tie_in_mat,
    
    # interview data - CPUE 
    IntC = nrow(distinct(interview_cg, interview_id)),  # int; total number of angler interviews with c & h data; distinct() here should be redundant
    day_IntC = left_join(interview_cg, days, by = "event_date") |> pull(day_index), # int vec; index denoting day/period   
    gear_IntC = interview_cg$angler_final_int, # int vec; index denoting "gear/angler type"
    section_IntC = interview_cg$section_num, # int vec; index for section
    c = interview_cg$fish_count, # num vec; total catch
    h = interview_cg$fishing_time_total, # num vec; total hours fished as fishing_time * person_count_final
    
    # interview data - Total Effort & Catch Creeled
    IntCreel = nrow(interview_cg_daily_summ), # int; totals from interviews aggregated by date-section-anglertype	
    day_Creel = left_join(interview_cg_daily_summ, days, by = "event_date") |> pull(day_index), # int vec; index denoting day/period
    gear_Creel = interview_cg_daily_summ$angler_final_int,  # int vec; index denoting "gear/angler type"
    section_Creel = interview_cg_daily_summ$section_num, # int vec; index for section 
    C_Creel = interview_cg_daily_summ$fish_count, # num vec; total reported catch by day-section-anglertype
    E_Creel = interview_cg_daily_summ$fishing_time_total,  #num vec; total hours fished by day-section-anglertype
    
    #10/12/22 DA temp soln; not sure exactly how to interpret "where V_A, T_A, A_A were collected" - all? any? something else?
    #Leaving for now same as IntC
    # interview data - angler expansion 
    IntA = nrow(distinct(interview_cg, interview_id)),     # int; total number of angler interviews where V_A, T_A, A_A were collected
    day_IntA = left_join(interview_cg, days, by = "event_date") |> pull(day_index), # int vec; index denoting day/period
    gear_IntA = interview_cg$angler_final_int, # int vec; index denoting "gear/angler type"
    section_IntA = interview_cg$section_num, # int vec; index for section
    V_A = interview_cg$vehicle_count,  # num vec; total number of vehicles an angler group brought
    T_A = interview_cg$trailer_count,  # num vec; total number of trailers an angler group brought
    A_A = interview_cg$angler_count,   # num vec; total number of anglers in the groups interviewed
    
    #priors
    #hyperhyper scale (degrees of freedom) parameters
    value_cauchyDF_sigma_eps_C = 0.5, #for the hyperprior distribution sigma_eps_C; default = 1  
    value_cauchyDF_sigma_eps_E = 0.5, #for the hyperprior distribution sigma_eps_E; default = 1  
    value_cauchyDF_sigma_r_E = 0.5,   #for the hyperprior distribution sigma_r_E; default = 1  
    value_cauchyDF_sigma_r_C = 0.5,   #for the hyperprior distribution sigma_r_C; default = 1 
    value_cauchyDF_sigma_mu_C = 0.5,  #the hyperhyper SD parameter in the hyperprior distribution sigma_mu_C
    value_cauchyDF_sigma_mu_E = 0.5,   #the hyperhyper SD parameter in the hyperprior distribution sigma_mu_E
    
    value_normal_sigma_omega_C_0 = 1, #the SD hyperparameter in the prior distribution omega_C_0; normal sd (log-space); default = 1   
    value_normal_sigma_omega_E_0 = 3, #the SD hyperparameter in the prior distribution omega_E_0; normal sd (log-space);; default = 3  
    value_lognormal_sigma_b = 1,      #the SD hyperparameter in the prior distribution b; default = 1  
    value_normal_sigma_B1 = 5,        #the SD hyperparameter in the prior distribution B1; default = 5  
    value_normal_mu_mu_C = log(0.02), #the mean hyperparameter in the prior distribution mu_C; median (log-space); default = 0.02 (was originally  0.05) 
    value_normal_sigma_mu_C = 1.5,    #the SD hyperparameter in the prior distribution mu_C; normal sd (log-space); default = 1.5 (was originally 5)
    value_normal_mu_mu_E = log(5),    #the mean hyperparameter in the prior distribution mu_E; median effort (log-space); default = 15 
    value_normal_sigma_mu_E = 2,      #the SD hyperparameter in the prior distribution mu_E; normal sd (log-space); default = 2 (was originally 5) 
    value_betashape_phi_E_scaled = 1, #the rate (alpha) and shape (beta) hyperparameters in phi_E_scaled; default = 1 (i.e., beta(1,1) which is uniform), alternative beta(2,2) 
    value_betashape_phi_C_scaled = 1 #the rate (alpha) and shape (beta) hyperparameters in phi_C_scaled; default = 1 (i.e., beta(1,1) which is uniform), alternative beta(2,2)
    
  )
  
 return(stan_list)   
}