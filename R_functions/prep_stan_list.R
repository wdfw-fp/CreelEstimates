
prep_stan_list <- function(
    period,
    days,
    interview,
    effort_index,
    effort_census,
    ...){
  
  effort_index_vehicle <- effort_index |> filter(str_detect(count_type, "ehicle"))
  
  list(
    D = nrow(days), # int; number of fishing days
    G = length(unique(interview$angler_final_int)),  # int; final number of unique gear/angler types 
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
    V_I = effort_index_vehicle$count_index, # int; observed # of vehicles 
    day_V = left_join(effort_index_vehicle, days, by = "event_date") |> pull(day_index),   # int; index for day/period 
    gear_V = as.integer(rep(1, nrow(effort_index_vehicle))), #update naming...
    section_V = as.integer(effort_index_vehicle$section_num), # int; index for section
    countnum_V = as.integer(effort_index_vehicle$count_sequence),     # int; index for count_num  

        
    # Trailer index effort counts
    # int; total number of boat trailer index effort counts 
    T_n = nrow(stan_data_prelim$effort_index),
    # int; observed # of boat trailers 
    T_I = stan_data_prelim$effort_index$`Trailers Only`,
    # int; index for day/period
    ## INTENTIONALLY LEFT "WRONG" TO MARK LATER REVISION IN ABOVE WRANGLING
    ## TO ALLOW RAGGED VEHICLE/TRAILER VECTORS 
    day_T = stan_data_prelim$effort_index$day_index, #day_V,
    gear_T = as.integer(rep(2, nrow(stan_data_prelim$effort_index))), #SAME - TEMP OPTION PENDING OTHER CHANGES
    # int; index for section
    section_T = stan_data_prelim$effort_index$section,
    # int; index for count_num  
    countnum_T = stan_data_prelim$effort_index$count_sequence,
    
    # Angler index effort counts
    # int; total number of angler index effort counts
    # Need to circle back on how to deal with this when applicable, set all values to 0 as a placeholder, matching example standat_2021-05-28.txt
    A_n = 0,
    # int; observed # of anglers
    A_I = numeric(0), #EB placeholder
    # int; index for day/period
    day_A = numeric(0),
    # int; index denoting "gear/angler type"  
    gear_A = numeric(0), #EB placeholder 
    # int; index for section 
    section_A = numeric(0),
    # int; index for count_num
    countnum_A = numeric(0),
    
    
    # Census (tie-in) effort counts   
    # int; total number of angler tie-in effort counts
    E_n = nrow(stan_data_prelim$effort_census),
    # int; index denoting day/period
    day_E = stan_data_prelim$effort_census$day_index,
    # int; index denoting "gear/angler type"  
    gear_E = stan_data_prelim$effort_census$angler_type_ind, #if_else(stan_data_prelim$effort_census$angler_type == "Boat", 2, 1),
    # int; index for section
    section_E = stan_data_prelim$effort_census$section,
    # EB count_sequence here needs to match that of the closest effort_index count (if my understanding is correct)
    # int; index for count_num
    countnum_E = stan_data_prelim$effort_census$count_sequence, 
    # int; observed # of anglers
    E_s = stan_data_prelim$effort_census$count_quantity,
    
    # Proportion tie-in expansion mat; proportion of section covered by tie in counts KB: user defined (see "Proportional_Expansions_for_Tie_In_Sections_Kalama_Example"; need to format)
    
    p_TI = lu_input$census_exp |> 
      select(gear_num, angler_type, section, p_TI) |> 
      pivot_wider(names_from = section, values_from = p_TI) |> 
      select(-gear_num, -angler_type) |> 
      as.matrix(),
    
    # interview data - CPUE 
    # int; total number of angler interviews with c & h data 
    IntC = nrow(stan_data_prelim$interview),
    # int; index denoting day/period   
    day_IntC = stan_data_prelim$interview$day_index,
    # int; index denoting "gear/angler type" 
    gear_IntC = stan_data_prelim$interview$angler_type_ind, #if_else(stan_data_prelim$interview$angler_type == "Boat", 2, 1),
    # int; index for section
    section_IntC = stan_data_prelim$interview$section, 
    # int; total catch
    c = stan_data_prelim$interview$fish_count,
    # vec; total hours fished
    # EB Does Total_Hours refer to fishing time (angler hours) multiplied by the total number of anglers in an                    interviewed party (group_angler_hours)? 
    h = stan_data_prelim$interview$angler_hours_total,
    
    # interview data - Total Effort & Catch Creeled
    # int; total interviews by sub-groups	
    IntCreel = nrow(stan_data_prelim$interview_daily_totals),
    # int; index denoting day/period     
    day_Creel = stan_data_prelim$interview_daily_totals$day_index,
    # int; index denoting "gear/angler type"  
    gear_Creel = stan_data_prelim$interview_daily_totals$angler_type_ind, #if_else(stan_data_prelim$interview_daily_totals$angler_type == "Boat", 2, 1),
    # int; index for section 
    section_Creel = stan_data_prelim$interview_daily_totals$section,
    # int; total catch  
    C_Creel = stan_data_prelim$interview_daily_totals$catch_dailysum,	  
    # vec; total hours fished 
    E_Creel = stan_data_prelim$interview_daily_totals$angler_hours_total_dailysum,
    
    # interview data - angler expansion 
    
    # int; total number of angler interviews where V_A, T_A, A_A were collected 
    IntA = nrow(stan_data_prelim$interview),
    # int; index denoting day/period
    day_IntA = stan_data_prelim$interview$day_index,
    # int; index denoting gear/angler  
    gear_IntA = stan_data_prelim$interview$angler_type_ind, #if_else(stan_data_prelim$interview$angler_type == "Boat", 2, 1),
    # int; index denoting day/period
    section_IntA = stan_data_prelim$interview$section, 
    # int; total number of vehicles an angler group brought
    V_A = stan_data_prelim$interview$vehicle_count,
    # int; total number of trailers an angler group brought
    T_A = stan_data_prelim$interview$trailer_count, 
    # int; total number of anglers in the groups interviewed
    A_A = stan_data_prelim$interview$angler_count,
    
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
    
}