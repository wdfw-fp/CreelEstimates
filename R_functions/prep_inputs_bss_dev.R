prep_inputs_bss_dev <- function(
    period,
    days, #tibble with time strata and closure fields
    dwg_summarized, #list with shared interview, index and census tibbles
    est_catch_group,
    tie_in_mat,
    priors,
    study_design,
    ...){

if(str_detect(study_design, "tandard" )){ #KB addition
  #in-function-scope intermediates
  effort_index_vehicle <- dwg_summarized$effort_index |> filter(str_detect(count_type, "ehicle"))
  effort_index_trailer <- dwg_summarized$effort_index |> filter(str_detect(count_type, "railer"))
  effort_index_angler <- dwg_summarized$effort_index |> filter(str_detect(count_type, "ngler")) |> 
    #based on outdated study design/database values that do not include current, more detailed census count levels  
    mutate(
      angler_final_int = case_when(
        count_type == "Bank Anglers"  ~ as.integer(1),
        count_type == "Boat Anglers"  ~ as.integer(2)
      )
    )
  
  interview_cg <- 
    dwg_summarized$interview |> 
    filter(est_cg == est_catch_group)
  
  interview_cg_intA <- 
    interview_cg |> 
    drop_na(vehicle_count, trailer_count, person_count_final) 
  
  interview_cg_boat<- interview_cg_intA # KB: interview_cg_boat isn't actually used in the "standard" study design analysis but has to be created/duplicated so that the case_when argument works
  
  interview_cg_daily_summ <- #KB NOTE: this object is used to create data objects that are no longer used in the most up-to-date model (perhaps could be omitted via deletion or at least commented out)
    interview_cg |> 
    group_by(event_date, section_num, angler_final_int) |> 
    summarise(across(c(fishing_time_total, fish_count), sum), .groups = "drop")

}else if(study_design == "Drano"){ #KB addition
  effort_index_vehicle <- dwg_summarized$effort_index |> filter(str_detect(angler_final, "iehicle"))
  effort_index_trailer <- dwg_summarized$effort_index |> filter(str_detect(angler_final, "railer"))
  effort_index_angler <- dwg_summarized$effort_index |> filter(str_detect(angler_final, "bank"))
  effort_index_boats <- dwg_summarized$effort_index |> filter(str_detect(angler_final, "boat"))
  
  interview_cg <- 
    dwg_summarized$interview |> 
    filter(est_cg == est_catch_group)
  
  interview_cg_boat<- #KB addition
    interview_cg |> 
    filter(str_detect(angler_final, "boat"))
  
  interview_cg_intA <- interview_cg_boat |> mutate(vehicle_count = as.integer(0), trailer_count = as.integer(0))# KB: interview_cg_intA isn't actually used in the "Drano" study design analysis but has to be created/duplicated so that the case_when argument works

  interview_cg_daily_summ <- #KB NOTE: this object is used to create data objects that are no longer used in the most up-to-date model (perhaps could be omitted via deletion or at least commented out)
    interview_cg |> 
    group_by(event_date, section_num, angler_final_int) |> 
    summarise(across(c(fishing_time_total, fish_count), sum), .groups = "drop")

}  
  
  #returned list object
  stan_list <- list(
    est_cg = est_catch_group,
    D = nrow(days), # int; number of fishing days
    G = length(unique(interview_cg$angler_final_int)),  # int; final number of unique gear/angler types 
    S = as.integer(length(unique(dwg_summarized$effort_census$section_num))),  # int; final number of river sections  
    H = max(dwg_summarized$effort_index$count_sequence), # int; max number of index counts within a sample day
    
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

    w = days$day_type_num, # int vec; 0/1 denoting Weekday/end for model offset
    L = days$day_length,  # num vec, daylength (model offset; assumption)
    # num mat; open/closed by section; 0 defined as 1E-6 for model
    O = days |> 
      select(contains("section_")) |>
      mutate(across(everything(), ~if_else(., 1, 0.000001))) |> 
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
    gear_T = as.integer(rep(2, nrow(effort_index_trailer))), 
    section_T = as.integer(effort_index_trailer$section_num), # int vec; index for section
    countnum_T = as.integer(effort_index_trailer$count_sequence), # int vec; index for count_sequence  
    
#KB: start back here - not exactly what data we'll need for updated likelihoods...
#KB ...we'll need to estimate bank effort, which is simply using the bank angler index counts.  but for boats, we'll assume what we saw was a census (so bias term would be 1)...
#KB ... but then divide that value by vessels per angler to get boat angler index counts   
#KB ...below, I've broken up the index data into two groupings - bank anglers (_A) and boats (_B) thinking that the boat counts will need to be multipled by vessels per angler
#KB ...However, this results in the index count vectors not matching the census count vector (which has both the bank and boat data)
    
    # Angler index effort counts
    A_n = nrow(effort_index_angler), # int; total number of angler index effort counts
    A_I = effort_index_angler$count_index, #num vec; observed # of anglers
    day_A = left_join(effort_index_angler, days, by = "event_date") |> pull(day_index), # int; index for day/period
    gear_A = effort_index_angler$angler_final_int, # int vec; index denoting "gear/angler type"
    section_A = as.integer(effort_index_angler$section_num), # int vec; index for section
    countnum_A = as.integer(effort_index_angler$count_sequence), # int vec; index for count_num
    
  ################################################# ###
  #KB addition: start 

  ## KB NOTE: not sure this new set of data parameters are needed; instead, I probably want to update the above section 
  
    # boat index effort counts
    B_n = nrow(effort_index_boats), # int; total number of angler index effort counts
    B_I = effort_index_boats$count_index, #num vec; observed # of anglers
    day_B = left_join(effort_index_boats, days, by = "event_date") |> pull(day_index), # int; index for day/period
    gear_B = effort_index_boats$angler_final_int, # int vec; index denoting "gear/angler type"
    section_B = as.integer(effort_index_boats$section_num), # int vec; index for section
    countnum_B = as.integer(effort_index_boats$count_sequence), # int vec; index for count_num
  
  #KB addition: end
  ################################################# ###

    # Census (tie-in) effort counts 
    E_n = nrow(dwg_summarized$effort_census), # int; total number of angler tie-in effort counts
    E_s = dwg_summarized$effort_census$count_census, # num vec; observed # of anglers
    day_E = left_join(dwg_summarized$effort_census, days, by = "event_date") |> pull(day_index), # int vec; index denoting day/period
    gear_E = dwg_summarized$effort_census$angler_final_int, # int vec; index denoting "gear/angler type"  
    section_E = as.integer(dwg_summarized$effort_census$section_num), # int vec; index for section
    countnum_E = as.integer(dwg_summarized$effort_census$count_sequence), # int vec; index for count_sequence
    

#KB update (4/2/24): previously, the object "tie_in_mat" was created outside the function using "dwg$effort"; however, our script created the object "inputs_pe$census_expan" which got us most of the way to "p_TI"; therefore, I updated the data wrangling and moved inside this funciton
    # Proportion tie-in expansion mat; proportion of section covered by tie in counts
#KB edit:    p_TI =   tie_in_mat, 
    p_TI = 
      tie_in_mat |> 
      select(angler_final, section_num, p_census) |> 
      pivot_wider(names_from = section_num, values_from = p_census) |> 
      select(-angler_final) |> 
      as.matrix(),
    
    # interview data - CPUE 
    IntC = nrow(distinct(interview_cg, interview_id)),  # int; total number of angler interviews with c & h data; distinct() here should be redundant
    day_IntC = left_join(interview_cg, days, by = "event_date") |> pull(day_index), # int vec; index denoting day/period   
    gear_IntC = interview_cg$angler_final_int, # int vec; index denoting "gear/angler type"
    section_IntC = interview_cg$section_num, # int vec; index for section
    c = interview_cg$fish_count, # num vec; total catch
    h = interview_cg$fishing_time_total, # num vec; total hours fished as fishing_time * person_count_final
    
#KB NOTE: The following six objects are no longer used in the most up-to-date creel model (thus, perhaps they could be removed or at least commented out?)
    # interview data - Total Effort & Catch Creeled
    IntCreel = nrow(interview_cg_daily_summ), # int; totals from interviews aggregated by date-section-anglertype	
    day_Creel = left_join(interview_cg_daily_summ, days, by = "event_date") |> pull(day_index), # int vec; index denoting day/period
    gear_Creel = interview_cg_daily_summ$angler_final_int,  # int vec; index denoting "gear/angler type"
    section_Creel = interview_cg_daily_summ$section_num, # int vec; index for section 
    C_Creel = interview_cg_daily_summ$fish_count, # num vec; total reported catch by day-section-anglertype
    E_Creel = interview_cg_daily_summ$fishing_time_total,  #num vec; total hours fished by day-section-anglertype
    
#KB update: I commented out the following 10 lines and replaced using "case_when"

    # #10/12/22 DA temp soln; not sure exactly how to interpret "where V_A, T_A, A_A were collected" - all? any? something else?
    # #Leaving for now same as IntC
    # # interview data - angler expansion 
    # IntA = nrow(distinct(interview_cg_intA, interview_id)),     # int; total number of angler interviews where V_A, T_A, A_A were collected
    # day_IntA = left_join(interview_cg_intA, days, by = "event_date") |> pull(day_index), # int vec; index denoting day/period
    # gear_IntA = interview_cg_intA$angler_final_int, # int vec; index denoting "gear/angler type"
    # section_IntA = interview_cg_intA$section_num, # int vec; index for section
    # V_A = as.integer(interview_cg_intA$vehicle_count),  # num vec; total number of vehicles an angler group brought
    # T_A = as.integer(interview_cg_intA$trailer_count),  # num vec; total number of trailers an angler group brought
    # A_A = as.integer(interview_cg_intA$person_count_final),  # num vec; total number of anglers in the groups interviewed

  ################################################# ###
  #KB addition: start 

#KB: I'm having issues with the vector lengths on the left-hand side of the case_when scenarios not matching in length
#KB: However, I'm not sure this set up will even work they way the likelihood is set up now with just IntA which will always be >0 with this script and the two study designs...
#KB  ...but we won't want to loop over all these variables (for example, with the standard study designs we won't have or want to calculate boats per anglers and with Drano study design we won't have or want to calculate...
#KB  ... trailers and vehicles per angler)

    IntA = case_when( # int; total number of angler interviews... 
      str_detect(study_design, "tandard" )  ~ nrow(distinct(interview_cg_intA, interview_id)), #...where V_A, T_A, A_A were collected when implementing "standard" study design
      study_design == "Drano"  ~ nrow(distinct(interview_cg_boat, interview_id))               #...where angler_final was a boat group when implementing "Drano" study design
    ),    
    day_IntA = case_when( # int vec; index denoting day/period...
      str_detect(study_design, "tandard" )  ~ left_join(interview_cg_intA, days, by = "event_date") |> pull(day_index),
      study_design == "Drano"  ~ left_join(interview_cg_boat, days, by = "event_date") |> pull(day_index)
    ),
    gear_IntA = case_when( # int vec; index denoting "gear/angler type"
      str_detect(study_design, "tandard" )  ~ interview_cg_intA$angler_final_int,
      study_design == "Drano"  ~ interview_cg_boat$angler_final_int   # KB NOTE: will this be a problem the "angler_final_int" was created before "bank" angler_final grouping was filtered out? (now goes 2-4)
    ),
    section_IntA = case_when( # int vec; index for section
      str_detect(study_design, "tandard" )  ~ interview_cg_intA$section_num,
      study_design == "Drano"  ~ interview_cg_boat$section_num
    ),
    V_A = case_when( # num vec; total number of vehicles an angler group brought
      str_detect(study_design, "tandard" )  ~ as.integer(interview_cg_intA$vehicle_count),
      study_design == "Drano"  ~ as.integer()
    ),
    T_A = case_when( # num vec; total number of trailers an angler group brought
      str_detect(study_design, "tandard" )  ~ as.integer(interview_cg_intA$trailer_count),
      study_design == "Drano"  ~ as.integer()
    ),
    A_A = case_when(  # num vec; total number of anglers in the groups interviewed
      str_detect(study_design, "tandard" )  ~ as.integer(interview_cg_intA$person_count_final),
      study_design == "Drano"  ~ as.integer(interview_cg_boat$person_count_final)
    ),
    B_A = case_when(  # num vec; total number of boats an angler group brought
      str_detect(study_design, "tandard" )  ~ as.integer(),
      study_design == "Drano"  ~ rep(1, nrow(interview_cg_boat))  #study design set up so that boat groups consisted of 1 boat
    ),

  #KB addition: end
  ################################################# ###

    #priors
    #hyperhyper scale (degrees of freedom) parameters
    value_cauchyDF_sigma_eps_C = priors["value_cauchyDF_sigma_eps_C"] , #for the hyperprior distribution sigma_eps_C; default = 1  
    value_cauchyDF_sigma_eps_E = priors["value_cauchyDF_sigma_eps_E"], #for the hyperprior distribution sigma_eps_E; default = 1  
    value_cauchyDF_sigma_r_E = priors["value_cauchyDF_sigma_r_E"],   #for the hyperprior distribution sigma_r_E; default = 1  
    value_cauchyDF_sigma_r_C = priors["value_cauchyDF_sigma_r_C"],   #for the hyperprior distribution sigma_r_C; default = 1 
    value_cauchyDF_sigma_mu_C = priors["value_cauchyDF_sigma_mu_C"],  #the hyperhyper SD parameter in the hyperprior distribution sigma_mu_C
    value_cauchyDF_sigma_mu_E = priors["value_cauchyDF_sigma_mu_E"],   #the hyperhyper SD parameter in the hyperprior distribution sigma_mu_E
    
    value_normal_sigma_omega_C_0 = priors["value_normal_sigma_omega_C_0"], #the SD hyperparameter in the prior distribution omega_C_0; normal sd (log-space); default = 1   
    value_normal_sigma_omega_E_0 = priors["value_normal_sigma_omega_E_0"], #the SD hyperparameter in the prior distribution omega_E_0; normal sd (log-space);; default = 3  
    value_lognormal_sigma_b = priors["value_lognormal_sigma_b"],      #the SD hyperparameter in the prior distribution b; default = 1  
    value_normal_sigma_B1 = priors["value_normal_sigma_B1"],        #the SD hyperparameter in the prior distribution B1; default = 5  
    value_normal_mu_mu_C = priors["value_normal_mu_mu_C"], #the mean hyperparameter in the prior distribution mu_C; median (log-space); default = 0.02 (was originally  0.05) 
    value_normal_sigma_mu_C = priors["value_normal_sigma_mu_C"],    #the SD hyperparameter in the prior distribution mu_C; normal sd (log-space); default = 1.5 (was originally 5)
    value_normal_mu_mu_E = priors["value_normal_mu_mu_E"],    #the mean hyperparameter in the prior distribution mu_E; median effort (log-space); default = 15 
    value_normal_sigma_mu_E = priors["value_normal_sigma_mu_E"],      #the SD hyperparameter in the prior distribution mu_E; normal sd (log-space); default = 2 (was originally 5) 
    value_betashape_phi_E_scaled = priors["value_betashape_phi_E_scaled"], #the rate (alpha) and shape (beta) hyperparameters in phi_E_scaled; default = 1 (i.e., beta(1,1) which is uniform), alternative beta(2,2) 
    value_betashape_phi_C_scaled = priors["value_betashape_phi_C_scaled"] #the rate (alpha) and shape (beta) hyperparameters in phi_C_scaled; default = 1 (i.e., beta(1,1) which is uniform), alternative beta(2,2)
    
  )
  
 return(stan_list)   
}