# calcs fishing_time & total time conditional on use angler vs group counts

prep_dwg_interview_fishing_time <- function(
    dwg_interview,          # interview data from dwg filtered using start & end dates passed from params
    min_fishing_time,       # numeric passed from params to filter fishing_time at least as long as this per-person duration
    study_design,           # string passed from params denoting which study design was followed during data collection
    ...
    ){

if(str_detect(study_design, "tandard" ) | study_design == "Drano"){ # If Study design in the YAML is specified as [S]tandard or Drano proceed with running this function...  
  
  # Define person_count_final based on study_design and, if needed, user designated person_count_type   
    if (str_detect(study_design, "tandard" )) { 
      if (exists("person_count_type")) { #If "person_count_type" is specified in YAML then use entry ("angler" vs. "group") to assign person_count_final as either...
        if (person_count_type == "angler") {
          dwg_interview$person_count_final <- dwg_interview$angler_count; #...the count of anglers from interviews, which is strictly the number of people that were/are fishing/angling (as opposed to total group count)
        } else if (person_count_type == "group"){
          dwg_interview$person_count_final <- dwg_interview$total_group_count; #...or the total group count, which is the ount of people present on the fishing trip regardless if they fished, including guides and non-anglers
        }
      } else { #If "person_count_type" is not specified in YAML then use total_group_count for person_count_final 
        dwg_interview$person_count_final <- dwg_interview$total_group_count;
      }
    } else if (study_design == "Drano") {
      dwg_interview$person_count_final <- dwg_interview$angler_count
    } 

  # calculate fishing time
    interview_fishing_time <- 
      dwg_interview |> 
      dplyr::mutate(
        trip_status = replace_na(trip_status, "Unknown"),
        end_time_final = 
          dplyr::if_else(
            trip_status == "Incomplete" | is.na(fishing_end_time),
            interview_time,
            fishing_end_time
          ),
        fishing_time = round(as.numeric(end_time_final - fishing_start_time) / 3600, 5),
        vehicle_count = if_else(vehicle_count > person_count_final, person_count_final, vehicle_count), #KB - I don't understand why this line of code is here; seems like a QAQC update but not sure
        fishing_time_total = fishing_time * person_count_final
      ) |>
      dplyr::filter(fishing_time >= min_fishing_time) # |> 
  
    return(interview_fishing_time)
    
}else{
  print("study_design specified in YAML does not match one of the existing options")
}
  
}

