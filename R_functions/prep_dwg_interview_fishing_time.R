# calcs fishing_time & total time conditional on use angler vs group counts

prep_dwg_interview_fishing_time <- function(
    dwg_interview,          # interview data from dwg filtered using start & end dates passed from params
    person_count_type = NA, # string passed from params controlling angler_count vs total_group_count 
    min_fishing_time,       # numeric passed from params to filter fishing_time at least as long as this per-person duration
    study_design,           # string passed from params denoting which study design was followed during data collection
    ...){
  
# Define person_count_final based on study_design and, if needed, user designated person_count_type 
  if(str_detect(study_design, "tandard" )){
    if(person_count_type == "group" | is.na(person_count_type)){
      dwg_interview$person_count_final <- dwg_interview$total_group_count
    }else{
      dwg_interview$person_count_final <- dwg_interview$angler_count
    }
  }else if(study_design == "Drano"){
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
}

