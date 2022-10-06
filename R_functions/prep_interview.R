#creates angler_final categorical and calcs fishing_time & total time conditional on user choice of angler vs group counts
#joins catch values (wide)

#best practice: section_num of fishing_loc as Section
#when no fishing_loc, then can use section_num of interview_loc
#but question of what section_num is assigned if both present

prep_interview <- function(
    interview, 
    catch,
    person_count_type, #string passed from params controlling angler_count vs total_group_count 
    min_fishing_time, #numeric passed from params to filter fishing_time at least as long as this per-person duration
    ...){
  
  if(any(is.na(c(interview$vehicle_count, interview$trailer_count)))){
    cat("Interview data have NA values for vehicle/trailer_count fields\n These records are dropped!")
  }

  interview |> 
    tidyr::drop_na(vehicle_count, trailer_count) |> 
    filter(is.na(angler_type) | str_detect(angler_type, "ank|oat")) |> 
    dplyr::mutate(
      trip_status = replace_na(trip_status, "Unknown"),
      
      angler_final = dplyr::case_when( 
        tolower(angler_type) == "boat" ~ "boat", 
        tolower(angler_type) == "bank" ~ "bank", 

        boat_used == "No" ~ "bank",
        boat_used == "Yes" & 
          stringr::str_detect(boat_type, "ontoon|ayak") & 
          (is.na(fish_from_boat) | fish_from_boat == "Bank") ~ "bank",
        #allow kayaks as "boat": boat_used == "Yes" & stringr::str_detect(boat_type, "ontoon|ayak") & fish_from_boat == "Boat" ~ "boat",
        boat_used == "Yes" & 
          !stringr::str_detect(boat_type, "ontoon|ayak") & 
          (is.na(fish_from_boat) | fish_from_boat == "Boat") ~ "boat",
        boat_used == "Yes" & !stringr::str_detect(boat_type, "ontoon|ayak") & 
          fish_from_boat == "Bank" ~ "boat"
        # EB these cases were left NA when tested on skagit fall salmon 2022 data; assigning to boat under current angler_type assignment protocol
      ),
      angler_final_int = as.integer(factor(angler_final)),
      
      end_time_final = dplyr::if_else(
        trip_status == "Incomplete" | is.na(fishing_end_time),
        interview_time,
        fishing_end_time),
      fishing_time = round(as.numeric(end_time_final - fishing_start_time) / 3600, 5),
      
      person_count_final = case_when(
        person_count_type == "group" ~ total_group_count,
        person_count_type == "angler" ~ angler_count
      ),
      
      fishing_time_total = fishing_time * person_count_final
    ) |>
    dplyr::filter(fishing_time >= min_fishing_time) |> 
    dplyr::select(
      interview_id, 
      section_num, event_date, angler_final, angler_final_int,
      vehicle_count, trailer_count,
      angler_count, total_group_count,
      fishing_time, person_count_final, fishing_time_total, 
      trip_status, previously_interviewed
      # -creel_event_id, -water_body, -project_name, -interview_number,
      # -crc_area, -fishing_location, -ends_with("_time"),
      # -comment_txt, -water_body_desc
    ) |> 
    dplyr::arrange(section_num, event_date, angler_final) |> 
    dplyr::left_join(
      catch |> 
        group_by(interview_id, catch_group) |> 
        summarise(fish_count = sum(fish_count, na.rm = T), .groups = "drop") |> 
        pivot_wider(names_from = catch_group, values_from = fish_count) |> 
        pivot_longer(cols = -interview_id, names_to = "catch_group", values_to = "fish_count") |> 
        mutate(fish_count = replace_na(fish_count, 0))
      ,
      by = "interview_id"
    )
  
}

