#creates angler_final categorical and calcs fishing_time & total time conditional on user choice of angler vs group counts
#joins catch values (wide)

#section_num of fishing_loc is assigned as Section whether or not interview_loc present
#when no fishing_loc, then use section_num of interview_loc

prep_dwg_interview_angler_types <- function(
    interview_fishing_time, 
    study_design,
    boat_type_collapse = NA,
    fish_location_determines_type = NA,
    angler_type_kayak_pontoon = NA, 
#KB    dwg_catch, 
#KB    person_count_type, #string passed from params controlling angler_count vs total_group_count 
#KB    min_fishing_time,  #numeric passed from params to filter fishing_time at least as long as this per-person duration
#KB    est_catch_groups, #data.fram passed from params of possibly-aggregated catch groups of interest to estimate
    ...){
  
# KB - commented out following 3 lines (not entirely sure why this warning message is needed)  
  # if(any(is.na(c(dwg_interview$vehicle_count, dwg_interview$trailer_count)))){ 
  #   cat("Interview data have NA values for vehicle/trailer_count fields")      
  # } 
  
# KB - commented out following 3 lines, which will be moved to another interview wrangling function      
  # #coerce missing values to actual strings to allow params$est_catch_groups to include NAs alongside non-NA 
  # #to allow 'run' specification in params, add 'run' within across() 
  # dwg_catch <- dwg_catch |> mutate(across(c(species, life_stage, fin_mark, fate), ~replace_na(as.character(.), "NA")))

# KB - commented out following 3 lines (these were used in the preceding new/updated ""prep_dwg_interview_fishing_time" function)   
  # if(person_count_type == "group"){
  #   dwg_interview$person_count_final <- dwg_interview$total_group_count
  # }else if (person_count_type == "angler"){
  #   dwg_interview$person_count_final <- dwg_interview$angler_count
  # }
# KB - commented out following 25 lines and replaced within the if/else if portion below; "angler_final" designations now use the two new arguments "fish_location_determines_type" & "angler_type_kayak_pontoon"
  
  #   interview_angler_types <- # KB addition
  #     interview_fishing_time |>   # KB addition
  # #KB     dwg_interview |> 
  #     filter(is.na(angler_type) | str_detect(angler_type, "ank|oat")) |> 
  #     dplyr::mutate(
  # # KB - commented out following 1 line (this were used in the preceding new/updated ""prep_dwg_interview_fishing_time" function)        
  #       # trip_status = replace_na(trip_status, "Unknown"),
  #       angler_final = dplyr::case_when( 
  #         tolower(angler_type) == "boat" ~ "boat", 
  #         tolower(angler_type) == "bank" ~ "bank", 
  #         
  #         boat_used == "No" ~ "bank", 
  #         boat_used == "Yes" & 
  #           stringr::str_detect(boat_type, "ontoon|ayak") & 
  #           (is.na(fish_from_boat) | fish_from_boat == "Bank") ~ "bank", 
  #         #allow kayaks as "boat":
  #         boat_used == "Yes" & stringr::str_detect(boat_type, "ontoon|ayak") & fish_from_boat == "Boat" ~ "boat", # EB if kayaks that fished from boat are left NA, they will break the code for BSS 
  #         boat_used == "Yes" & 
  #           !stringr::str_detect(boat_type, "ontoon|ayak") & 
  #           (is.na(fish_from_boat) | fish_from_boat == "Boat") ~ "boat", 
  #         boat_used == "Yes" & !stringr::str_detect(boat_type, "ontoon|ayak") & 
  #           fish_from_boat == "Bank" ~ "boat" # EB anglers who used a non- "ontoon|ayak" boat but primarily fished from shore 
  #       ),
  #       angler_final_int = as.integer(factor(angler_final)) #, #KB
  #     ) #KB addition
  

if(str_detect(study_design, "tandard" )){ #KB addition

  #KB  interviews <- 
  interview_angler_types <- # KB addition
    interview_fishing_time |>  # KB addition
    #KB     dwg_interview |> 
    #KB     filter(is.na(angler_type) | str_detect(angler_type, "ank|oat")) |> 
    dplyr::mutate(
      # KB - commented out following 1 line (this were used in the preceding new/updated ""prep_dwg_interview_fishing_time" function)
      # trip_status = replace_na(trip_status, "Unknown"),
      angler_final = 
        dplyr::case_when(
          boat_used == "No" ~ "bank", #bank: if anglers did not have/use a boat
          
          boat_used == "Yes" & is.na(fish_from_boat) & !stringr::str_detect(boat_type, "ontoon|ayak") ~ "boat", # boat: if anglers had a boat, were NOT asked where they primarily fished (boat vs. bank), and not a "ontoon|ayak"

          boat_used == "Yes" & is.na(fish_from_boat) & stringr::str_detect(boat_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") ~ "boat", # boat: if anglers had a boat, were NOT asked where they primarily fished (boat vs. bank), were in a "ontoon|ayak" AND "ontoon|ayak" considered a boat
          boat_used == "Yes" & is.na(fish_from_boat) & stringr::str_detect(boat_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "ank") ~ "bank", # bank: if anglers had a boat, were NOT asked where they primarily fished (boat vs. bank), were in a "ontoon|ayak" AND "ontoon|ayak" considered a bank

          boat_used == "Yes" & !is.na(fish_from_boat) & stringr::str_detect(fish_location_determines_type, "Y") & stringr::str_detect(fish_from_boat, "Y|oat")  ~ "boat", # boat: if anglers had a boat, were asked where they primarily fished (boat vs. bank) AND the fishing location determines the angler type, and primarily fish from a boat
          boat_used == "Yes" & !is.na(fish_from_boat) & stringr::str_detect(fish_location_determines_type, "Y") & stringr::str_detect(fish_from_boat, "N|ank")  ~ "bank", # bank: if anglers had a boat, were asked where they primarily fished (boat vs. bank) AND the fishing location determines the angler type, and primarily fish from a bank

          boat_used == "Yes" & !is.na(fish_from_boat) & stringr::str_detect(fish_location_determines_type, "N") & !stringr::str_detect(boat_type, "ontoon|ayak") ~ "boat", # boat: if anglers had a boat, were asked where they primarily fished (boat vs. bank) BUT the fishing location does NOT determine the angler type, and were NOT in a "ontoon|ayak"
            
          boat_used == "Yes" & !is.na(fish_from_boat) & stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(boat_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") ~ "boat", # boat: if anglers had a boat, were asked where they primarily fished (boat vs. bank) BUT the fishing location does NOT determine the angler type, were in a "ontoon|ayak" AND "ontoon|ayak" considered a boat
          boat_used == "Yes" & !is.na(fish_from_boat) & stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(boat_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "ank") ~ "bank", # bank: if anglers had a boat, were asked where they primarily fished (boat vs. bank) BUT the fishing location does NOT determine the angler type, were in a "ontoon|ayak" AND "ontoon|ayak" considered a bank
          TRUE ~ "fail"
        )
      , angler_final_int = as.integer(factor(angler_final)) #, #KB
    ) #KB addition
  
}else if(study_design == "Drano"){ #KB addition
#KB  interviews <- 
  interview_angler_types <- # KB addition
    interview_fishing_time |>  # KB addition
#KB     dwg_interview |> 
#KB     filter(is.na(angler_type) | str_detect(angler_type, "ank|oat")) |> 
    dplyr::mutate(
# KB - commented out following 1 line (this were used in the preceding new/updated ""prep_dwg_interview_fishing_time" function)
      # trip_status = replace_na(trip_status, "Unknown"),
      angler_final = 
        dplyr::case_when(
          boat_used == "No" ~ "bank",
          
          boat_used == "Yes" & stringr::str_detect(boat_type, "railered|Motorized") & stringr::str_detect(boat_type_collapse, "Y") ~ "boat",
          boat_used == "Yes" & stringr::str_detect(boat_type, "railered|Motorized") & stringr::str_detect(boat_type_collapse, "N") ~ "boat_motor",
          
          boat_used == "Yes" & stringr::str_detect(boat_type, "Skiff|Pram") & stringr::str_detect(boat_type_collapse, "Y") ~ "boat",
          boat_used == "Yes" & stringr::str_detect(boat_type, "Skiff|Pram") & stringr::str_detect(boat_type_collapse, "N") ~  "boat_skiff",
          
          boat_used == "Yes" & stringr::str_detect(boat_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") & stringr::str_detect(boat_type_collapse, "Y") ~ "boat",
          boat_used == "Yes" & stringr::str_detect(boat_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") & stringr::str_detect(boat_type_collapse, "N") ~ "boat_single",
          boat_used == "Yes" & stringr::str_detect(boat_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "ank")  ~ "bank",
          TRUE ~ "fail"
         )
      , angler_final_int = as.integer(factor(angler_final)) #, #KB
    ) #KB addition
}  #KB addition
    

# KB - commented out following 5 lines (these were used in the preceding new/updated ""prep_dwg_interview_fishing_time" function)     
      # end_time_final = dplyr::if_else(
      #   trip_status == "Incomplete" | is.na(fishing_end_time),
      #   interview_time,
      #   fishing_end_time),
      # fishing_time = round(as.numeric(end_time_final - fishing_start_time) / 3600, 5),
      
# KB - these 6 lines were already commented out in the original "prep_dwg_interview" function
      # ,
      # 
      # person_count_final = case_when(
      #   person_count_type == "group" ~ total_group_count,
      #   person_count_type == "angler" ~ angler_count # EB this fails when using the "angler" person_count_type option
      # ),

# KB - commented out following 4 lines (these were used in the preceding new/updated ""prep_dwg_interview_fishing_time" function) 
    #   vehicle_count = if_else(vehicle_count > person_count_final, person_count_final, vehicle_count), #KB - I don't understand why this line of code is here; seems like a QAQC update but not sure
    #   fishing_time_total = fishing_time * person_count_final
    # ) |>
    # dplyr::filter(fishing_time >= min_fishing_time) # |> # KB - commented out the pipe here
  
# KB - commented out following 12 lines, need to review and update as needed, in final "interview" output/df
  # dplyr::select(
  #   interview_id, 
  #   section_num, event_date, angler_final, angler_final_int,
  #   vehicle_count, trailer_count,
  #   angler_count, total_group_count,
  #   fishing_time, person_count_final, fishing_time_total, 
  #   trip_status, previously_interviewed
  #   # -creel_event_id, -water_body, -project_name, -interview_number,
  #   # -crc_area, -fishing_location, -ends_with("_time"),
  #   # -comment_txt, -water_body_desc
  # ) |> 
  # dplyr::arrange(section_num, event_date, angler_final) # KB - review and update this line, as needed, in final "interview" output/df

# KB - commented out following 28 lines, which will be moved to another interview wrangling function
  # catches <- map_df(
  #   1:nrow(est_catch_groups),
  #   ~dwg_catch |> 
  #     filter(
  #       str_detect(species, est_catch_groups$species[.x]),
  #       str_detect(life_stage, est_catch_groups$life_stage[.x]),
  #       str_detect(fin_mark, est_catch_groups$fin_mark[.x]),
  #       str_detect(fate, est_catch_groups$fate[.x])
  #     ) |> 
  #     mutate(
  #       est_cg = paste0(unlist(est_catch_groups[.x,]), collapse = "_")
  #     ) |> 
  #     group_by(est_cg, interview_id) |> 
  #     summarise(fish_count = sum(fish_count, na.rm = T), .groups = "drop")
  # )
  # 
  # #replicate wrangled interviews n-many of catch_groups to estimate
  # int_cat <- map_df(
  #   1:nrow(est_catch_groups), #unique(catches$est_cg),
  #   ~interviews |> 
  #     mutate(est_cg = paste0(unlist(est_catch_groups[.x,]), collapse = "_")) 
  #   ) |> 
  #   left_join(catches, by = c("est_cg", "interview_id")) |> 
  #   mutate(fish_count = replace_na(fish_count, 0)) |> 
  #   mutate(
  #     fishery_name = params$fishery_name # add back fishery_name
  #   ) |> 
  #   relocate(fishery_name)
  
  return(
#KB - the following 1 line was already commented out in the original "prep_dwg_interview" function
    #list(interviews = interviews, catches = catches, int_cat = int_cat) 
# KB   int_cat 
    interview_angler_types # KB addition
  )
}

