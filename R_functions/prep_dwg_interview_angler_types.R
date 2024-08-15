#creates angler_final field based on study_design designation and user input values for boat_type_collapse, fish_location_determines_type, angler_type_kayak_pontoon

prep_dwg_interview_angler_types <- function(
    interview_fishing_time,             # output from preceding function that calculates fishing time of angler groups
    study_design,                       # string passed from params denoting which study design was followed during data collection
    boat_type_collapse = NA,            # string passed from params that controls whether all (potential) boat types (e.g., motor_boat, drift_boat) are collapsed (i.e., boat_type_collapse: "Yes") into a single boat type or kept separate (boat_type_collapse: "No"). 
    fish_location_determines_type = NA, # string passed from params that controls whether the observed fishing location for a given angler group during an effort count determines their angler type. 
    angler_type_kayak_pontoon = NA,     # string passed from params that controls whether a boat designated as a kayak, pontoon, or kick during an effort count or angler group interview should be designated as a boat or bank angler.
    ...){

if(str_detect(study_design, "tandard" )){ 

  interview_angler_types <- 
    interview_fishing_time |>  
    dplyr::mutate(
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
      , angler_final_int = as.integer(factor(angler_final)) 
    )
  
}else if(study_design == "Drano"){ 

  interview_angler_types <- 
    interview_fishing_time |>  
    dplyr::mutate(
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
      , angler_final_int = as.integer(factor(angler_final)) 
    ) 
} 
  return(interview_angler_types)
}

