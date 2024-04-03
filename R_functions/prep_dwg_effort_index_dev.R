#Aggregates index effort counts over locations within count_seq & section
#Note summarize() does not account for missed locations with date-section-sequence 
prep_dwg_effort_index_dev <- function(
    eff, 
    study_design,
    boat_type_collapse = NA,
    fish_location_determines_type = NA,
    angler_type_kayak_pontoon = NA, 
    ...){
  
if(str_detect(study_design, "tandard" )){ #KB addition  
  
  index_angler_groups<- #KB addition
    eff |>
    dplyr::filter(
      tie_in_indicator == 0,
      is.na(no_count_reason),
      !is.na(count_type)
    ) |>
    dplyr::mutate(
      angler_final = 
        dplyr::case_when(
          count_type == "Trailers Only" ~ "boat",
          count_type == "Vehicle Only" ~ "total",
          TRUE ~ "fail"
    )
) #KB addition

  index_angler_final<-  #KB addition
    index_angler_groups |>  #KB addition
    #KB    dplyr::group_by(section_num, event_date, count_sequence, count_type) |>
    dplyr::group_by(section_num, event_date, count_sequence, count_type, angler_final) |> #KB addition 
    dplyr::summarise(count_index = sum(count_quantity), .groups = "drop") |>
    dplyr::arrange(section_num, event_date, count_sequence) |>
    mutate(
      fishery_name = params$fishery_name # add back fishery_name
    , angler_final_int = as.integer(factor(angler_final)) #KB addition
    ) |>
    relocate(fishery_name)
  
}else if(study_design == "Drano"){ #KB addition

  index_angler_groups<- #KB addition 
    eff |> 
    dplyr::filter( 
      tie_in_indicator == 0,
      is.na(no_count_reason),
      !is.na(count_type)
    ) |>
    dplyr::mutate(
      angler_final = 
        dplyr::case_when(
  
          stringr::str_detect(count_type, "Shore") ~ "bank",
          
          stringr::str_detect(count_type, "Motor") & stringr::str_detect(boat_type_collapse, "Y") ~ "boat",
          stringr::str_detect(count_type, "Motor") & stringr::str_detect(boat_type_collapse, "N") ~ "boat_motor",
          
          stringr::str_detect(count_type, "Skiff|Pram") & stringr::str_detect(boat_type_collapse, "Y") ~ "boat",
          stringr::str_detect(count_type, "Skiff|Pram") & stringr::str_detect(boat_type_collapse, "N") ~ "boat_skiff",
          
          stringr::str_detect(count_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") & stringr::str_detect(boat_type_collapse, "Y") ~ "boat",
          stringr::str_detect(count_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") & stringr::str_detect(boat_type_collapse, "N") ~ "boat_single",
          stringr::str_detect(count_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "ank")  ~ "bank", 
          TRUE ~ "fail"
        )
    ) #KB addition

  index_angler_final<-  #KB addition 
    index_angler_groups |>  #KB addition 
#KB    dplyr::group_by(section_num, event_date, count_sequence, count_type) |>
    dplyr::group_by(section_num, event_date, count_sequence, angler_final) |> #KB addition
    dplyr::summarise(count_index = sum(count_quantity), .groups = "drop") |> 
    dplyr::arrange(section_num, event_date, count_sequence) |> 
    mutate(
      fishery_name = params$fishery_name # add back fishery_name
    , angler_final_int = as.integer(factor(angler_final)) #KB addition
    ) |> 
    relocate(fishery_name)
  
}  #KB addition 
  
  return(list(index_angler_groups = index_angler_groups, index_angler_final = index_angler_final))  
  
}
