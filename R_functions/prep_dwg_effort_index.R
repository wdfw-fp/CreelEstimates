#Aggregates index effort counts over locations within count_seq & section
#Note summarize() does not account for missed locations with date-section-sequence 
prep_dwg_effort_index <- function(
    eff, 
    study_design,
    boat_type_collapse = NA,
    fish_location_determines_type = NA,
    angler_type_kayak_pontoon = NA, 
    ...){
  
#create intermediate object index_angler_groups that converts count_type objects to angler_final based on study design & user defined arguments (in YAML)
if(str_detect(study_design, "tandard" )){

  index_angler_groups<- 
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
) 

}else if(study_design == "Drano"){ 

  index_angler_groups<- 
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
    ) 
} 
# create final object output of interest index_angler_final that summarizes index count data by section_num, event_date, count_sequence, & angler_final  
  index_angler_final<-  
    index_angler_groups |>  
    dplyr::group_by(section_num, event_date, count_sequence, angler_final) |> #
    dplyr::summarise(count_index = sum(count_quantity), .groups = "drop") |> 
    dplyr::arrange(section_num, event_date, count_sequence) |> 
    mutate(
      fishery_name = params$fishery_name # add back fishery_name
      , angler_final_int = as.integer(factor(angler_final)) 
    ) |> 
    relocate(fishery_name)
  
  return(list(index_angler_groups = index_angler_groups, index_angler_final = index_angler_final))  
  
}
