#Aggregate census (tie in) effort counts, associating to closest-in-time index count.

prep_dwg_effort_census_dev <- function(
    eff, 
    study_design,
    angler_type_kayak_pontoon = NA, 
    fish_location_determines_type = NA,
    boat_type_collapse = NA,
    ...){
  
  eff_cen <- dplyr::filter(eff, tie_in_indicator == 1) #Filter for effort census (aka tie-in) data
  eff_ind <- dplyr::filter(eff, tie_in_indicator == 0) #Filter for effort index data
  
  if(nrow(eff_cen) == 0){cat("ATTENTION: No effort census data collected and/or entered into the creel database \n")}

  if(str_detect(study_design, "tandard" )){

      census_angler_groups<-
        dplyr::left_join(
          #census values of interest...
          dplyr::select(eff_cen, section_num, event_date, tie_in_indicator, count_type, count_quantity)
          ,
          #...get reassigned count_seq from closest index
          #this nested join is typically expanding, as multiple index counts may match each census section & date
          #then the slice_min & distinct cut back down to a single value to reassign to above
          dplyr::left_join(
            dplyr::distinct(eff_cen, section_num, event_date, tie_in_indicator, effort_start_time, count_sequence),
            dplyr::distinct(eff_ind, section_num, event_date, tie_in_indicator, effort_start_time, count_sequence),
            by = c("section_num", "event_date"),
            suffix = c("_cen", "_ind")
          ) |>
            dplyr::group_by(section_num, event_date) |>
            dplyr::slice_min(abs(effort_start_time_cen - effort_start_time_ind), n = 1) |>
            dplyr::ungroup() |>
            dplyr::distinct(section_num, event_date, count_sequence = count_sequence_ind)
          ,
          by = c("section_num", "event_date")
        ) |>
          dplyr::mutate(
            angler_final = 
              dplyr::case_when(
                count_type %in% c("Boat", "Boat Anglers") ~ "boat",
                count_type %in% c("Bank", "Bank Anglers") ~ "bank",
                
                # #expecting future mods to allow position rather than craft type
                # #https://dfw-fp-r5.s3.us-west-2.amazonaws.com/data-dictionaries/creel_data_dictionary.html#effort_count_type_lut
                # count_type == c("Boat - Motor") ~ "boat",
                # count_type == c("Boat - Drift Raft") ~ "boat",
                # count_type == c("Shore - Drift/Raft") ~ "boat", #possibly subject to mod along with interview case_when
                # count_type == c("Shore - Motor Boat") ~ "boat", #possibly subject to mod along with interview case_when
                # 
                # count_type == c("Boat - Pontoon/Kick/Kayak") ~ "bank",
                # count_type == c("Shore - No Boat") ~ "bank",
                # count_type == c("Shore - Pontoon/Kick/Kayak") ~ "bank"
                
                stringr::str_detect(fish_location_determines_type, "Y") & stringr::str_detect(count_type, "Shore") ~ "bank",
                stringr::str_detect(fish_location_determines_type, "Y") & stringr::str_detect(count_type, "Boat - D|Boat - M|Boat - P") ~ "boat",
                
                stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(count_type, "Motor|Large") ~ "boat", 
                stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(count_type, "Drift|Raft")  ~ "boat",
    
                stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(count_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") ~ "boat",
                stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(count_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "ank") ~ "bank",
    
                stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(count_type, "No Boat") ~ "bank",
                TRUE ~ "fail"
    
            ),
            angler_final_int = as.integer(factor(angler_final))
          )  #filter( event_date == "2023-04-15")  |> print(n = Inf)  #|> count(angler_final)
#KB       tidyr::drop_na(angler_final) |> #KB - i don't think this is needed now that I've added TRUE ~ "fail"
      
    census_angler_final<-
      census_angler_groups |>
      filter(angler_final != "fail") |> 
      dplyr::group_by(section_num, event_date, tie_in_indicator, count_sequence, angler_final, angler_final_int) |>
      dplyr::summarize(count_census = sum(count_quantity), .groups = "drop") |>
      dplyr::arrange(section_num, event_date, count_sequence) |>
      mutate(
        fishery_name = params$fishery_name # add back fishery_name
      ) |>
      relocate(fishery_name)
    
   
    
  }else if(study_design == "Drano"){
  
    cat("NOTE: Per Drano study design, index effort counts were assumed to be census counts of banks anglers and boat vessels.  
        Thus, the code used in the above function 'prep_dwg_effort_index_dev' was duplicated to create the object 'census_angler_final' where the field count_index was renamed as 'count_census'.
        Put another way, the effort index data set was duplicated to generate an effort census data set." )

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
    
    census_angler_groups<-index_angler_groups
    census_angler_final<-index_angler_final |> rename(count_census = count_index) |> relocate(count_census, .after = angler_final_int)
    
    
  }

  return(list(census_angler_groups = census_angler_groups, census_angler_final = census_angler_final))
 
}
