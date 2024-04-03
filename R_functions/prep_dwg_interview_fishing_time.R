#creates angler_final categorical and calcs fishing_time & total time conditional on user choice of angler vs group counts
#joins catch values (wide)

#section_num of fishing_loc is assigned as Section whether or not interview_loc present
#when no fishing_loc, then use section_num of interview_loc

prep_dwg_interview_fishing_time <- function(
    dwg_interview, 
#KB    dwg_catch, 
    person_count_type = NA, #string passed from params controlling angler_count vs total_group_count 
    min_fishing_time,  #numeric passed from params to filter fishing_time at least as long as this per-person duration
    study_design, 
#KB    est_catch_groups, #data.fram passed from params of possibly-aggregated catch groups of interest to estimate
    ...){
  
# KB - commented out following 3 lines, which will be moved to another interview wrangling function    
  
  # if(any(is.na(c(dwg_interview$vehicle_count, dwg_interview$trailer_count)))){ 
  #   cat("Interview data have NA values for vehicle/trailer_count fields")      
  # } 

# KB - commented out following 3 lines, which will be moved to another interview wrangling function      
  
  # #coerce missing values to actual strings to allow params$est_catch_groups to include NAs alongside non-NA 
  # #to allow 'run' specification in params, add 'run' within across() 
  # dwg_catch <- dwg_catch |> mutate(across(c(species, life_stage, fin_mark, fate), ~replace_na(as.character(.), "NA")))
    
  
  
#KB if(person_count_type == "group"){
#KB   dwg_interview$person_count_final <- dwg_interview$total_group_count
#KB }else if (person_count_type == "angler"){
#KB  dwg_interview$person_count_final <- dwg_interview$angler_count
#KB }
#KB NOTE: we should highlight this detail in the protocol/study design & analysis outline document - "person_count_final" MUST match what was counted during census counts
  if(str_detect(study_design, "tandard" )){
    dwg_interview$person_count_final <- dwg_interview$total_group_count
  }else if(study_design == "Drano"){
    dwg_interview$person_count_final <- dwg_interview$angler_count
  }
  
#KB  interviews <- 
  interview_fishing_time <- # KB additions
    dwg_interview |> 
#KB    filter(is.na(angler_type) | str_detect(angler_type, "ank|oat")) |> 
    dplyr::mutate(
      trip_status = replace_na(trip_status, "Unknown"),

# KB - commented out following 17 lines, which will be moved to another interview wrangling function    
      
      # angler_final = dplyr::case_when( #KB
      #   tolower(angler_type) == "boat" ~ "boat", #KB
      #   tolower(angler_type) == "bank" ~ "bank", #KB
      # 
      #   boat_used == "No" ~ "bank", #KB
      #   boat_used == "Yes" & #KB
      #     stringr::str_detect(boat_type, "ontoon|ayak") & #KB
      #     (is.na(fish_from_boat) | fish_from_boat == "Bank") ~ "bank", #KB
      #   #allow kayaks as "boat": #KB
      #   boat_used == "Yes" & stringr::str_detect(boat_type, "ontoon|ayak") & fish_from_boat == "Boat" ~ "boat", # EB if kayaks that fished from boat are left NA, they will break the code for BSS #KB
      #   boat_used == "Yes" & #KB
      #     !stringr::str_detect(boat_type, "ontoon|ayak") & #KB
      #     (is.na(fish_from_boat) | fish_from_boat == "Boat") ~ "boat", #KB
      #   boat_used == "Yes" & !stringr::str_detect(boat_type, "ontoon|ayak") & #KB
      #     fish_from_boat == "Bank" ~ "boat" # EB anglers who used a non- "ontoon|ayak" boat but primarily fished from shore #KB
      #   ),
      # angler_final_int = as.integer(factor(angler_final)), #KB
      
      end_time_final = 
        dplyr::if_else(
          trip_status == "Incomplete" | is.na(fishing_end_time),
          interview_time,
          fishing_end_time
        ),
      fishing_time = round(as.numeric(end_time_final - fishing_start_time) / 3600, 5),

# KB - these 6 lines were already commented out in the original "prep_dwg_interview" function

      # ,
      # 
      # person_count_final = case_when(
      #   person_count_type == "group" ~ total_group_count,
      #   person_count_type == "angler" ~ angler_count # EB this fails when using the "angler" person_count_type option
      # ),
      vehicle_count = if_else(vehicle_count > person_count_final, person_count_final, vehicle_count), #KB - I don't understand why this line of code is here; seems like a QAQC update but not sure
      fishing_time_total = fishing_time * person_count_final
    ) |>
    dplyr::filter(fishing_time >= min_fishing_time) # |> # KB - commented out the pipe here
    
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
    #list(interviews = interviews, catches = catches, int_cat = int_cat) #KB (this was already commented out)
# KB    int_cat 
    interview_fishing_time # KB addition
    )
}

