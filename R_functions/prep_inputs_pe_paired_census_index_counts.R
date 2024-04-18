# Summarize paired census and index angler effort counts 

prep_inputs_pe_paired_census_index_counts <- function(
    days,                     # tibble with time strata and closure fields
    dwg_summarized,           # list with shared interview, index and census tibbles
    interview_ang_per_object, # tibble of interview-based values to translate vehicle/trailer counts to boat/bank
    census_expan,             # tibble summarizing p_census by angler_final and section_num where p_census is a hard coded value in the database specifying the proportion of a a section that is covered during a census count; values less than 1 with result in census counts being expanded (e.g., census count divided by p_census)
    study_design,             # parameter specifying study design and which if/else loop gets called below
    ...
){

if(str_detect(study_design, "tandard" )){ 

  if(nrow(dwg_summarized$effort_census) == 0) {
    census_TI_expan <- 
      expand_grid(
        section_num = dwg_summarized$effort_index |> distinct(section_num) |> pull(), 
        angler_final = dwg_summarized$effort_index |> distinct(angler_final)|> pull(),
        TI_expan_final = 1 # If no census counts counted, this hard codes the spatial expansion to be 1 (i.e., assumes bias parameter is 1); should revisit later
      )
  } else {
    #begin census expansion values object by joining census and index in terms of total & boat
    census_TI_expan <- 
      dplyr::left_join(
      #census already grouped & summed by event_date, section_num, tie_in_indicator, count_sequence, and angler_final 
      #but as for interview above, first split and collapse to reassign angler_final as total & boat
        bind_rows(
          dwg_summarized$effort_census |>
            dplyr::group_by(section_num, event_date, count_sequence) |>
            dplyr::summarize(angler_final = "total", count_census = sum(count_census),  .groups = "drop")
          ,
          dwg_summarized$effort_census |>
            dplyr::filter(angler_final == "boat") |>
            dplyr::group_by(section_num, event_date, count_sequence) |>
            dplyr::summarize(angler_final = "boat", count_census = sum(count_census), .groups = "drop")
        ),
        dwg_summarized$effort_index |> select(-fishery_name, -angler_final_int)
        ,
        by = c("section_num", "event_date", "count_sequence", "angler_final")
      ) |> 
      tidyr::drop_na(count_index) |> 
      left_join(interview_ang_per_object |> select(angler_final, ang_per_object), by=c("angler_final"))|>
      dplyr::ungroup() |>
      mutate(
        count_index_expand = ang_per_object * count_index 
      ) |> 
      select(-count_index, -ang_per_object)  |> 
      rename(count_index = count_index_expand)
    
    #now overwrite, coercing angler_final back to bank/boat as above for pe_estimates$angler_hours_daily_mean
    #again dropping NAs and negatives as invalid for inferring estimates
    if(any(census_TI_expan$angler_final=="boat")){
      census_TI_expan <- 
        census_TI_expan |> 
        tidyr::pivot_longer(
          cols = c(count_census, count_index),
          names_to = "count_type",
          values_to = "count"
        ) |> 
        tidyr::pivot_wider(
          names_from = angler_final,
          values_from = count
        ) |>
        dplyr::mutate(
          boat = replace_na(boat, 0), # NA's here are implicit 0's due to lack of boat anglers in data 
          bank = total - boat, 
          total = NULL
        ) |> 
        tidyr::pivot_longer(
          cols = c(boat, bank),
          names_to = "angler_final",
          values_to = "count"
        ) |>
        tidyr::pivot_wider(
          names_from = count_type,
          values_from = count
        ) |>
        dplyr::mutate( 
          count_index = if_else(count_index < 0 , 0, count_index) 
          # need derived negative count_index estimates of bank anglers to pass through subsequent filter, 
          # if count_index doesn't pass filter then TI_expan is NA. Should only be an issue in very data limited situations
          # if not addressed, then effort from index counts with no TI_expan value get dropped from final estimates 
        ) |> 
        dplyr::filter(
          !is.na(count_census),
          !is.na(count_index),
          count_census >= 0,
          count_index >= 0
        )
    } else {
      census_TI_expan <- census_TI_expan |> mutate(angler_final = "bank")
    }
    
    census_TI_expan <- 
      census_TI_expan |> 
      dplyr::group_by(section_num, angler_final) |>
      dplyr::summarise(
        dplyr::across(c(count_census, count_index), sum),
        .groups = "drop"
      ) |>
      dplyr::left_join(census_expan, by = c("section_num", "angler_final")) |>
      dplyr::mutate(
        TI_expan_weighted = count_census / count_index,
        TI_expan_weighted = if_else(
          is.infinite(TI_expan_weighted) | TI_expan_weighted == 0,
          1,
          TI_expan_weighted
        ) |> replace_na(1),
        TI_expan_final = TI_expan_weighted / p_census
      )
    
  }
}else if(study_design == "Drano"){ 
# NOTE: Drano study design was set up such that effort was censused during each count thus pairing index and census counts not applicable; 
# NOTE: assumed each effort count was a census (i.e., TI_expan_final = 1 for all angler_types and sections)
  
  census_TI_expan <- 
    expand_grid(
      section_num = dwg_summarized$effort_index |> distinct(section_num) |> pull(), 
      angler_final = dwg_summarized$effort_index |> distinct(angler_final)|> pull(),
      TI_expan_final = 1
    )

}  
  return(census_TI_expan)
}
