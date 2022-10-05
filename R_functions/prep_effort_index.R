#Aggregates index effort counts over locations within count_seq & section

#MAYBE MOVE MUTATE TO LATER TO ALLOW SUMMARIZATION SHARED WITH BSS
#WHICH DOES NOT WANT TRAILERS/VEHICLES COERCED TO boat/total
prep_effort_index <- function(eff, ...){
  
  eff |> dplyr::filter( 
    tie_in_indicator == 0,
    is.na(no_count_reason),
    !is.na(count_type)
    ) |>
    dplyr::group_by(section_num, event_date, count_sequence, count_type) |>
    dplyr::summarise(count_index = sum(count_quantity), .groups = "drop") |> 
    dplyr::arrange(section_num, event_date, count_sequence)
  
}
