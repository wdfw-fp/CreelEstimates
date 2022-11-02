#Aggregates index effort counts over locations within count_seq & section
#Note summarize() does not account for missed locations with date-section-sequence 
prep_dwg_effort_index <- function(eff, ...){
  
  eff |> dplyr::filter( 
    tie_in_indicator == 0,
    is.na(no_count_reason),
    !is.na(count_type)
    ) |>
    dplyr::group_by(section_num, event_date, count_sequence, count_type) |>
    dplyr::summarise(count_index = sum(count_quantity), .groups = "drop") |> 
    dplyr::arrange(section_num, event_date, count_sequence)
  
}
