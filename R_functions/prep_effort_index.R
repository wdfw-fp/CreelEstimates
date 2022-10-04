#Aggregates index effort counts over locations within count_seq & section
#mutate angler_type here creates a later join-by column

prep_effort_index <- function(eff, days, ...){
  
  dplyr::filter(eff, 
                tie_in_indicator == 0,
                is.na(no_count_reason),
                !is.na(count_type)
  ) |>
    dplyr::left_join(days |> select(event_date, DayType), by = "event_date") |> 
    dplyr::group_by(section_num, DayType, event_date, count_sequence, count_type) |>
    dplyr::summarise(count_index = sum(count_quantity), .groups = "drop") |>
    dplyr::mutate(
      angler_type = dplyr::case_when(
        count_type == "Boat Anglers" ~ "boat",
        count_type == "Bank Anglers" ~ "bank",
        count_type == "Trailers Only" ~ "boat",
        count_type == "Vehicle Only" ~ "total"
      )
    ) |> 
    dplyr::arrange(section_num, event_date, count_sequence)
  
}
