#Aggregate census (tie in) effort counts, associating to closest-in-time index count.

prep_effort_census <- function(eff, ...){
  eff_cen <- dplyr::filter(eff, tie_in_indicator == 1)
  eff_ind <- dplyr::filter(eff, tie_in_indicator == 0)
  
  dplyr::left_join(
    #census values of interest...
    dplyr::select(eff_cen, section_num, location, event_date, tie_in_indicator, count_type, count_quantity)
    ,
    #...get reassigned count_seq from closest index
    #this nested join is typically expanding, as multiple index counts may match each census section & date
    #then the slice_min & distinct cut back down to a single value to reassign to above
    dplyr::left_join(
      dplyr::distinct(eff_cen, section_num, location, event_date, tie_in_indicator, effort_start_time, count_sequence),
      dplyr::distinct(eff_ind, section_num, event_date, tie_in_indicator, effort_start_time, count_sequence),
      by = c( "section_num", "event_date"),
      suffix = c("_cen", "_ind")
    ) |>
      dplyr::group_by(section_num, event_date, location) |>
      dplyr::slice_min(abs(effort_start_time_cen - effort_start_time_ind), n = 1) |>
      dplyr::ungroup() |>
      dplyr::distinct(section_num, location, event_date, count_sequence = count_sequence_ind)
    ,
    by = c("section_num", "location", "event_date")
  ) |>
    dplyr::mutate(
      angler_type = dplyr::case_when(
        stringr::word(count_type, 1) %in% c("Bank","bank","Shore") ~ "bank",
        stringr::word(count_type, 1) %in% c("Boat","boat") ~ "boat" #Note "Boats" plural intentionally excluded
      )
    ) |>
    tidyr::drop_na(angler_type) |> 
    dplyr::group_by(section_num, event_date, tie_in_indicator, count_sequence, angler_type) |>
    dplyr::summarize(count_census = sum(count_quantity), .groups = "drop") |>
    dplyr::arrange(section_num, event_date, count_sequence)
  
}
