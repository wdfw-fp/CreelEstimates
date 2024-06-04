#Aggregates index effort counts over locations within count_seq & section
#Note summarize() does not account for missed locations with date-section-sequence 
prep_dwg_effort_index_baker <- function(eff, ...){
  
  eff |> dplyr::filter( 
    tie_in_indicator == 0,
    is.na(no_count_reason),
    !is.na(count_type)
    ) |>
    dplyr::group_by(section_num, event_date, count_sequence, count_type) |>
    dplyr::summarise(count_index = sum(count_quantity), .groups = "drop") |> 
    tidyr::pivot_wider(names_from = count_type, values_from = count_index, values_fill = 0) |> # **start of Baker specific modification accounting for moored boats
    dplyr::mutate(
      `Trailers Only` = `Trailers Only` - Boats,
      `Trailers Only` = if_else(`Trailers Only` < 0, 0, `Trailers Only`) # "Zero-out" negative values for trailers where boats > trailers, following previous year's methods. Consult study design in off-season. 
    ) |>
    tidyr::pivot_longer(cols = c(`Trailers Only`,Boats), names_to = "count_type", values_to = "count_index") |> 
    dplyr::filter(!count_type %in% c("Boats")) |> # Baker specific modification accounting for moored boats **stop of Baker specific mods  
    dplyr::arrange(section_num, event_date, count_sequence) |> 
    mutate(
      fishery_name = params$fishery_name # add back fishery_name
    ) |> 
    relocate(fishery_name)
  
}
