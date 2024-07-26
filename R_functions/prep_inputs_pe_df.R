prep_inputs_pe_df <- function(
    days,
    angler_hours_daily_mean,
    ...
){
  
  dplyr::left_join(
    angler_hours_daily_mean
    , 
    days |> dplyr::select(event_date, period)
    ,
    by = "event_date"
    ) |> 
    dplyr::count(section_num, period, day_type, angler_final, name = "n_days_samp") |> 
    dplyr::group_by(section_num, angler_final) |>
    dplyr::mutate(
      df = (min(n_days_samp - 1) + sum(n_days_samp))/2
      ) |> 
    dplyr::ungroup()
  
}
