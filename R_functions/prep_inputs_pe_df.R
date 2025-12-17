# calculate degrees of freedom based on number of creel survey days

prep_inputs_pe_df <- function(
    angler_hours_daily_mean,
    ...
){
  angler_hours_daily_mean |>  #KB addition
    dplyr::count(section_num, period, day_type, angler_final, name = "n_days_samp") |> 
    dplyr::group_by(section_num, angler_final) |>
    dplyr::mutate(
      df = (min(n_days_samp - 1) + sum(n_days_samp))/2
      ) |> 
    dplyr::ungroup()
  
}
