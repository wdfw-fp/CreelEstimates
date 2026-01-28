# Divide time strata (period) effort estimates into estimated daily values

est_pe_effort_daily <- function(
    days,
    pe_effort, 
    closures,
    ...
){
  
  # closures <- closures |> rename(section_num = section)
  
  pe_effort_daily <- pe_effort |> 
    left_join(days |>
                # dplyr::filter(!(event_date %in% closures$event_date)) |> 
                dplyr::select(event_date, day, day_type, day_length, week, month, year, period,
                              fishery_day_index = day_index, fishery_week_index = week_index, fishery_month_index = month_index)
              , by = c("period", "day_type")) |>  # expanding left join adding individual dates (event_date) to each period
    anti_join(closures, by = c("section_num", "event_date")) |>  # filter to remove the section-day combinations that were closed
    rename(est_effort_period = est, est_effort_daily = ang_hrs_mean) |> 
    arrange(event_date, section_num, angler_final, day_type)
  
  return(pe_effort_daily)
  
}