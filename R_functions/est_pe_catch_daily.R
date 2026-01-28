# Divide time strata (period) catch estimates into estimated daily values

est_pe_catch_daily <- function(
    days,
    pe_catch, 
    closures,
    ...
){
  # closures <- closures |> rename(section_num = section)
  
  if(nrow(dwg$closures) == 0){
  
  pe_catch_daily <- pe_catch |> 
    left_join(days |>
                # dplyr::filter(!(event_date %in% closures$event_date)) |> 
                dplyr::select(event_date, day, day_type, day_length, week, month, year, period,
                              fishery_day_index = day_index, fishery_week_index = week_index, fishery_month_index = month_index)
              , by = c("period", "day_type")) |>  # expanding left join adding individual dates (event_date) to each period
    # anti_join(closures, by = c("section_num", "event_date")) |>  # filter to remove the section-day combinations that were closed
    rename(est_catch_period = est, est_catch_daily = catch_est_mean) |> 
    left_join(dwg$effort |>
                filter(location_type == "Section") |>
                mutate(section_num = as.numeric(section_num)) |> 
                distinct(water_body, section_num),
              by = c("section_num")) |>
    arrange(est_cg, event_date, section_num, angler_final, day_type)
  
  return(pe_catch_daily)
  
  }
  
  else if(nrow(dwg$closures) > 0){  
    
    pe_catch_daily <- pe_catch |> 
      left_join(days |>
                  # dplyr::filter(!(event_date %in% closures$event_date)) |> 
                  dplyr::select(event_date, day, day_type, day_length, week, month, year, period,
                                fishery_day_index = day_index, fishery_week_index = week_index, fishery_month_index = month_index)
                , by = c("period", "day_type")) |>  # expanding left join adding individual dates (event_date) to each period
      anti_join(closures, by = c("section_num", "event_date")) |>  # filter to remove the section-day combinations that were closed
      rename(est_catch_period = est, est_catch_daily = catch_est_mean) |> 
      left_join(dwg$effort |>
                  filter(location_type == "Section") |>
                  mutate(section_num = as.numeric(section_num)) |> 
                  distinct(water_body, section_num),
                by = c("section_num")) |>
      arrange(est_cg, event_date, section_num, angler_final, day_type)
    
    return(pe_catch_daily)  }
  
}