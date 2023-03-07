
prep_days <- function(
    date_begin, date_end,
    weekends = c("Saturday", "Sunday"),
    holidays, #date/char vector of YYYY-MM-DD dates to categorize as "weekend" strata
    lat, long,
    period_pe,
    sections, #numeric vector of all possible sections to estimate
    closures, #tibble of fishery_name, section number and date of closures
    ...){
  
  date_begin <- as.Date(date_begin, format="%Y-%m-%d")
  date_end <- as.Date(date_end, format="%Y-%m-%d")
  holidays <- as.Date(holidays, format="%Y-%m-%d")
  
  days <- tibble::tibble(
    event_date = seq.Date(date_begin, date_end, by = "day"),
    day = weekdays(event_date),
    day_type = if_else(day %in% weekends | event_date %in% holidays, "weekend", "weekday"),
    day_type_num = as.integer(c("weekend" = 1, "weekday" = 0)[day_type]),  #if_else(str_detect(day_type, "end"), 1, 0),
    day_length = suncalc::getSunlightTimes(
      date = event_date,
      tz = "America/Los_Angeles",
      lat = lat, lon = long,
      keep=c("sunrise", "sunset")
    ) |>
      mutate(day_length = as.numeric((sunset + 3600) - (sunrise - 3600))) |>
      pluck("day_length"),
    #Monday to Sunday weeks, see ?strptime
    week = as.numeric(format(event_date, "%W")),
    month = as.numeric(format(event_date, "%m")),
    year = as.numeric(format(event_date, "%Y")),
    period = case_when(
      period_pe == "week" ~ week,
      period_pe == "month" ~ month,
      period_pe == "duration" ~ double(1)
    ),
    day_index = as.integer(seq_along(event_date)),
    week_index = as.integer(factor(week, levels = unique(week))),
    month_index = as.integer(factor(month, levels = unique(month)))
    )
  
  days <- left_join(
    days,
    dplyr::rows_update(
      tidyr::expand_grid(event_date = days$event_date, section_num = sections, open = TRUE)
      ,
      closures |>
        mutate(
          event_date = as.Date(event_date, format="%Y-%m-%d"),
          section_num = as.double(section_num)
          ) |> 
        dplyr::filter(dplyr::between(event_date, date_begin, date_end)) |> 
        dplyr::select(section_num, event_date) |> 
        dplyr::mutate(open = FALSE)
      ,
      by = c("section_num", "event_date")
      ) |> 
      dplyr::arrange(section_num, event_date) |> 
      dplyr::mutate(section_num = paste0("open_section_", section_num)) |> 
      tidyr::pivot_wider(names_from = section_num, values_from = open)
    ,
    by = "event_date"
  )
  
  return(days)
}


#event_date = seq.Date(as.Date(params$est_date_start), as.Date(params$est_date_end), by = "day")

