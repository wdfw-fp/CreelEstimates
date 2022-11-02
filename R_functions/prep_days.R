
prep_days <- function(
    date_begin, date_end,
    weekends = c("Saturday", "Sunday"),
    holidays, #date/char vector of YYYY-MM-DD dates to categorize as "weekend" strata
    Lat, Long,
    period_pe,
    sections, #numeric vector of all possible sections to estimate
    closures, #tibble of fishery_name, section number and date of closures
    ...){
  
  date_begin <- as.Date(date_begin, format="%Y-%m-%d")
  date_end <- as.Date(date_end, format="%Y-%m-%d")
  holidays <- as.Date(holidays, format="%Y-%m-%d")
  
  days <- tibble::tibble(
    event_date = seq.Date(date_begin, date_end, by = "day"),
    Day = weekdays(event_date),
    DayType = if_else(Day %in% weekends | event_date %in% holidays, "Weekend", "Weekday"),
    DayType_num = as.integer(c("Weekend" = 1, "Weekday" = 0)[DayType]),  #if_else(str_detect(DayType, "end"), 1, 0),
    DayL = suncalc::getSunlightTimes(
      date = event_date,
      tz = "America/Los_Angeles",
      lat = Lat, lon = Long,
      keep=c("sunrise", "sunset")
    ) |>
      mutate(DayL = as.numeric((sunset + 3600) - (sunrise - 3600))) |>
      pluck("DayL"),
    #Monday to Sunday weeks, see ?strptime
    Week = as.numeric(format(event_date, "%W")),
    Month = as.numeric(format(event_date, "%m")),
    period = case_when(
      period_pe == "week" ~ Week,
      period_pe == "month" ~ Month,
      period_pe == "duration" ~ double(1)
    ),
    day_index = as.integer(seq_along(event_date)),
    week_index = as.integer(factor(Week, levels = unique(Week))),
    month_index = as.integer(factor(Month, levels = unique(Month)))
    )
  
  days <- left_join(
    days,
    dplyr::rows_update(
      tidyr::expand_grid(event_date = days$event_date, section = sections, open = TRUE)
      ,
      closures |> 
        filter(between(event_date, date_begin, date_end)) |> 
        dplyr::select(section, event_date) |> 
        dplyr::mutate(open = FALSE)
      ,
      by = c("section", "event_date")
      ) |> 
      arrange(section, event_date) |> 
      mutate(section = paste0("open_section_", section)) |> 
      pivot_wider(names_from = section, values_from = open)
    ,
    by = "event_date"
  )
  
  return(days)
}


#event_date = seq.Date(as.Date(params$est_date_start), as.Date(params$est_date_end), by = "day")

