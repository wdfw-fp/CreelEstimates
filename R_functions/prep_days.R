prep_days <- function(
    date_begin, date_end,
    weekends = c("Saturday", "Sunday"),
    holidays,
    Lat, Long,
    mod_per,
    closures,
    ...){
  
  tibble::tibble(
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
    ModelPeriod = mod_per,
    time_strata = dplyr::case_when(
      ModelPeriod == "Week" ~ Week,
      ModelPeriod == "Month" ~ Month,
      ModelPeriod == "Duration" ~ double(1)
    )
  ) |>
    tibble::rowid_to_column(var = "day_index") |>
    dplyr::left_join(closures, by = "event_date")
  
}
