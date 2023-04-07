
prep_days <- function(
    date_begin, date_end,
    weekends = c("Saturday", "Sunday"),
    holidays, #date/char vector of YYYY-MM-DD dates to categorize as "weekend" strata
    lat, long,
    period_pe,
    sections, #numeric vector of all possible sections to estimate
    closures, #tibble of fishery_name, section number and date of closures
    day_length,
    day_length_inputs,
    ...){
  
  date_begin <- as.Date(date_begin, format="%Y-%m-%d")
  date_end <- as.Date(date_end, format="%Y-%m-%d")
  holidays <- as.Date(holidays, format="%Y-%m-%d")
  
  # create tibble with dates and time period strata 
  
  days <- tibble::tibble(
    event_date = seq.Date(date_begin, date_end, by = "day"),
    day = weekdays(event_date),
    day_type = if_else(day %in% weekends | event_date %in% holidays, "weekend", "weekday"),
    day_type_num = as.integer(c("weekend" = 1, "weekday" = 0)[day_type]),  #if_else(str_detect(day_type, "end"), 1, 0),
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

# section calculating day length, using param option day_length_expansion
# the manual options allows the user to select fixed times or sunrise / sunset with offsets for either start or end times
# the alternative options are "night closure", "dawn/dusk", "sunrise/sunset"
  if(day_length == "manual"){
  
    # If manual start and/or end times enter, specify "ui_start_time" and "ui_end_time" in case they were left blank
    # if( day_length_inputs$start_time == "manual"){day_length_inputs$start_time<-c("sunrise")}
    # if( day_length_inputs$end_time == "manual")  {day_length_inputs$end_time<-c("sunset")} 
    
    # Day length based on sunrise/sunset 
      
    day_length_values <- 
      tibble(
        getSunlightTimes(
          keep=c("sunrise", "sunset"), 
          date=days$event_date, 
          lat = lat, 
          lon= long,
          tz = "America/Los_Angeles"
        )
      ) |> 
      select(-lat, -lon)
    
    # Start times using sunrise + offset 
    if(day_length_inputs$start_time != "manual"){
      day_length_values <- 
        day_length_values |> 
        mutate(
          start_date_time = sunrise - (60*60*day_length_inputs$start_adj)
          )
    }else{ # or manually specified time
      day_length_values <- 
        day_length_values |> 
        mutate(
          start_date_time = as.POSIXct(paste(day_length_values$date, day_length_inputs$start_manual), format = "%Y-%m-%d %H:%M:%S") 
        )
    }
    
    # End times using sunset + offset 
    if(day_length_inputs$end_time != "manual"){
      day_length_values <- 
        day_length_values |> 
        mutate(
          end_date_time = sunset + (60*60*day_length_inputs$end_adj)
        )
    }else{ # or manually specified time
      day_length_values <- 
        day_length_values |> 
        mutate(
          end_date_time = as.POSIXct(paste(day_length_values$date, day_length_inputs$end_manual), format = "%Y-%m-%d %H:%M:%S") 
        )
    }
    
    # calculate day length from start and end times from above 
    day_length_values <- day_length_values |> 
      mutate(
        day_length = as.numeric(end_date_time - start_date_time)
      ) |> 
      select(event_date = date, day_length)
    
    
    # 
  }else{
    day_length_values <- suncalc::getSunlightTimes(
      date = days$event_date,
      tz = "America/Los_Angeles",
      lat = lat, lon = long,
      keep=c("sunrise", "sunset", "dawn", "dusk")
    ) |> 
      select(event_date = date, sunrise, sunset, dawn, dusk) |> 
      mutate(
        day_length_dawn_dusk = as.numeric((dusk) - (dawn)),
        day_length_sunrise_sunset = as.numeric((sunset) - (sunrise)),
        day_length_night_closure = as.numeric((sunset + 3600) - (sunrise - 3600)),
      ) |> 
      mutate(
        day_length = case_when(
          day_length == "dawn/dusk" ~ day_length_dawn_dusk,
          day_length == "sunrise/sunset" ~ day_length_sunrise_sunset,
          day_length == "night closure" ~ day_length_night_closure
        )
      ) |> 
      select(event_date, day_length)
  }
  
  # join day_length to days tibble
  
  days <- days |> left_join(day_length_values, by = "event_date")

  # expanding join to incorporate closure dates 
  
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
  ) |> 
    mutate(
      fishery_name = params$fishery_name
    ) |> 
    relocate(fishery_name)
  
  return(days)
}


#event_date = seq.Date(as.Date(params$est_date_start), as.Date(params$est_date_end), by = "day")

