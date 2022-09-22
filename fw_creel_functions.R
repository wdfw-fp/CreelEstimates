#### support functions for freshwater creel estimation based on data.wa.gov views

#### fetch_dwg ------------------------------

fetch_dwg <- function(fishery_name, ...){
  
  dwg_base <- list(
    #event = "https://data.wa.gov/resource/ui95-axtn.csv",
    effort = "https://data.wa.gov/resource/h9a6-g38s.csv",
    interview = "https://data.wa.gov/resource/rpax-ahqm.csv",
    catch = "https://data.wa.gov/resource/6y4e-8ftk.csv"
    #,gear = "https://data.wa.gov/resource/d2ks-afhz.csv" #currently unused?
  )
  
  dwg <- list()
  
  dwg$effort <- paste0(
    dwg_base$effort,
    "?$where=fishery_name='",
    fishery_name,
    "'&$limit=100000"
    ) |>
    utils::URLencode() |>
    readr::read_csv(show_col_types = F) |>
    dplyr::filter(!is.na(count_type)) |>
    dplyr::select(-created_datetime, -modified_datetime) #|> dplyr::inner_join(lu_input$sections, by = c("location"))
  
  dwg$interview <- paste0(
    dwg_base$interview,
    "?$where=fishery_name='",
    fishery_name,
    "'&$limit=100000"
  ) |>
    utils::URLencode() |>
    readr::read_csv(show_col_types = F) |>
    dplyr::select(
      -created_datetime, -modified_datetime,
      -state_residence, -zip_code) |>
    dplyr::mutate(
      location = if_else(is.na(interview_location), as.character(fishing_location), as.character(interview_location))
    ) #|> dplyr::inner_join(lu_input$sections, by = c("location"))
  
  dwg$catch <- paste0(
    dwg_base$catch,
    "?$where=fishery_name='",
    fishery_name,
    "'&$limit=100000"
  ) |> 
    utils::URLencode() |>
    readr::read_csv(show_col_types = F) |>
    dplyr::select(interview_id, catch_id, species, run, life_stage, fin_mark, fate, fish_count) |> 
    dplyr::mutate(
      catch_group = paste(species, life_stage, fin_mark, fate, sep = "_") # fish catch groups to estimate catch of 
    )
  
  return(dwg)
}

# #tests
# fetch_dwg("Naselle winter steelhead 2022")
# fetch_dwg("Skykomish summer Chinook 2022")

#### prep_days -------------------------------

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

#### prep_effort_census --------------
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

#### prep_effort_index -------------------------------
#Aggregates index effort counts over locations within count_seq & section
#mutate angler_type here creates a later join-by column

prep_effort_index <- function(eff, days, ...){
  
  dplyr::filter(eff, 
    tie_in_indicator == 0,
    is.na(no_count_reason),
    !is.na(count_type)
    ) |>
    dplyr::left_join(days |> select(event_date, DayType), by = "event_date") |> 
    dplyr::group_by(section_num, DayType, event_date, count_sequence, count_type) |>
    dplyr::summarise(count_index = sum(count_quantity), .groups = "drop") |>
    dplyr::mutate(
      angler_type = dplyr::case_when(
        count_type == "Boat Anglers" ~ "boat",
        count_type == "Bank Anglers" ~ "bank",
        count_type == "Trailers Only" ~ "boat",
        count_type == "Vehicle Only" ~ "total"
      )
    ) |> 
    dplyr::arrange(section_num, event_date, count_sequence)
  
}

#### function -------------------------------
#### function -------------------------------
#### function -------------------------------

