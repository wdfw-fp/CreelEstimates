prep_inputs_pe_ang_hrs <- function(
    days,                       # tibble with time strata and closure fields
    dwg_summarized,             # list with shared interview, effort index and effort census tibbles
    interview_ang_per_object,   # tibble (from list) that has summarized anglers per count_type object by angler_final 
    paired_census_index_counts, # tibble (from list) that has summarized tie-in (aka census) count expansion factors by section & angler_final
    study_design,               # parameter specifying study design and which if/else loop gets called below
    ...
    ){
  
  if(str_detect(study_design, "tandard" )){ 

      effort_index_daily_mean <-
        dwg_summarized$effort_index |>
        dplyr::group_by(section_num, event_date, angler_final) |>
        dplyr::summarise(count_index_mean = mean(count_index, na.rm = TRUE), .groups = "drop")|>
        arrange(event_date, section_num) # |>

      angler_hours_daily_mean <-
        effort_index_daily_mean |>
        left_join(days |> dplyr::select(event_date, day_type, period, day_length), by=c("event_date")) |>
        left_join(interview_ang_per_object |> select(angler_final, ang_per_object), by=c("angler_final"))|>
        dplyr::ungroup() |>
        mutate(
          angler_hours_daily_mean = ang_per_object * count_index_mean * day_length
        ) |>
        select(-day_length, -ang_per_object, -count_index_mean) |>
        tidyr::drop_na(angler_hours_daily_mean) |>
        arrange(section_num, event_date)

    #now coerce back to angler_final bank/boat (unexpanded)
    if(any(angler_hours_daily_mean$angler_final=="boat")){
      angler_hours_daily_mean <-
        angler_hours_daily_mean |>
        tidyr::pivot_wider(
          names_from = angler_final,
          values_from = angler_hours_daily_mean,
        ) |>
        dplyr::mutate(
          boat = tidyr::replace_na(boat, 0),
          bank = total - boat,
          total = NULL
        ) |>
        tidyr::pivot_longer(
          cols = c(boat, bank),
          names_to = "angler_final",
          values_to = "angler_hours_daily_mean"
        ) |>
        mutate(
          angler_hours_daily_mean = ifelse(is.na(angler_hours_daily_mean) | angler_hours_daily_mean<0, 0, angler_hours_daily_mean) #KB addition (instead of filtering out NAs and negatives, I turned them to zero)
        )

    } else { #if no boat/trailer then total==bank
      angler_hours_daily_mean <-
        angler_hours_daily_mean |>
        dplyr::mutate(angler_final = "bank")
    }

  }else if(study_design == "Drano"){ #KB addition

  effort_index_daily_mean <-
    dwg_summarized$effort_index |>
    dplyr::group_by(section_num, event_date, angler_final) |>
    dplyr::summarise(count_index_mean = mean(count_index, na.rm = TRUE), .groups = "drop")|>
    arrange(event_date, section_num) # |>

  angler_hours_daily_mean <-
    effort_index_daily_mean |>
    left_join(days |> dplyr::select(event_date, day_type, period, day_length), by=c("event_date")) |>
    left_join(interview_ang_per_object |> select(angler_final, ang_per_object), by=c("angler_final"))|>
    dplyr::ungroup() |>
    mutate(
      angler_hours_daily_mean = ifelse(angler_final == "bank", count_index_mean * day_length, ang_per_object * count_index_mean * day_length)
    ) |>
    select(-day_length, -ang_per_object, -count_index_mean) |>
    tidyr::drop_na(angler_hours_daily_mean) |>
    arrange(section_num, event_date)

  }
  
  #now multiply mean daily effort in fishing_time by tie-in ratio bias term 
  #aiming for event_date, section_num, angler_final [total, boat, bank (as total-boat)]
  angler_hours_daily_mean_TI_expan <- 
    left_join(
      angler_hours_daily_mean
      , 
      paired_census_index_counts |> dplyr::select(section_num, angler_final, TI_expan_final)
      # census_TI_expan |> dplyr::select(section_num, angler_final, TI_expan_final)
      ,
      by = c("section_num", "angler_final")
    ) |>
    mutate(
      ang_hrs_daily_mean_TI_expan = angler_hours_daily_mean * TI_expan_final
    )

  #return(angler_hours_daily_mean)
  return(angler_hours_daily_mean_TI_expan)
}
