prep_inputs_pe_ang_hrs_bank_boat <- function(
    days, #tibble with time strata and closure fields
    dwg_summarized, #list with shared interview, index and census tibbles
    census_expan,
    ...
    ){
  
  eff_ind <- dplyr::left_join(
    dwg_summarized$effort_index, 
    days |> dplyr::select(event_date, day_type, period, day_length),
    by=c("event_date")
    ) |>  
    dplyr::mutate(
      angler_final = dplyr::case_when(
        count_type == "Boat Anglers" ~ "boat",
        count_type == "Bank Anglers" ~ "bank",
      )
    )
  
  #census_TI_expan: left join effort index counts to census counts and expand by TI
  #count_census begins summed by event_date, section_num, tie_in_indicator, count_sequence, with angler_final in [bank, boat]
  #excluding date-section_num-anglers with missing (or negative) counts as invalid to support inferring point estimates
  census_TI_expan <- dplyr::left_join(
    dwg_summarized$effort_census,
    eff_ind,
    by = c("section_num", "event_date", "count_sequence", "angler_final")
  ) |>
    dplyr::filter(
      !is.na(count_census),
      !is.na(count_index),
      count_census >= 0,
      count_index >= 0
    ) |> 
    dplyr::group_by(section_num, angler_final) |>
    dplyr::summarise(
      dplyr::across(c(count_census, count_index), sum),
      .groups = "drop"
    ) |>
    dplyr::left_join(census_expan, by = c("section_num", "angler_final")) |>
    dplyr::mutate(
      TI_expan_weighted = count_census / count_index,
      TI_expan_weighted = if_else(
        is.infinite(TI_expan_weighted) | TI_expan_weighted == 0,
        1, 
        TI_expan_weighted
        ) |> replace_na(1),
      TI_expan_final = if_else(
        cen_exp_meth == "Direct",
        TI_expan_weighted / p_census,
        census_indir)
    )
  
  #initial angler_hours_daily_mean: join day length against mean counts over count_seqs per section_num-day-angler_final
  angler_hours_daily_mean <- eff_ind |> 
    dplyr::group_by(section_num, event_date, angler_final, DayL) |>
    dplyr::summarise(count_index_mean = mean(count_index), .groups = "drop") |>
    dplyr::mutate(angler_hours_daily_mean = count_index_mean * DayL) |>
    dplyr::arrange(section_num, event_date) |> 
    dplyr::left_join(census_TI_expan, by = c("section_num", "angler_final")) |>
    dplyr::mutate(
      ang_hrs_daily_mean_TI_expan = angler_hours_daily_mean * replace_na(TI_expan_final, 1)
    ) 
  
  return(angler_hours_daily_mean)
}
