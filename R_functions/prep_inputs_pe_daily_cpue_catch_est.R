prep_inputs_pe_daily_cpue_catch_est <- function(
    days, #tibble with time strata and closure fields
    dwg_summarized, #list with shared interview, index and census tibbles
    angler_hours_daily_mean,
    ...
){
  
  dplyr::left_join(
    dwg_summarized$interview
    , 
    days |> dplyr::select(event_date, DayType, period, DayL)
    ,
    by=c("event_date")
  ) |> 
    dplyr::group_by(section_num, period, DayType, event_date, angler_final, est_cg) |>
    dplyr::summarise(
      total_catch = sum(fish_count),
      total_hours = sum(fishing_time_total),
      cpue_rom_daily = total_catch / total_hours,
      .groups = "drop"
      ) |> 
    dplyr::left_join(
      angler_hours_daily_mean, 
      by = c("section_num", "DayType", "event_date", "angler_final")
    ) |>
    tidyr::drop_na(ang_hrs_daily_mean_TI_expan) |> 
    dplyr::mutate(
      catch_estimate = round(cpue_rom_daily * ang_hrs_daily_mean_TI_expan, 3)
      ) |>
    dplyr::arrange(section_num, event_date, angler_final, est_cg)
  
}
