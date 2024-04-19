# calculate daily cpue by section, date (period, day_type), angler_final, and catch group (aka est_cg) using the Ratio-of-Means (rom) calculation 
# NOTE: the Ratio-of-Mean cpue calculation inherently weights angler groups with longer fishing times as opposed to Mean-of-Ratio (mor) calculation which...
# ... gives equal weight to every interview 

prep_inputs_pe_daily_cpue_catch_est <- function(
    days, #tibble with time strata and closure fields
    dwg_summarized, #list with shared interview, index and census tibbles
    angler_hours_daily_mean, #tibble of daily mean angler hours by event_date, angler_type, and section_num
    ...
){
  
  dplyr::left_join(
    dwg_summarized$interview
    , 
    days |> dplyr::select(event_date, day_type, period, day_length)
    ,
    by=c("event_date")
  ) |> 
    dplyr::group_by(section_num, period, day_type, event_date, angler_final, est_cg) |>
    dplyr::summarise(
      n_obs = n(),
      total_catch = sum(fish_count),
      total_hours = sum(fishing_time_total),
      cpue_rom_daily = total_catch / total_hours,
      .groups = "drop"
      ) |> 
    dplyr::full_join(
      angler_hours_daily_mean, 
      by = c("section_num", "period", "day_type", "event_date", "angler_final")
    ) |>
    tidyr::drop_na(ang_hrs_daily_mean_TI_expan) |> 
    dplyr::mutate(
      catch_estimate = round(cpue_rom_daily * ang_hrs_daily_mean_TI_expan, 3)
      ) |>
    dplyr::arrange(section_num, event_date, angler_final, est_cg)
  
}
