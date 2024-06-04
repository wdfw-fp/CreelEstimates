# Evaluate proportion interviewed hours by angler type compared to effort estimates by angler type

est_pe_prop_effort_sampled_by_angler_type <- function(
    days,
    dwg_summarized,
    estimates_pe_effort,
    ...
){
  
  dwg_summ$interview |>
    left_join(dwg$days |> dplyr::select(event_date, period, day_type), by = "event_date") |> 
    drop_na(angler_final) |>  
    group_by(period, section_num, day_type, angler_final) |> 
    summarise(
      interview_hours_total = sum(fishing_time_total)
    ) |> 
    left_join(estimates_pe$effort |> ## join expanded catch estimates in pe$est_effort_s_ts_dt_at to total angler hours from interviews in pe$interview 
                select(section_num, period, day_type, angler_final, effort_est = est) |>
                group_by(section_num, period, day_type, angler_final) |> 
                summarise(
                  angler_effort_total = sum(effort_est))
              , 
              by = c("section_num", "period", "day_type", "angler_final"), .groups = "drop") |> 
    mutate(
      angler_effort_total = if_else(is.nan(angler_effort_total), 0 , angler_effort_total) # coerce to zero rows where bank effort not estimable due
      # to method of obtaining bank anglers from total - boat, circle back on this with KB at some point
    ) |> 
    pivot_wider(names_from = angler_final, values_from = c("interview_hours_total", "angler_effort_total"), values_fill = 0) |>
    group_by(section_num) |>
    summarise(
      proportion_angler_effort_boat = sum(angler_effort_total_boat) / (sum(angler_effort_total_boat) + sum(angler_effort_total_bank)),
      proportion_interview_hours_boat = sum(interview_hours_total_boat) / (sum(interview_hours_total_boat) + sum(interview_hours_total_bank)),
      proportion_angler_effort_bank = sum(angler_effort_total_bank) / (sum(angler_effort_total_boat) + sum(angler_effort_total_bank)),
      proportion_interview_hours_bank = sum(interview_hours_total_bank) / (sum(interview_hours_total_boat) + sum(interview_hours_total_bank))) |>
    mutate(across(where(is.numeric), round, 2)) |>
    left_join(
      dwg$interview |> distinct(fishing_location, section_num),
      by = "section_num") |> 
    gt(groupname_col = "section_name") |> 
    tab_header(
      title = md("Proportion of angler hours from interviews and total effort estimates by angler type"),
      subtitle = md("Season-long proportions (prop.) of the sum total of angler hours by angler type (e.g., boat, bank) from total estimates of angler effort and angler interviews.")) |> 
    cols_label(
      proportion_angler_effort_boat = md("prop. boat angler hours in expanded effort estimates"),
      proportion_interview_hours_boat = md("prop. boat angler hours sampled from interviews"),
      proportion_angler_effort_bank = md("prop. bank angler hours in expanded effort estimates"),
      proportion_interview_hours_bank = md("prop. bank angler hours sampled from interviews")
    ) |>
    tab_options(container.overflow.x = T, container.overflow.y = T)
}
