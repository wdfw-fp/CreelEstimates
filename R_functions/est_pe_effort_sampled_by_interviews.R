# Evaluate proportion of estimated total angler hours (effort) sampled during interviews, grouped by angler type

# EB 7.24.2023 need to deal with repeating data over catch groups, right now multiplying total stratum fishing hours from interviews by n-number of catch groups 


est_pe_effort_sampled_by_interviews <- function(
    days,
    dwg_summarized,
    estimates_pe_effort,
    ...
){
  
  dwg_summ$interview |>
    distinct(interview_id, section_num, event_date, angler_final, fishing_time_total) |> # ensure only distinct interviews are evaluated, given potentially > 1 catch groups
    left_join(dwg$days |> dplyr::select(event_date, period, day_type), by = "event_date") |> 
    drop_na(angler_final) |>  
    group_by(period, section_num, day_type, angler_final) |> 
    summarise(
      interview_hours_total = sum(fishing_time_total)
    ) |> 
    left_join(estimates_pe$effort |> ## join expanded catch estimates in pe$est_effort_s_ts_dt_at to total angler hours from interviews in pe$interview 
                select(section_num, period, section_num, day_type, angler_final, effort_est = est) |>
                group_by(section_num, period, day_type, angler_final) |> 
                summarise(
                  angler_effort_total = sum(effort_est))
              , 
              by = c("section_num", "period", "day_type", "angler_final")) |>
    mutate(
      angler_effort_total = if_else(is.nan(angler_effort_total), 0 , angler_effort_total) # coerce to zero rows where bank effort not estimable due
      # to method of obtaining bank anglers from total - boat, circle back on this with KB at some point
    ) |> 
    group_by(section_num, angler_final) |> 
    summarise(
      interview_hours_total = sum(interview_hours_total),
      angler_effort_total = sum(angler_effort_total),
      proportion_interviewed_effort = (interview_hours_total / angler_effort_total)
    ) |> 
    gt(groupname_col = "section_name") |> 
    fmt_number(c(interview_hours_total, angler_effort_total, proportion_interviewed_effort), decimals = 2)
  # |>
  #   bind_rows(
  #     dwg_summ$interview |>
  #       left_join(dwg$days |> dplyr::select(event_date, period, day_type), by = "event_date") |> 
  #       drop_na(angler_final) |>  
  #       group_by(period, section_num, day_type, angler_final) |> 
  #       summarise(
  #         interview_hours_total = sum(fishing_time_total)
  #       ) |> 
  #       left_join(estimates_pe$effort |> ## join expanded catch estimates in pe$est_effort_s_ts_dt_at to total angler hours from interviews in pe$interview 
  #                   select(section_num, period, section_num, day_type, angler_final, effort_est = est) |>
  #                   group_by(section_num, period, day_type, angler_final) |> 
  #                   summarise(
  #                     angler_effort_total = sum(effort_est))
  #                 , 
  #                 by = c("section_num", "period", "day_type", "angler_final")) |> 
  #       group_by(section_num) |> 
  #       summarise(
  #         interview_hours_total = sum(interview_hours_total),
  #         angler_effort_total = sum(angler_effort_total),
  #         proportion_interviewed_effort = (interview_hours_total / angler_effort_total)
  #       ) 
  #     |> 
  #       mutate(
  #         angler_final = "total",
  #         angler_effort_total = if_else(is.nan(angler_effort_total), 0 , angler_effort_total) # Again, coerce to zero rows where bank effort not estimable due
  #         # to method of obtaining bank anglers from total - boat, circle back on this with KB at some point
  #       )
  #   ) |>
  #   mutate(across(where(is.numeric), round, 2)) |>
  #   left_join(
  #     dwg$interview |> distinct(fishing_location, section_num),
  #     by = "section_num") |>
  #   gt(groupname_col = "section_name") |>
  #   cols_label(
  #     angler_final = md("angler type"),
  #     interview_hours_total = md("total angler hours from direct sampling (interviews)"),
  #     angler_effort_total = md("total angler hours from expanded effort estimates"),
  #     proportion_interviewed_effort = md("proportion of total effort sampled in interviews")
  #   ) |>
  #   tab_header(
  #     title = md("Estimated total angler effort sampled during interviews"),
  #     subtitle = md("The season-long total of angler hours from interviews and effort counts. The fifth column displays the estimated proportion of total angling effort (angler hours) that was directly sampled during creel interviews.")) |>
  #   tab_options(container.overflow.x = T, container.overflow.y = T)

  
}