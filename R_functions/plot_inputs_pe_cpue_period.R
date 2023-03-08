# plot cpue by period, section_num, angler_final, and catch group
# aggregating over day_type to simplify plot 

plot_inputs_pe_cpue_period <- function(
    days, #tibble with time strata and closure fields
    dwg_summarized, #list with shared interview, index and census tibbles
    daily_cpue_catch_est,
    est_catch_group,
    ...
){
  
  # cpue_period <- dplyr::left_join(
  #   dwg_summarized$interview
  #   , 
  #   days |> dplyr::select(event_date, day_type, period)
  #   ,
  #   by=c("event_date")
  # ) |>
  #   dplyr::filter(est_cg == est_catch_group) |> 
  #   dplyr::group_by(section_num, period, angler_final, est_cg) |> #day_type dropped to simplify plot 
  #   dplyr::summarise(
  #     n_obs = n(),
  #     total_catch = sum(fish_count),
  #     total_hours = sum(fishing_time_total),
  #     cpue_rom_period = total_catch / total_hours,
  #     .groups = "drop"
  #   )
  # 
  # cpue_period |> 
  #   ggplot(aes(period, cpue_rom_period, fill = angler_final, color = angler_final)) +
  #   geom_point(size = 3) +
  #   # scale_x_date("", date_breaks = "7 days", date_labels =  "%m-%d") + scale_y_continuous("") + # if using dates for x-axis
  #   scale_color_brewer(palette = "Set2", aesthetics = c("color", "fill")) +
  #   labs(title = est_catch_group) +
  #   # geom_text(aes(label = n_obs), nudge_y = 0.02, color = "black", check_overlap = TRUE, size = 2.5) + #option to see sample size
  #   facet_wrap(~section_num, scales = "fixed", ncol = 2, labeller = label_wrap_gen(multi_line = F))


  cpue_period <- dplyr::left_join(
    dwg_summarized$interview
    , 
    days |> dplyr::select(event_date, day_type, period)
    ,
    by=c("event_date")
  ) |>
    dplyr::filter(est_cg == est_catch_group) |> 
    dplyr::group_by(section_num, period, day_type, angler_final, est_cg) |> #day_type dropped to simplify plot 
    dplyr::summarise(
      n_obs = n(),
      total_catch = sum(fish_count),
      total_hours = sum(fishing_time_total),
      cpue_rom_period = total_catch / total_hours,
      .groups = "drop"
    )
  
  cpue_period |> 
    ggplot(aes(period, cpue_rom_period, fill = interaction(day_type, angler_final))) +
    # geom_jitter(aes(color = interaction(day_type, angler_final)), size = 3) +
    geom_point(aes(fill = interaction(day_type, angler_final)), color = "black", pch = 21, size = 3.25) +
    labs(title = est_catch_group, fill = "Angler and day type groups") +
    # scale_x_date("", date_breaks = "7 days", date_labels =  "%m-%d") + scale_y_continuous("") + # if using dates for x-axis
    scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
    # geom_text(aes(label = n_obs), nudge_y = 0.02, color = "black", check_overlap = TRUE, size = 2.5) + #option to see sample size
    facet_wrap(~section_num, scales = "fixed", ncol = 2, labeller = label_wrap_gen(multi_line = F))
  
}
