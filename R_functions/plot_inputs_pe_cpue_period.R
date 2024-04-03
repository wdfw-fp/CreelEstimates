# plot cpue by period, section_num, angler_final, and catch group
# aggregating over day_type to simplify plot 

plot_inputs_pe_cpue_period <- function(
    days, #tibble with time strata and closure fields
    dwg_summarized, #list with shared interview, index and census tibbles
    daily_cpue_catch_est,
    est_catch_group,
    period_pe,
    ...
){
  
  cpue_period <- 
    dplyr::left_join(
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
    ) |>
    left_join( # add back matching date information for stratum estimates
      dwg$days |>
        select(event_date, period, year) |> 
        group_by(period) |> 
        summarise(
          min_event_date = min(event_date),
          max_event_date = max(event_date)),
      by = "period"
    )
  
  if(period_pe == "week"){
    cpue_period |> 
      ggplot(aes(min_event_date, cpue_rom_period, fill = interaction(day_type, angler_final))) +
      geom_point(aes(fill = interaction(day_type, angler_final)), color = "black", pch = 21, size = 3.25) +
      scale_x_date(date_breaks = "1 week", labels = scales::date_format("%W"),
                   sec.axis = dup_axis(name = "", breaks = waiver(), labels = scales::date_format("%b"))) +
      ylab("Catch per unit effort (fish/hr)") +
      xlab("Date (week)") +
      labs(title = est_catch_group, fill = "Angler and day type groups") +
      scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
      # geom_text(aes(label = n_obs), nudge_y = 0.02, color = "black", check_overlap = TRUE, size = 2.5) + #option to see sample size
      facet_wrap(~section_num, scales = "fixed", ncol = 2, labeller = label_wrap_gen(multi_line = F))
  }
  else if(period_pe == "month"){
    
    cpue_period |> 
      ggplot(aes(min_event_date, cpue_rom_period, fill = interaction(day_type, angler_final))) +
      geom_point(aes(fill = interaction(day_type, angler_final)), color = "black", pch = 21, size = 3.25) +
      scale_x_date(date_breaks = "1 month", labels = scales::date_format("%b")) +
      ylab("Catch per unit effort (fish/hr)") +
      xlab("Date (month)") +
      labs(title = est_catch_group, fill = "Angler and day type groups") +
      scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
      # geom_text(aes(label = n_obs), nudge_y = 0.02, color = "black", check_overlap = TRUE, size = 2.5) + #option to see sample size
      facet_wrap(~section_num, scales = "fixed", ncol = 2, labeller = label_wrap_gen(multi_line = F))
    
  }
  
  else if(period_pe == "duration"){
    cpue_period |> 
      ggplot(aes(min_event_date, cpue_rom_period, fill = interaction(day_type, angler_final))) +
      geom_point(aes(fill = interaction(day_type, angler_final)), color = "black", pch = 21, size = 3.25) +
      scale_x_date() +
      ylab("Catch per unit effort (fish/hr)") +
      xlab("Date") +
      labs(title = est_catch_group, fill = "Angler and day type groups") +
      scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
      # geom_text(aes(label = n_obs), nudge_y = 0.02, color = "black", check_overlap = TRUE, size = 2.5) + #option to see sample size
      facet_wrap(~section_num, scales = "fixed", ncol = 2, labeller = label_wrap_gen(multi_line = F))
  }
}