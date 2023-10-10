# plot PE catch estimates by catch group (est_cg), period (time stratum) and section

plot_est_pe_catch <- function(
    estimates_pe_catch,
    est_catch_group,
    period_pe,
    ...
)

{
  if(period_pe == "week"){
    estimates_pe$catch |>
      filter(est_cg == est_catch_group) |>
      ggplot(aes(min_event_date, est, fill = interaction(day_type, angler_final))) +
      scale_x_date(date_breaks = "1 week", labels = scales::date_format("%W"),
                   sec.axis = dup_axis(name = "", breaks = waiver(), labels = scales::date_format("%b"))) +
      ylab("Catch") +
      xlab("Date (week)") +
      labs(fill = "Angler and day type groups") +
      geom_col(position = position_stack(), color = "black", width = 3.5) +
      scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
      labs(title = est_catch_group, fill = "Angler and day type groups") +
      facet_wrap(~section_num, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2)
  
  }
  else if(period_pe == "month"){
    estimates_pe$catch |>
      filter(est_cg == est_catch_group) |>
      ggplot(aes(min_event_date, est, fill = interaction(day_type, angler_final))) +
      scale_x_date(date_breaks = "1 month", labels = scales::date_format("%b")) +
      ylab("Catch") +
      xlab("Date (month)") +
      labs(fill = "Angler and day type groups") +
      geom_col(position = position_stack(), color = "black", width = 12) +
      scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
      labs(title = est_catch_group, fill = "Angler and day type groups") +
      facet_wrap(~section_num, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2)
    
  }
  
  else if(period_pe == "duration"){
    estimates_pe$catch |>
      filter(est_cg == est_catch_group) |>
      ggplot(aes(min_event_date, est, fill = interaction(day_type, angler_final))) +
      scale_x_date() +
      ylab("Catch") +
      xlab("Date") +
      labs(fill = "Angler and day type groups") +
      geom_col(position = position_stack(), color = "black", width = 12) +
      scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
      labs(title = est_catch_group, fill = "Angler and day type groups") +
      facet_wrap(~section_num, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2)
  }
}