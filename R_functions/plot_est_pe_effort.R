# plot PE effort estimates by period (time stratum) and section

plot_est_pe_effort <- function(
    estimates_pe_effort,
    period_pe,
    ...
){
  if(period_pe == "week"){
  estimates_pe$effort |> 
    ggplot(aes(min_event_date, est, fill = interaction(day_type, angler_final))) +
    scale_x_date(date_breaks = "1 week", labels = scales::date_format("%W"),
                 sec.axis = dup_axis(name = "", breaks = waiver(), labels = scales::date_format("%b"))) +
    labs(fill = "Angler and day type groups") +
    ylab("Effort (angler hours)") +
    xlab("Date (week)") +
    geom_col(position = position_stack(), color = "black", width = 3.5) +
    scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
    facet_wrap(~section_num, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2) 
}
else if(period_pe == "month"){
  estimates_pe$effort |> 
    ggplot(aes(min_event_date, est, fill = interaction(day_type, angler_final))) +
    scale_x_date(date_breaks = "1 month", labels = scales::date_format("%b")) +
    labs(fill = "Angler and day type groups") +
    ylab("Effort (angler hours)") +
    xlab("Date (month)") +
    geom_col(position = position_stack(), color = "black", width = 12) +
    scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
    facet_wrap(~section_num, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2) 
  }
  
else if(period_pe == "duration"){
      estimates_pe$effort |> 
        ggplot(aes(min_event_date, est, fill = interaction(day_type, angler_final))) +
        scale_x_date() +
        labs(fill = "Angler and day type groups") +
        ylab("Effort (angler hours)") +
        xlab("Date") +
        geom_col(position = position_stack(), color = "black", width = 12) +
        scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
        facet_wrap(~section_num, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2) 
  }
}

