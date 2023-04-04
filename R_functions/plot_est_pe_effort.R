# plot PE effort estimates by period (time stratum) and section

plot_est_pe_effort <- function(
    estimates_pe_effort,
    ...
){

  # x axis uses period
  
  # estimates_pe$effort |> 
  #   ggplot(aes(period, est, fill = interaction(day_type, angler_final))) +
  #   labs(fill = "Angler and day type groups") +
  #   geom_col(position = position_stack(), color = "black") +
  #   scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
  #   facet_wrap(~section_num, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2) 
  
  # x axis uses min_event_date
  
  estimates_pe$effort |> 
    ggplot(aes(min_event_date, est, fill = interaction(day_type, angler_final))) +
    scale_x_date() +
    labs(fill = "Angler and day type groups") +
    geom_col(position = position_stack(), color = "black", width = 3.5) +
    scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
    facet_wrap(~section_num, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2) 
   
}