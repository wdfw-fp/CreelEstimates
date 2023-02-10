# plot PE effort estimates by period (time stratum) and section

plot_est_pe_effort <- function(
    estimates_pe_effort,
    ...
){

  estimates_pe$effort |> 
    ggplot(aes(period, est, fill = interaction(day_type, angler_final))) +
    labs(fill = "Angler and day type groups") +
    geom_col(position = position_stack()) +
    scale_color_brewer(palette = "PRGn", aesthetics = c("fill")) +
    facet_wrap(~section_num, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2) 
   
}