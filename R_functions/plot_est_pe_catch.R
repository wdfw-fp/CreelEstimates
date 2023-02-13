# plot PE catch estimates by catch group (est_cg), period (time stratum) and section

plot_est_pe_catch <- function(
    estimates_pe_catch,
    est_catch_group,
    ...
){
  
  # estimates_pe$catch |>
  #   filter(est_cg == est_catch_group) |>
  #   ggplot(aes(period, est, fill = angler_final, color = angler_final)) +
  #   geom_col(position = position_stack()) +
  #   labs(title = est_catch_group) +
  #   facet_wrap(~section_num, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2)
  
  estimates_pe$catch |>
    filter(est_cg == est_catch_group) |>
    ggplot(aes(period, est, fill = interaction(day_type, angler_final))) +
    geom_col(position = position_stack(), color = "black") +
    scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
    labs(title = est_catch_group, fill = "Angler and day type groups") +
    facet_wrap(~section_num, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2)
  
}
