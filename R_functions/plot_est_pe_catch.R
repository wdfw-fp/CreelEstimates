# plot PE catch estimates by catch group (est_cg), period (time stratum) and section

plot_est_pe_catch <- function(
    estimates_pe_catch,
    est_catch_group,
    ...
){
  
  estimates_pe$catch |>
    filter(est_cg == est_catch_group) |> 
    ggplot(aes(period, est, fill = angler_final, color = angler_final)) +
    geom_col(position = position_stack()) +
    facet_wrap(~est_cg + section_num, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2) 
  
}
