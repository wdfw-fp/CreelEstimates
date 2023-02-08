# plot PE effort estimates by period (time stratum) and section

plot_est_pe_effort <- function(
    estimates_pe_effort,
    ...
){

  estimates_pe$effort |> 
    ggplot(aes(period, est, fill = angler_final, color = angler_final)) +
    geom_col(position = position_stack()) +
    facet_wrap(~section_num, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2) 
   
}