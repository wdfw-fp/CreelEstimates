# plot paired season-long counts from census and index angler effort count surveys

plot_inputs_pe_census_vs_index <- function(
    census_TI_expan,
    ...
){
  census_TI_expan |>
    ggplot(aes(count_index, count_census, fill = angler_final)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_continuous(limits = c(0, NA)) +
    geom_point(color = "black", pch = 21, size = 3.25) +
    labs(fill = "Angler type") +
    scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
    geom_smooth(method = "lm", se = FALSE) +
    guides(fill = guide_legend(override.aes = list(linetype = 0))) +
    facet_wrap(~paste("Section:",section_num), labeller = label_wrap_gen(multi_line = F))
}


  
  