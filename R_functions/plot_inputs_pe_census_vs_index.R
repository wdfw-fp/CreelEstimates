# plot paired season-long counts from census and index angler effort count surveys

plot_inputs_pe_census_vs_index <- function(
    census_TI_expan,
    ...
){
  
  if(params$census_expansion == "Direct") {
    
census_TI_expan |>
      ggplot(aes(count_index, count_census, color = angler_final)) +
      geom_abline(slope = 1, intercept = 0, linetype = 2) +
      scale_x_continuous(limits = c(0, NA)) +
      scale_y_continuous(limits = c(0, NA)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      facet_wrap(~paste("Section:",section_num), labeller = label_wrap_gen(multi_line = F))
    
    
  }
  
}


  
  