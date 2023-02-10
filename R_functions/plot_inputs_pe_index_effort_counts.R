# plot paired season-long counts from census and index angler effort count surveys

plot_inputs_pe_index_effort_counts <- function(
    effort_index,
    ...
){

  dwg_summ$effort_index |>
    mutate(count_sequence = factor(count_sequence)) |> 
    ggplot(aes(event_date, count_index, fill = count_sequence, color = count_sequence)) +
    #geom_point() + geom_text(aes(label = count_index), nudge_y = 1, check_overlap = T) +
    geom_col(position = position_dodge(width = 0.7)) +
    scale_x_date("", date_breaks = "7 days", date_labels =  "%m-%d") + scale_y_continuous("") +
    scale_color_brewer(palette = "Dark2", aesthetics = c("fill")) +
    facet_wrap(~section_num + count_type, scales = "fixed", ncol = 1, labeller = label_wrap_gen(multi_line = F))
   
}