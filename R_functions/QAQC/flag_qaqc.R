#function to highlight column on tables which flagged an issue in the data
#example: hightlight_qaqc(no_end_time, "MISSING_end_time")
flag_qaqc <- function(table, column_name) {
  foo <- table %>% 
    tab_style(
      style = cell_text(color = "red"),
      locations = cells_body(columns = column_name)
    ) %>%
    tab_style(
      style = cell_fill(color = "yellow"),
      locations = cells_column_labels(columns = column_name)
    ) 
  return(foo)
}