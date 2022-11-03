prep_inputs_pe_days_total <- function(days, ...){
  
  days |>
    tidyr::pivot_longer(
      cols = starts_with("open_section"),
      names_to = "section_temp",
      values_to = "is_open") |>
    dplyr::filter(is_open) |>
    dplyr::mutate(section_num = as.numeric(gsub("^.*_", "", section_temp))) |>
    count(period, DayType, section_num, name = "N_days_open")  

}
