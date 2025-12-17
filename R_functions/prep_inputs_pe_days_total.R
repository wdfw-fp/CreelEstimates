# Calculate total days the fishery was open per strata (strata = period, day_type, and section_num)

prep_inputs_pe_days_total <- function(
    days, # tibble of dates and corresponding fields (e.g., year, month, period, open/close fishery by section)
    ...
){
  
  days |>
    tidyr::pivot_longer(
      cols = starts_with("open_section"),
      names_to = "section_temp",
      values_to = "is_open") |>
    dplyr::filter(is_open) |>
    dplyr::mutate(section_num = as.numeric(gsub("^.*_", "", section_temp))) |>
    count(period, day_type, section_num, name = "N_days_open")  

}
