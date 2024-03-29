prep_inputs_pe_census_expan <- function(
    int,
    eff,
    census_expansion,
    ...){

  if(int |> filter(location_type == "Section") |> nrow() > 0){
  int |> 
    dplyr::filter(location_type == "Section") |> 
    dplyr::distinct(section_num, p_census_bank, p_census_boat) |>
    tidyr::pivot_longer(
      cols = starts_with("p_census_"), 
      names_prefix = "p_census_",
      names_to = "angler_final",
      values_to = "p_census"
    ) |> 
    dplyr::mutate(
      p_census = replace_na(p_census, 1),
      census_indir = 1,
      cen_exp_meth = census_expansion
    ) |> 
    dplyr::arrange(angler_final, section_num)
  }
  else if(int |> filter(location_type == "Section") |> nrow() == 0){
      eff |> 
        dplyr::filter(location_type == "Section") |> 
        dplyr::distinct(section_num, p_census_bank, p_census_boat) |>
        tidyr::pivot_longer(
          cols = starts_with("p_census_"), 
          names_prefix = "p_census_",
          names_to = "angler_final",
          values_to = "p_census"
        ) |> 
        dplyr::mutate(
          p_census = replace_na(p_census, 1),
          census_indir = 1,
          cen_exp_meth = census_expansion
        ) |> 
        dplyr::arrange(angler_final, section_num)      
  }
}
