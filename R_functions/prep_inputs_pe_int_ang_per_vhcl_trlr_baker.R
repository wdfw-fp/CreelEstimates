prep_inputs_pe_int_ang_per_vhcl_trlr_baker <- function(
    dwg_summarized, #list with shared interview, index and census tibbles
    ...
    ){

  bind_rows(
    # dwg_summarized$interview |>
    #   dplyr::summarize(
    #     angler_final = "total",
    #     ang_per_vhcl_trlr = sum(person_count_final) / sum(vehicle_count), .groups = "drop")
    # ,
    dwg_summarized$interview |>
      dplyr::filter(angler_final == "boat") |>
      dplyr::summarize(
        angler_final = "boat",
        ang_per_vhcl_trlr = sum(person_count_final) / sum(trailer_count), .groups = "drop")
  )
  
}
