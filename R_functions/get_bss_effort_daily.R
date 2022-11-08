get_bss_effort_daily <- function(bss_fit, ecg, ...){
  bss_fit |> 
    summary(pars = c("E")) |> 
    pluck("summary") |> #only want the combined-chains version
    as.data.frame() |> 
    rownames_to_column("estimate") |> 
    as_tibble() |> 
    mutate(indices = str_sub(estimate, 3, 20) |> str_remove("\\]")) |> 
    separate(
      col = indices,
      into = c("section_num", "day_index", "angler_final")
    ) |> 
    mutate(
      across(c(section_num, day_index), as.integer),
      angler_final = if_else(angler_final == "1", "bank", "boat"),
      est_cg = ecg
    )
}