prep_inputs_pe_int_ang_per_object <- function(
    dwg_summarized, #list with shared interview, index and census tibbles
    study_design,
    ...
    ){

if(str_detect(study_design, "tandard" )){ #KB addition 
  
  # KB - commented out following 14 lines of the original code
  # bind_rows(
  #   dwg_summarized$interview |>
  #     dplyr::summarize(
  #       angler_final = "total",
  #       ang_per_vhcl_trlr = sum(person_count_final) / sum(vehicle_count), .groups = "drop"
  #     )
  #   ,
  #   dwg_summarized$interview |>
  #     dplyr::filter(angler_final == "boat") |>
  #     dplyr::summarize(
  #       angler_final = "boat",
  #       ang_per_vhcl_trlr = sum(person_count_final) / sum(trailer_count), .groups = "drop"
  #     )
  # )
  
  #KB NOTES: I think this updated summary would be more accurate (estimates anglers per trailer for just boat anglers) and explicit (keeps "angler_final" matching orignal call)...
  # ... BUT may not be correct (may want summaries to be across all angler groups and not separated/group_by angler_final) and certainly will require additional format updates in other functions
  # VERSION #1 UPDATE (group_by)
  # dwg_summarized$interview |>
  # group_by(angler_final) |> 
  #   dplyr::summarize(
  #     ang_per_vhcl = sum(person_count_final) / sum(vehicle_count)
  #   , ang_per_trlr = sum(person_count_final) / sum(trailer_count)
  #   , .groups = "drop"
  #   )
  # VERSION #2 UPDATE (same as before but adding column denoting what is being summarized)
  bind_rows(
    dwg_summarized$interview |>
      dplyr::summarize(
        angler_final = "total",
        count_type = "vehicle",
        person_count_total = sum(person_count_final),
        object_count_total = sum(vehicle_count),
        ang_per_object = sum(person_count_final) / sum(vehicle_count), .groups = "drop"
      )
    ,
    dwg_summarized$interview |>
      dplyr::filter(angler_final == "boat") |>
      dplyr::summarize(
        angler_final = "boat",
        count_type = "trailer",
        person_count_total = sum(person_count_final),
        object_count_total = sum(trailer_count),
        ang_per_object = sum(person_count_final) / sum(trailer_count), .groups = "drop"
      )
  )

}else if(study_design == "Drano"){
  
# List of "angler_final" that are in effort_index dataset but missing from interview dataset (in theory, this should not happen but hypothetically could)
# KB NOTE - would be good to add code to deal with the possibilty of "angler_final" missing and add it to summary table with some sort of average  
  missing_angler_final_int<-
    anti_join(
      dwg_summarized$effort_index |> distinct(angler_final)
      ,   
      dwg_summarized$interview |> distinct(angler_final)
      , by = "angler_final"
    ) |> pull()
  
  #KB NOTE: study design did not require a "boat_count" (number of boats for a given interview group); so here I'm simply dividing the person count by the number of interviews which implicitly assumes 1 boat per interviewed group
  bind_rows(
    dwg_summarized$interview |>
    group_by(angler_final) |> 
    dplyr::summarize(
      count_type = "boats",
      person_count_total = sum(person_count_final),
      groups_interviewed = n(),
      ang_per_object = sum(person_count_final) / n(), .groups = "drop"
    )
  )
}  
}
