# calculate the number of objects (e.g., trailers, vehicles, boats) each angler group brought to the fishery based on study_design used

prep_inputs_pe_int_ang_per_object <- function(
    dwg_summarized, # list with shared interview, index and census tibbles
    study_design,   # parameter specifying study design and which if/else loop gets called below
    ...
    ){

if(str_detect(study_design, "tandard" )){ 
  
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
