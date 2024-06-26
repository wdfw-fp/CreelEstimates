prep_export <- function(con, creel_estimates) {

  #query database tables necessary to get UUIDS
  cat("\nFetching database uuids.")
  project_lut <- fetch_db_table(con, "creel", "project_lut") |> select(project_name, project_id)
  fishery_lut <- fetch_db_table(con, "creel", "fishery_lut") |> select(fishery_name, fishery_id)
  species_lut <- fetch_db_table(con, "creel", "species_lut") |> select(species_name, species_id)
  life_stage_lut <- fetch_db_table(con, "creel", "life_stage_lut") |> select(life_stage_name, life_stage_id)
  fin_mark_lut <- fetch_db_table(con, "creel", "fin_mark_lut") |> select(fin_mark_code, fin_mark_id)
  fate_lut <- fetch_db_table(con, "creel", "fate_lut") |> select(fate_name, fate_id)
  angler_type_lut <- fetch_db_table(con, "creel", "angler_type_lut") |> select(angler_type_code, angler_type_id)
  crc_area_lut <- fetch_db_table(con, "creel", "crc_area_lut") |> select(catch_area_code, crc_area_id)
  
  #parse out catch group column into component fields to match with database format and use of UUIDs
  
  ##total UUIDs ----------------------------------------------------------------------------------------------
  creel_estimates$total <- creel_estimates$total |>
    tidyr::separate(col = est_cg, 
                    into = c("species_name", "life_stage_name", "fin_mark_code", "fate_name"),
                    sep = "_")
  
  #join UUIDs to appropriate fields
  creel_estimates$total <- creel_estimates$total |> left_join(project_lut, by = "project_name")
  creel_estimates$total <- creel_estimates$total |> left_join(fishery_lut, by = "fishery_name")
  creel_estimates$total <- creel_estimates$total |> left_join(species_lut, by = "species_name")
  creel_estimates$total <- creel_estimates$total |> left_join(life_stage_lut, by = "life_stage_name")
  creel_estimates$total <- creel_estimates$total |> left_join(fin_mark_lut, by = "fin_mark_code")
  creel_estimates$total <- creel_estimates$total |> left_join(fate_lut, by = "fate_name")
  
  #reformat, remove common name fields in favor of UUID fields
  creel_estimates$total <- creel_estimates$total |> 
    select(-c("project_name", "fishery_name", "species_name", "life_stage_name","fin_mark_code", "fate_name")) |>
    relocate(c("project_id", "fishery_id")) |> 
    relocate(c("species_id", "life_stage_id", "fin_mark_id", "fate_id"), .after = "model_type")
  
  ##stratum UUIDS --------------------------------------------------------------------------------------------
  creel_estimates$stratum <- creel_estimates$stratum |>
    tidyr::separate(col = est_cg, 
                    into = c("species_name", "life_stage_name", "fin_mark_code", "fate_name"),
                    sep = "_")
  
  #join UUIDs to appropriate fields
  creel_estimates$stratum <- creel_estimates$stratum |> left_join(project_lut, by = "project_name")
  creel_estimates$stratum <- creel_estimates$stratum |> left_join(fishery_lut, by = "fishery_name")
  creel_estimates$stratum <- creel_estimates$stratum |> left_join(species_lut, by = "species_name")
  creel_estimates$stratum <- creel_estimates$stratum |> left_join(life_stage_lut, by = "life_stage_name")
  creel_estimates$stratum <- creel_estimates$stratum |> left_join(fin_mark_lut, by = "fin_mark_code")
  creel_estimates$stratum <- creel_estimates$stratum |> left_join(fate_lut, by = "fate_name")
  creel_estimates$stratum <- creel_estimates$stratum |> left_join(angler_type_lut, by = c("angler_final" = "angler_type_code"))
  creel_estimates$stratum <- creel_estimates$stratum |> left_join(crc_area_lut, by = "catch_area_code")
  
  #reformat, remove common name fields in favor of UUID fields
  creel_estimates$stratum <- creel_estimates$stratum |> 
    select(-c("project_name", "fishery_name","species_name", "life_stage_name","fin_mark_code", "fate_name", 
              "angler_final", "catch_area_code", "catch_area_description")) |>
    relocate(c("project_id", "fishery_id")) |> 
    relocate(c("species_id", "life_stage_id", "fin_mark_id", "fate_id", "angler_type_id"), .after = "model_type") 
  
  #reformat NaN estimate values in stratum scale to 0 values
  creel_estimates$stratum <- creel_estimates$stratum |>  
    mutate(estimate_value = case_when(
      is.nan(estimate_value) ~ 0,
      TRUE ~ estimate_value
    ))
  
  return(creel_estimates)

}
