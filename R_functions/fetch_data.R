fetch_data <- function(fishery_name, data_source = "dwg", ...){
  
  #validate and format data_source input
  valid_sources <- c("dwg", "public", "internal", "direct") 
  data_source <- trimws(tolower(data_source))
  
  if (!data_source %in% valid_sources) {
    stop("Invalid data_source argument. \nMust be either `dwg` or `public` for accessing data.wa.gov. \nMust be either `internal` or `direct` for accessing Postgres database.")
  }
  
  #### public data portal ####
  if (data_source %in% c("dwg", "public")) {
  
    dwg_base <- list(
      #event = "https://data.wa.gov/resource/ui95-axtn.csv",
      effort = "https://data.wa.gov/resource/h9a6-g38s.csv",
      interview = "https://data.wa.gov/resource/rpax-ahqm.csv",
      catch = "https://data.wa.gov/resource/6y4e-8ftk.csv",
      water_bodies = "https://data.wa.gov/resource/nbd2-vdmz.csv",
      closures = "https://data.wa.gov/resource/6zm6-iep6.csv",
      #,gear = "https://data.wa.gov/resource/d2ks-afhz.csv" #currently unused?
      fishery_manager = "https://data.wa.gov/resource/vkjc-s5u8.csv"
    )
    
    dwg <- list()
    
    dwg$effort <- paste0(
      dwg_base$effort,
      "?$where=fishery_name='",
      fishery_name,
      "'&$limit=100000"
    ) |>
      utils::URLencode() |>
      readr::read_csv(show_col_types = F) |>
      tidyr::drop_na(count_type) |> 
      dplyr::select(-created_datetime, -modified_datetime)
    
    dwg$ll <- paste0(
      dwg_base$water_bodies,
      "?$where=water_body_desc in('",
      paste0(unique(dwg$effort$water_body), collapse = "','"),
      "')&$limit=100000"
    ) |>
      utils::URLencode() |>
      readr::read_csv(show_col_types = F)
    
    dwg$interview <- paste0(
      dwg_base$interview,
      "?$where=fishery_name='",
      fishery_name,
      "'&$limit=100000"
    ) |>
      utils::URLencode() |>
      readr::read_csv(show_col_types = F) |>
      dplyr::select(
        -created_datetime, -modified_datetime)
    
    dwg$catch <- paste0(
      dwg_base$catch,
      "?$where=fishery_name='",
      fishery_name,
      "'&$limit=100000"
    ) |> 
      utils::URLencode() |>
      readr::read_csv(show_col_types = F) |>
      dplyr::select(interview_id, catch_id, species, run, life_stage, fin_mark, sex, fork_length_cm, fate, fish_count) |> 
      dplyr::mutate(
        catch_group = paste(species, life_stage, fin_mark, fate, sep = "_") # fish catch groups to estimate catch of 
      )
    
    dwg$closures <- paste0(
      dwg_base$closures,
      "?$where=fishery_name='",
      fishery_name,
      "'&$limit=100000"
    ) |> 
      utils::URLencode() |>
      readr::read_csv(show_col_types = F) |>
      dplyr::select(fishery_name, section_num, event_date)
    
    dwg$fishery_manager <- paste0(
      dwg_base$fishery_manager,
      "?$where=fishery_name='",
      fishery_name,
      "'&$limit=100000"
    ) |>
      utils::URLencode() |>
      readr::read_csv(show_col_types = F)
    
    data <- dwg
  }
  
  #### Postgres database connection ####
  if (data_source %in% c("internal", "direct")) {
    
    #connect to database
    con <- establish_db_con()
    
    #verify active connection
    if (!DBI::dbIsValid(con)) {
      stop("No valid database connection provided.")
    }
    
    #inititalize output
    data <- list()
    
    #define standard filter to fishery name
    filter_condition <- paste0("fishery_name=='", fishery_name,"'")
    
    ##### query database for creel data ####
    cat("\nQuerying Postgres database...")
  
    #fishery_locations_table
    data$fishery_locations <- fetch_db_table(con, "creel", "vw_fishery_manager", filter = filter_condition) 
    
    #effort
    data$effort <- fetch_db_table(con, "creel", "vw_analysis_effort_count", filter = filter_condition) |> 
      tidyr::drop_na(count_type) |>   
      dplyr::select(-created_datetime, -modified_datetime)
    
    #format column type to match dwg
    data$effort$count_sequence <- as.double(data$effort$count_sequence)
    
    #interview
    data$interview <- fetch_db_table(con, "creel", "vw_analysis_interview", filter = filter_condition) |> 
      dplyr::select(-created_datetime, -modified_datetime)
    
    #catch
    data$catch <- fetch_db_table(con, "creel", "vw_analysis_catch", filter = filter_condition) |> 
      dplyr::select(interview_id, catch_id, species, run, life_stage, fin_mark, sex, fork_length_cm, fate, fish_count) |> 
      dplyr::mutate(
        catch_group = paste(species, life_stage, fin_mark, fate, sep = "_") # fish catch groups to estimate catch of 
      )
    
    #closures
    data$closures <- fetch_db_table(con, "creel", "vw_fishery_closure", filter = filter_condition) |> 
      dplyr::select(fishery_name, section_num, event_date)
    
    #water bodies - needs its own filter with outputs from data$effort
    water_bodies <- unique(data$effort$water_body)
    water_bodies <- paste0("'", water_bodies, "'", collapse = ", ")
    filter_condition2 <- paste0("water_body_desc %in% c(", water_bodies,")")
    
    data$ll <- fetch_db_table(con, "creel", "water_body_lut", filter = filter_condition2)
    
    #arrange to match dwg order
    dwg_order <- c("effort", "ll", "interview", "catch", "closures", "fishery_locations")
    
    data <- data[dwg_order]
  }
  
  return(data)
}

#examples

# # a, b, and c are equivalent. Function defaults to data.wa.gov.
# a <- fetch_data("Snohomish fall salmon 2023")
# b <- fetch_data("Snohomish fall salmon 2023", data_source = "dwg")
# c <- fetch_data("Snohomish fall salmon 2023", data_source = "public")
# 
# # d and e are equivalent. Queries database directly.
# d <- fetch_data("Snohomish fall salmon 2023", data_source = "internal")
# e <- fetch_data("Snohomish fall salmon 2023", data_source = "direct")
