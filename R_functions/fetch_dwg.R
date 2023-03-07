fetch_dwg <- function(fishery_name, ...){
  
  dwg_base <- list(
    #event = "https://data.wa.gov/resource/ui95-axtn.csv",
    effort = "https://data.wa.gov/resource/h9a6-g38s.csv",
    interview = "https://data.wa.gov/resource/rpax-ahqm.csv",
    catch = "https://data.wa.gov/resource/6y4e-8ftk.csv",
    water_bodies = "https://data.wa.gov/resource/nbd2-vdmz.csv",
    closures = "https://data.wa.gov/resource/6zm6-iep6.csv"
    #,gear = "https://data.wa.gov/resource/d2ks-afhz.csv" #currently unused?
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
      -created_datetime, -modified_datetime,
      -state_residence, -zip_code)
  
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
  
  return(dwg)
}

# #tests
# fetch_dwg("Naselle winter steelhead 2022")
# fetch_dwg("Skykomish summer Chinook 2022")
