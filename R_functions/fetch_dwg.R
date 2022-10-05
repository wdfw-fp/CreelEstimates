fetch_dwg <- function(fishery_name, ...){
  
  dwg_base <- list(
    #event = "https://data.wa.gov/resource/ui95-axtn.csv",
    effort = "https://data.wa.gov/resource/h9a6-g38s.csv",
    interview = "https://data.wa.gov/resource/rpax-ahqm.csv",
    catch = "https://data.wa.gov/resource/6y4e-8ftk.csv"
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
    dplyr::select(interview_id, catch_id, species, run, life_stage, fin_mark, fate, fish_count) |> 
    dplyr::mutate(
      catch_group = paste(species, life_stage, fin_mark, fate, sep = "_") # fish catch groups to estimate catch of 
    )
  
  return(dwg)
}

# #tests
# fetch_dwg("Naselle winter steelhead 2022")
# fetch_dwg("Skykomish summer Chinook 2022")
