paired_effort_interview_check <- function(
    dwg_effort,
    dwg_interview,
    ...){
  
  effort_interview_check <- dwg_summ$effort_index |>
  select(-fishery_name) |> 
  pivot_wider(names_from = count_type, values_from = count_index, values_fill = 0) |> 
  group_by(event_date) |> 
  summarize(
    sum_trailers = sum(`Trailers Only`),
    sum_vehicles = sum(`Vehicle Only`)
  ) |> 
  left_join(
    dwg_summ$interview |>
      group_by(event_date, angler_final) |> 
      summarize(
        n_interviews = n()
      ) |> 
      pivot_wider(names_from = angler_final, values_from = n_interviews, values_fill = 0) |> 
      rename(
        n_bank_interviews = bank, n_boat_interviews = boat
      )
    ,
    by = c("event_date")
  ) |> 
    mutate(
      n_bank_interviews = if_else(is.na(n_bank_interviews), 0, n_bank_interviews),
      n_boat_interviews = if_else(is.na(n_boat_interviews), 0, n_boat_interviews),
      trailer_no_interview = if_else(sum_trailers > 0 & n_boat_interviews == 0, TRUE, FALSE)
    ) |> 
  select(event_date, sum_vehicles, n_bank_interviews, sum_trailers, n_boat_interviews, trailer_no_interview)
  
  return(effort_interview_check)
  

  effort_interview_check |>
    pivot_longer(
        cols = c(sum_vehicles, n_bank_interviews, sum_trailers, n_boat_interviews),
        names_to = "count_type",
        values_to = "count_quantity"
    ) |> 
    filter(count_type %in% c("sum_trailers", "n_boat_interviews")) |> 
    mutate(count_type = as.factor(count_type)) |> 
    ggplot(aes(event_date, count_quantity, group = count_type)) +
    geom_line(aes(color = count_type)) 
    theme_bw()
  
  
}

