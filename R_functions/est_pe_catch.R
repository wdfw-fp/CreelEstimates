est_pe_catch <- function(
    days,
    pe_inputs_list, 
    ...
){
  
  est_catch <- dplyr::left_join(
    #dates expanded to sections * angler_final * opendays
    days |>
      dplyr::select(period, day_type, event_date, starts_with("open_section")) |>
      tidyr::pivot_longer(
        cols = starts_with("open_section"), 
        names_to = "section_num", 
        values_to = "is_open"
      ) |>
      dplyr::filter(is_open) |>
      dplyr::mutate(
        section_num = as.numeric(gsub("^.*_", "", section_num)), 
        is_open = NULL,
        angler_final = list(unique(pe_inputs_list$ang_hrs_daily_mean$angler_final)) 
      ) |> 
      tidyr::unnest(angler_final)
    ,
    pe_inputs_list$daily_cpue_catch_est |> 
      dplyr::select(section_num, event_date, angler_final, est_cg, catch_estimate)
    ,
    by = c("section_num", "event_date", "angler_final")
  ) |> 
    dplyr::group_by(section_num, period, day_type, angler_final, est_cg) |>
    dplyr::summarize(
      n_obs = sum(!is.na(catch_estimate)), 
      dplyr::across(
        .cols = c(catch_estimate),
        .fns = list(
          mean = ~mean(.x, na.rm = T),
          var = ~var(.x, na.rm = T)
        ), #sd = ~sd(.x, na.rm = T), med = ~median(.x, na.rm=T),
        .names = "catch_est_{.fn}"
      ),
      .groups = "drop"
    ) |> 
    dplyr::right_join(
      pe_inputs_list$days_total
      ,
      by = c("section_num", "period", "day_type")
    ) |> 
    #!!not sure this is correct - could/should recalc df for within-week/month?
    dplyr::left_join(
      pe_inputs_list$df |> 
        dplyr::distinct(section_num, angler_final, df)
      ,
      by = c("section_num", "angler_final")
    ) |> 
    dplyr::mutate(
      catch_est_var = replace_na(catch_est_var, 0),
      est = N_days_open * catch_est_mean,
      var = if_else(
        n_obs < N_days_open,
        (N_days_open^2) * (catch_est_var / n_obs) * (1-(n_obs/N_days_open)),
        (N_days_open^2) * (catch_est_var / n_obs)
      ),
      l95 = est - qt(1-(0.05/2),df)*(var^0.5),
      u95 = est + qt(1-(0.05/2),df)*(var^0.5)
    ) |>
    tidyr::drop_na(est_cg)
    
  return(est_catch)
}
