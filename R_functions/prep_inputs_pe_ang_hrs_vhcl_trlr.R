prep_inputs_pe_ang_hrs_vhcl_trlr <- function(
    days, #tibble with time strata and closure fields
    dwg_summarized, #list with shared interview, index and census tibbles
    interview_ang_per_vehic, #tibble of interview-based values to translate vehicle/trailer counts to boat/bank
    paired_census_index_counts,
    census_expan,
    ...
    ){
  
  eff_ind <- dplyr::left_join(
    dwg_summarized$effort_index
    , 
    days |> dplyr::select(event_date, day_type, period, day_length)
    ,
    by=c("event_date")
    ) |>  
    dplyr::mutate(
      angler_final = dplyr::case_when(
        count_type == "Trailers Only" ~ "boat",
        count_type == "Vehicle Only" ~ "total"
      )
    )

  #using means if no interviews available to assign angler numbers per vehicle/trailer on a given day
  angler_hours_daily_mean <- dplyr::full_join(
    interview_ang_per_vehic #coerced above to total/boat
    ,
    eff_ind |> #already in total/boat
      dplyr::group_by(section_num, day_type, event_date, day_length, angler_final) |>
      dplyr::summarise(count_index_mean = mean(count_index), .groups = "drop")
    ,
    by = c("angler_final")
    ) |>
    dplyr::group_by(section_num, day_type, angler_final) |> 
    dplyr::mutate(
      count_index_mean = replace_na(count_index_mean, 0), 
      ang_per_vhcl_trlr = if_else(
        is.na(ang_per_vhcl_trlr),
        mean(ang_per_vhcl_trlr, na.rm=T),
        ang_per_vhcl_trlr)
    ) |> 
    dplyr::ungroup() |> 
    mutate(angler_hours_daily_mean = ang_per_vhcl_trlr * count_index_mean * day_length) |>
    dplyr::select(-day_length, -ang_per_vhcl_trlr, -count_index_mean) |> 
    tidyr::drop_na(angler_hours_daily_mean) |> 
    arrange(section_num, event_date)
  
  #now coerce back to angler_final bank/boat (unexpanded)
  if(any(angler_hours_daily_mean$angler_final=="boat")){
    angler_hours_daily_mean <- angler_hours_daily_mean |> 
      tidyr::pivot_wider(
        names_from = angler_final, 
        values_from = angler_hours_daily_mean, 
      ) |>
      dplyr::mutate(
        boat = tidyr::replace_na(boat, 0),
        bank = total - boat, 
        total = NULL
        ) |> 
      tidyr::pivot_longer(
        cols = c(boat, bank),
        names_to = "angler_final",
        values_to = "angler_hours_daily_mean"
        ) |> 
      #NAs and negatives here reflect inadequate or problematic data barring meaningful inference...
      dplyr::filter(
        !is.na(angler_hours_daily_mean),
        angler_hours_daily_mean >= 0
        )
  } else { #if no boat/trailer then total==bank
    angler_hours_daily_mean <- angler_hours_daily_mean |> 
      dplyr::mutate(angler_final = "bank")
  }
  
  
  if(nrow(dwg_summarized$effort_census) == 0) {
    census_TI_expan <- expand_grid(
      section_num = unique(angler_hours_daily_mean$section_num), 
      angler_final = unique(angler_hours_daily_mean$angler_final),
      TI_expan_final = 1)
  } else {
    #begin census expansion values object by joining census and index in terms of total & boat
    census_TI_expan <- dplyr::left_join(
      #census already grouped & summed by event_date, section_num, tie_in_indicator, count_sequence, and angler_final [bank, boat]
      #but as for interview above, first split and collapse to reassign angler_final as total & boat
      bind_rows(
        dwg_summarized$effort_census |>
          dplyr::group_by(section_num, event_date, count_sequence) |>
          dplyr::summarize(angler_final = "total", count_census = sum(count_census),  .groups = "drop")
        ,
        dwg_summarized$effort_census |>
          dplyr::filter(angler_final == "boat") |>
          dplyr::group_by(section_num, event_date, count_sequence) |>
          dplyr::summarize(angler_final = "boat", count_census = sum(count_census), .groups = "drop")
      ),
      #index counts via interviews for angler-per-vehic; angler_final already total & boat
      #this is very similar to above pe_estimates$angler_hours_daily_mean
      #but all count_seqs rather than summarized to daily mean
      #as above, applies mean ang-per-vehic within section_num-day_type-angtype
      #where interview missing an ang-type or no interviews on that date
      #prevents loss of use of census info b/c of a single day missing interviews...
      dplyr::full_join(
        interview_ang_per_vehic
        ,
        eff_ind
        ,
        by = c("angler_final")
        ) |>
        dplyr::group_by(section_num, day_type, angler_final) |> 
        dplyr::mutate(
          ang_per_vhcl_trlr = if_else(
            is.na(ang_per_vhcl_trlr),
            mean(ang_per_vhcl_trlr, na.rm=T),
            ang_per_vhcl_trlr)) |> 
        dplyr::ungroup() |> 
        dplyr::mutate(count_index = ang_per_vhcl_trlr * count_index) |>
        dplyr::select(section_num, event_date, count_sequence, angler_final, count_index)
      ,
      by = c("section_num", "event_date", "count_sequence", "angler_final")
    ) |> 
      tidyr::drop_na(count_index)
    
    #now overwrite, coercing angler_final back to bank/boat as above for pe_estimates$angler_hours_daily_mean
    #again dropping NAs and negatives as invalid for inferring estimates
####    
    
  #   if(any(census_TI_expan$angler_final=="boat")){
  #     census_TI_expan <- census_TI_expan |> 
  #       tidyr::pivot_longer(
  #         cols = c(count_census, count_index),
  #         names_to = "count_type",
  #         values_to = "count"
  #         ) |> 
  #       tidyr::pivot_wider(
  #         names_from = angler_final,
  #         values_from = count
  #         ) |>
  #       dplyr::mutate(
  #         boat = replace_na(boat, 0), # NA's here are implicit 0's due to lack of boat anglers in data 
  #         bank = total - boat, 
  #         total = NULL
  #         ) |> 
  #       tidyr::pivot_longer(
  #         cols = c(boat, bank),
  #         names_to = "angler_final",
  #         values_to = "count"
  #         ) |>
  #       tidyr::pivot_wider(
  #         names_from = count_type,
  #         values_from = count
  #         ) |>
  #       dplyr::mutate( 
  #         count_index = if_else(count_index < 0 , 0, count_index) 
  #         # need derived negative count_index estimates of bank anglers to pass through subsequent filter, 
  #         # if count_index doesn't pass filter then TI_expan is NA. Should only be an issue in very data limited situations
  #         # if not addressed, then effort from index counts with no TI_expan value get dropped from final estimates 
  #       ) |> 
  #       dplyr::filter(
  #         !is.na(count_census),
  #         !is.na(count_index),
  #         count_census >= 0,
  #         count_index >= 0
  #       )
  #   } else {
  #     census_TI_expan <- census_TI_expan |> mutate(angler_final = "bank")
  #   }
  #   
  #   census_TI_expan <- census_TI_expan |> 
  #     dplyr::group_by(section_num, angler_final) |>
  #     dplyr::summarise(
  #       dplyr::across(c(count_census, count_index), sum),
  #       .groups = "drop"
  #     ) |>
  #     dplyr::left_join(census_expan, by = c("section_num", "angler_final")) |>
  #     dplyr::mutate(
  #       TI_expan_weighted = count_census / count_index,
  #       TI_expan_weighted = if_else(
  #         is.infinite(TI_expan_weighted) | TI_expan_weighted == 0,
  #         1,
  #         TI_expan_weighted
  #         ) |> replace_na(1),
  #       TI_expan_final = if_else(
  #         cen_exp_meth == "Direct",
  #         TI_expan_weighted / p_census,
  #         census_indir)
  #     )
  #     
  # }
  
  }  
#####    
  census_TI_expan <- inputs_pe$paired_census_index_counts
  
  #now multiply mean daily effort in fishing_time by tie-in ratio bias term 
  #aiming for event_date, section_num, angler_final [total, boat, bank (as total-boat)]
  angler_hours_daily_mean <- left_join(
    angler_hours_daily_mean
    , 
    census_TI_expan |> dplyr::select(section_num, angler_final, TI_expan_final)
    ,
    by = c("section_num", "angler_final")
    ) |>
    mutate(
      ang_hrs_daily_mean_TI_expan = angler_hours_daily_mean * TI_expan_final
      )
  
  return(angler_hours_daily_mean)
}
