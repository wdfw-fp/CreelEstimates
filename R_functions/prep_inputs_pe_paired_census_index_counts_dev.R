# Summarize paired census and index angler effort counts 

prep_inputs_pe_paired_census_index_counts_dev <- function(
    days, #tibble with time strata and closure fields
    dwg_summarized, #list with shared interview, index and census tibbles
    interview_ang_per_object, #tibble of interview-based values to translate vehicle/trailer counts to boat/bank
    census_expan,
    study_design,
    ...
){

if(str_detect(study_design, "tandard" )){ #KB addition  
  
#KB NOTE: I replaced "ang_per_vhcl_trlr" with "ang_per_object" throughout entire script per changes in "prep_inputs_pe_int_ang_per_object" function
#KB QUESTIONs: 1.) I don't understand why NAs for count_index_mean were being turned into zeros in this original function (see ~L53)
#              2.) I don't understand why there was an if_else that would replace NAs for ang_per_object with a mean ang_per_object (see ~L54-59) 
#              3.) I don't understand why we have a filter for "angler_hours_daily_mean" that are NA; not sure why we would have any NAs
#KB MODS -     1.) Instead of filtering out bank angler rows where the resulting "angler_hours_daily_mean" values where negative (total - boat), I turn them to zeros (~L108); not sure if this matters but seems good to keep the row of data 
  

# KB - commented out the following 8 lines (reordered how datasets are combined and then summarized) 
# eff_ind <-
#   dplyr::left_join(
#     dwg_summarized$effort_index
#     ,
#     days |> dplyr::select(event_date, day_type, period, day_length)
#     ,
#     by=c("event_date")
#   ) # |>
# KB - commented out the following 6 lines ("angler_final" already completed in /moved to "prep_dwg_effort_index_dev" function using exact same logic)
# dplyr::mutate(
#   angler_final = dplyr::case_when(
#     count_type == "Trailers Only" ~ "boat",
#     count_type == "Vehicle Only" ~ "total"
#   )
    # )

#KB - commented out the following 29 lines and replaced with chunked up version  
#   #using means if no interviews available to assign angler numbers per vehicle/trailer on a given day
#   angler_hours_daily_mean <- 
#     dplyr::full_join(
# #KB    #interview_ang_per_vehic #coerced above to total/boat 
#       interview_ang_per_object |> select(angler_final, ang_per_object)  #KB addition  
#       ,
#       eff_ind |> #already in total/boat
#         dplyr::group_by(section_num, day_type, event_date, day_length, count_type, angler_final) |>
#         dplyr::summarise(count_index_mean = mean(count_index), .groups = "drop")
#       ,
#       by = c("angler_final")
#     ) |>
#     dplyr::group_by(section_num, day_type, angler_final) |> 
#     dplyr::mutate(
#       count_index_mean = replace_na(count_index_mean, 0), 
#      ang_per_object = 
#         if_else(
#           is.na(ang_per_object),
#           mean(ang_per_object, na.rm=T),
#           ang_per_object
#         )
#     ) |> 
#     dplyr::ungroup() |> 
#     mutate(
#       angler_hours_daily_mean = ang_per_object * count_index_mean * day_length
#     ) |>
#     select(-day_length, -ang_per_object, -count_index_mean) |> 
#     tidyr::drop_na(angler_hours_daily_mean) |> 
#     arrange(section_num, event_date)
#   
#   ################################################# ###
#   #KB edits: start
#   
#     effort_index_daily_mean <-
#       dwg_summarized$effort_index |>
#       dplyr::group_by(section_num, event_date, angler_final) |>
#       dplyr::summarise(count_index_mean = mean(count_index, na.rm = TRUE), .groups = "drop")|>
#       arrange(event_date, section_num) # |> 
# #KB NOTE:          dplyr::mutate(count_index_mean = replace_na(count_index_mean, 0)) #KB - i don't understand why NAs were being turned into zeros before
# 
#     angler_hours_daily_mean <- 
#       effort_index_daily_mean |>
#       left_join(days |> dplyr::select(event_date, day_type, period, day_length), by=c("event_date")) |> 
#       left_join(interview_ang_per_object |> select(angler_final, ang_per_object), by=c("angler_final"))|>
#       dplyr::ungroup() |>
#       mutate(
#         angler_hours_daily_mean = ang_per_object * count_index_mean * day_length
#       ) |>
#       select(-day_length, -ang_per_object, -count_index_mean) |>
#       tidyr::drop_na(angler_hours_daily_mean) |>
#       arrange(section_num, event_date)
# 
#     #KB edits: end
#     ################################################# ###    
#     
#   #now coerce back to angler_final bank/boat (unexpanded)
#   if(any(angler_hours_daily_mean$angler_final=="boat")){
#     angler_hours_daily_mean <- 
#       angler_hours_daily_mean |> 
#       tidyr::pivot_wider(
#         names_from = angler_final, 
#         values_from = angler_hours_daily_mean, 
#       ) |>
#       dplyr::mutate(
#         boat = tidyr::replace_na(boat, 0),
#         bank = total - boat, 
#         total = NULL
#       ) |> 
#       tidyr::pivot_longer(
#         cols = c(boat, bank),
#         names_to = "angler_final",
#         values_to = "angler_hours_daily_mean"
#       ) |>
#       mutate(
#         angler_hours_daily_mean = ifelse(is.na(angler_hours_daily_mean) | angler_hours_daily_mean<0, 0, angler_hours_daily_mean) #KB addition (instead of filtering out NAs and negatives, I turned them to zero)
#       )
# #KB      # #NAs and negatives here reflect inadequate or problematic data barring meaningful inference...
# #KB       # dplyr::filter(
# #KB       #   !is.na(angler_hours_daily_mean),
# #KB       #   angler_hours_daily_mean >= 0
# #KB       # )
#   } else { #if no boat/trailer then total==bank
#     angler_hours_daily_mean <- 
#       angler_hours_daily_mean |> 
#       dplyr::mutate(angler_final = "bank")
#   }
  
  
  if(nrow(dwg_summarized$effort_census) == 0) {
    census_TI_expan <- 
      expand_grid(
#KB        # section_num = unique(angler_hours_daily_mean$section_num), 
#KB        # angler_final = unique(angler_hours_daily_mean$angler_final),
        section_num = dwg_summarized$effort_index |> distinct(section_num) |> pull(), 
        angler_final = dwg_summarized$effort_index |> distinct(angler_final)|> pull(),
        TI_expan_final = 1
      )
  } else {
    #begin census expansion values object by joining census and index in terms of total & boat
    census_TI_expan <- 
      dplyr::left_join(
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
#KB - commented out following 21 lines and replaced with similar code from above (that is reduced/simpler)      
      # #index counts via interviews for angler-per-vehic; angler_final already total & boat
      # #this is very similar to above pe_estimates$angler_hours_daily_mean
      # #but all count_seqs rather than summarized to daily mean
      # #as above, applies mean ang-per-vehic within section_num-day_type-angtype
      # #where interview missing an ang-type or no interviews on that date
      # #prevents loss of use of census info b/c of a single day missing interviews...
      # dplyr::full_join(
      #   interview_ang_per_object |> select(angler_final, ang_per_object) #interview_ang_per_vehic
      #   ,
      #   eff_ind
      #   ,
      #   by = c("angler_final")
      # ) |>
      #   dplyr::group_by(section_num, day_type, angler_final) |> 
      #   dplyr::mutate(
      #     ang_per_object = if_else(
      #       is.na(ang_per_object),
      #       mean(ang_per_object, na.rm=T),
      #       ang_per_object)) |> 
      #   dplyr::ungroup() |> 
      #   dplyr::mutate(count_index = ang_per_object * count_index) |>
      #   dplyr::select(section_num, event_date, count_sequence, angler_final, count_index)

      dwg_summarized$effort_index |> select(-fishery_name, -count_type, -angler_final_int)
      ,
      by = c("section_num", "event_date", "count_sequence", "angler_final")
    ) |> 
    tidyr::drop_na(count_index) |> 
    left_join(interview_ang_per_object |> select(angler_final, ang_per_object), by=c("angler_final"))|>
    dplyr::ungroup() |>
    mutate(
      count_index_expand = ang_per_object * count_index 
    ) |> 
    select(-count_index, -ang_per_object)  |> 
    rename(count_index = count_index_expand)
    
    #now overwrite, coercing angler_final back to bank/boat as above for pe_estimates$angler_hours_daily_mean
    #again dropping NAs and negatives as invalid for inferring estimates
    if(any(census_TI_expan$angler_final=="boat")){
      census_TI_expan <- 
        census_TI_expan |> 
        tidyr::pivot_longer(
          cols = c(count_census, count_index),
          names_to = "count_type",
          values_to = "count"
        ) |> 
        tidyr::pivot_wider(
          names_from = angler_final,
          values_from = count
        ) |>
        dplyr::mutate(
          boat = replace_na(boat, 0), # NA's here are implicit 0's due to lack of boat anglers in data 
          bank = total - boat, 
          total = NULL
        ) |> 
        tidyr::pivot_longer(
          cols = c(boat, bank),
          names_to = "angler_final",
          values_to = "count"
        ) |>
        tidyr::pivot_wider(
          names_from = count_type,
          values_from = count
        ) |>
        dplyr::mutate( 
          count_index = if_else(count_index < 0 , 0, count_index) 
          # need derived negative count_index estimates of bank anglers to pass through subsequent filter, 
          # if count_index doesn't pass filter then TI_expan is NA. Should only be an issue in very data limited situations
          # if not addressed, then effort from index counts with no TI_expan value get dropped from final estimates 
        ) |> 
        dplyr::filter(
          !is.na(count_census),
          !is.na(count_index),
          count_census >= 0,
          count_index >= 0
        )
    } else {
      census_TI_expan <- census_TI_expan |> mutate(angler_final = "bank")
    }
    
    census_TI_expan <- 
      census_TI_expan |> 
      dplyr::group_by(section_num, angler_final) |>
      dplyr::summarise(
        dplyr::across(c(count_census, count_index), sum),
        .groups = "drop"
      ) |>
      dplyr::left_join(census_expan, by = c("section_num", "angler_final")) |>
      dplyr::mutate(
        TI_expan_weighted = count_census / count_index,
        TI_expan_weighted = if_else(
          is.infinite(TI_expan_weighted) | TI_expan_weighted == 0,
          1,
          TI_expan_weighted
        ) |> replace_na(1),
        TI_expan_final = if_else(
          cen_exp_meth == "Direct",
          TI_expan_weighted / p_census,
          census_indir)
      )
    
  }
}else if(study_design == "Drano"){ #KB addition
  # effort_index_daily_mean <-
  #   dwg_summarized$effort_index |>
  #   dplyr::group_by(section_num, event_date, angler_final) |>
  #   dplyr::summarise(count_index_mean = mean(count_index, na.rm = TRUE), .groups = "drop")|>
  #   arrange(event_date, section_num) # |> 
  # #KB NOTE:          dplyr::mutate(count_index_mean = replace_na(count_index_mean, 0)) #KB - i don't understand why NAs were being turned into zeros before
  # 
  # angler_hours_daily_mean <- 
  #   effort_index_daily_mean |>
  #   left_join(days |> dplyr::select(event_date, day_type, period, day_length), by=c("event_date")) |> 
  #   left_join(interview_ang_per_object |> select(angler_final, ang_per_object), by=c("angler_final"))|>
  #   dplyr::ungroup() |>
  #   mutate(
  #     angler_hours_daily_mean = ang_per_object * count_index_mean * day_length
  #   ) |>
  #   select(-day_length, -ang_per_object, -count_index_mean) |>
  #   tidyr::drop_na(angler_hours_daily_mean) |>
  #   arrange(section_num, event_date)
  census_TI_expan <- 
    expand_grid(
      section_num = dwg_summarized$effort_index |> distinct(section_num) |> pull(), 
      angler_final = dwg_summarized$effort_index |> distinct(angler_final)|> pull(),
      TI_expan_final = 1
    )

}  
  return(census_TI_expan)
}
