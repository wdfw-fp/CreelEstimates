#Aggregate census (tie in) effort counts, associating to closest-in-time index count.

prep_effort_census <- function(eff, ...){
  eff_cen <- dplyr::filter(eff, tie_in_indicator == 1)
  eff_ind <- dplyr::filter(eff, tie_in_indicator == 0)
  
  dplyr::left_join(
    #census values of interest...
    dplyr::select(eff_cen, section_num, event_date, tie_in_indicator, count_type, count_quantity)
    ,
    #...get reassigned count_seq from closest index
    #this nested join is typically expanding, as multiple index counts may match each census section & date
    #then the slice_min & distinct cut back down to a single value to reassign to above
    dplyr::left_join(
      dplyr::distinct(eff_cen, section_num, event_date, tie_in_indicator, effort_start_time, count_sequence),
      dplyr::distinct(eff_ind, section_num, event_date, tie_in_indicator, effort_start_time, count_sequence),
      by = c("section_num", "event_date"),
      suffix = c("_cen", "_ind")
    ) |>
      dplyr::group_by(section_num, event_date) |>
      dplyr::slice_min(abs(effort_start_time_cen - effort_start_time_ind), n = 1) |>
      dplyr::ungroup() |>
      dplyr::distinct(section_num, event_date, count_sequence = count_sequence_ind)
    ,
    by = c("section_num", "event_date")
  ) |>
    dplyr::mutate(
      angler_final = dplyr::case_when(
        count_type %in% c("Boat", "Boat Anglers") ~ "boat",
        count_type %in% c("Bank", "Bank Anglers") ~ "bank",
        
        #expecting future mods to allow position rather than craft type
        #https://dfw-fp-r5.s3.us-west-2.amazonaws.com/data-dictionaries/creel_data_dictionary.html#effort_count_type_lut
        count_type == c("Boat - Motor") ~ "boat",
        count_type == c("Boat - Drift Raft") ~ "boat",
        count_type == c("Shore - Drift/Raft") ~ "boat", #possibly subject to mod along with interview case_when
        count_type == c("Shore - Motor Boat") ~ "boat", #possibly subject to mod along with interview case_when
        
        count_type == c("Boat - Pontoon/Kick/Kayak") ~ "bank",
        count_type == c("Shore - No Boat") ~ "bank",
        count_type == c("Shore - Pontoon/Kick/Kayak") ~ "bank"
      ),
      angler_final_int = as.integer(factor(angler_final))
    ) |>
    tidyr::drop_na(angler_final) |> 
    dplyr::group_by(section_num, event_date, tie_in_indicator, count_sequence, angler_final, angler_final_int) |>
    dplyr::summarize(count_census = sum(count_quantity), .groups = "drop") |>
    dplyr::arrange(section_num, event_date, count_sequence)
  
}
