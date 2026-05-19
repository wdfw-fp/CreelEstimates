#' Estimate proportion of total angler effort sampled during interviews
#'
#' Evaluates the proportion of estimated total angler hours (effort) that were
#' sampled during creel interviews, grouped by section, angler type, period,
#' and day type. Returns a season-long summary collapsed across periods.
#'
#' @param days A data frame of fishing days produced by [prep_days()], containing
#'   columns `event_date`, `period`, and `day_type`.
#' @param dwg_summ A named list of summarized creel data frames, as produced by
#'   the shared data aggregation steps in the analysis template. Must contain an
#'   `interview` element with columns `interview_id`, `section_num`, `event_date`,
#'   `angler_final`, and `fishing_time_total`.
#' @param estimates_pe_effort A data frame of PE effort estimates, typically
#'   `estimates_pe$effort`. Must contain columns `section_num`, `period`,
#'   `day_type`, `angler_final`, and `est`.
#'
#' @return A data frame with one row per `section_num` and `angler_type`,
#'   containing columns:
#'   \describe{
#'     \item{section_num}{River section identifier.}
#'     \item{angler_type}{Angler type (renamed from `angler_final`).}
#'     \item{interview_hours_total}{Total angler hours recorded across interviews.}
#'     \item{angler_effort_total}{Total estimated angler effort (hours) from PE.}
#'     \item{proportion_interviewed_effort}{Ratio of interviewed hours to total
#'       estimated effort.}
#'   }
#'
#' @export
est_pe_effort_sampled_by_interviews <- function(
    days,
    dwg_summ,
    estimates_pe_effort
) {
  
  effort_by_strata <- estimates_pe_effort |>
    dplyr::select(section_num, period, day_type, angler_final, effort_est = est) |>
    dplyr::group_by(section_num, period, day_type, angler_final) |>
    dplyr::summarise(
      angler_effort_total = sum(effort_est),
      .groups = "drop"
    )
  
  dwg_summ$interview |>
    dplyr::distinct(interview_id, section_num, event_date, angler_final, fishing_time_total) |>
    dplyr::left_join(
      dplyr::select(days, event_date, period, day_type),
      by = "event_date"
    ) |>
    tidyr::drop_na(angler_final) |>
    dplyr::group_by(period, section_num, day_type, angler_final) |>
    dplyr::summarise(
      interview_hours_total = sum(fishing_time_total),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      effort_by_strata,
      by = c("section_num", "period", "day_type", "angler_final")
    ) |>
    dplyr::mutate(
      # Coerce NaN to zero for strata where bank effort is not directly estimable
      # (derived as total minus boat effort); revisit with KB
      angler_effort_total = dplyr::if_else(is.nan(angler_effort_total), 0, angler_effort_total)
    ) |>
    dplyr::group_by(section_num, angler_final) |>
    dplyr::summarise(
      interview_hours_total = sum(interview_hours_total),
      angler_effort_total   = sum(angler_effort_total),
      proportion_interviewed_effort = interview_hours_total / angler_effort_total,
      .groups = "drop"
    ) |>
    dplyr::rename(angler_type = angler_final)
  
}