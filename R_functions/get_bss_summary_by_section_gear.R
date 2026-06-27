#' Summarize BSS model draws by section and gear type
#'
#' Computes season-total effort (angler-hours) and catch (fish) from posterior
#' draws of `lambda_E_S` and `lambda_C_S`, returning mean, median, and 95% CIs
#' disaggregated by section and gear (angler type). Optionally collapses across
#' sections and/or gear types, and optionally stratifies by a day-level grouping
#' variable (e.g. a regulatory open/close period).
#'
#' @details
#' **What this function computes:**
#' - `lambda_E_S[s,d,g]` is the *mean daily effort rate* for section s, day d,
#'   gear g. To convert to angler-hours it must be multiplied by `L[d]` (day
#'   length in hours), which is supplied via `dwg$days`.
#' - Season effort per draw: `sum_d( lambda_E_S[s,d,g] * L[d] )`
#' - Season catch per draw: `sum_d( lambda_E_S[s,d,g] * L[d] * lambda_C_S[s,d,g] )`
#'   (i.e. effort-hours * CPUE = catch)
#' - Summaries (mean, median, 95% CI) are then taken *across draws*, never
#'   across summary statistics — all aggregation (summing over days, sections,
#'   gears, or day-group levels) is done draw-by-draw before any moments are
#'   computed, avoiding transformation bias.
#'
#' **Column naming convention in draws_df:**
#' Columns follow the `posterior::as_draws_df()` format:
#' `lambda_E_S[s,d,g]` where s = section index, d = day index, g = gear index.
#' These indices are *positional* (1-based), not the actual section_num or
#' angler_final labels — the lookup tables `section_map` and `gear_map` handle
#' the translation.
#'
#' **Day grouping (`day_groups`) — two supported formats:**
#'
#' *Uniform format* (D rows, one grouping column): one group value per day,
#' applied identically across all sections. Use for fisheries where regulatory
#' periods apply uniformly across space (e.g. a season-wide closure window).
#' Row order must match `dwg$days`.
#'
#' *Section-specific format* (D × S rows, `section_num` column + one grouping
#' column): group values vary by both day and section. Use for crossed designs
#' like the Hoh River 2023-24 boat regulation, where the same day of week is
#' open in one section and closed in another. Must contain a `section_num`
#' column whose values match those in `s_map` (i.e. the mapped section labels,
#' not positional indices), and one additional grouping column. Row order within
#' each section must match `dwg$days`.
#'
#' In both formats, rows where the grouping column is `NA` are dropped from
#' group-level totals with a warning but retained in ungrouped season totals.
#' Prepare this data frame in the calling code to keep fishery-specific logic
#' out of this function.
#'
#' **Collapsed totals (`collapse`):**
#' When `collapse = TRUE`, additional rows are appended with
#' `section_num = "all"` and `angler_final = "all"`, representing the grand
#' total across all sections and gear types. When `day_groups` is also supplied
#' and `collapse_within_groups = TRUE`, collapsed rows are also computed within
#' each group level. For section-specific groupings, the collapsed group mask
#' is the union across sections: a day contributes to a group level's collapsed
#' total if *any* section has that group value on that day.
#'
#' @param draws_bss Named list of per-catch-group draw objects, as produced by
#'   the `estimates_bss` chunk when `save_draws = TRUE`. Each element is itself
#'   a list with `$draws_df` (a `posterior::as_draws_df()` data frame containing
#'   columns `.draw`, `.iteration`, `.chain`, and parameter columns named
#'   `lambda_E_S[s,d,g]` and `lambda_C_S[s,d,g]`) and `$summary_df`. The
#'   function extracts the correct element using `ecg` as the key.
#' @param dwg List. The creel data working group object. Requires:
#'   - `dwg$days`: a data frame with one row per fishing day (in the same order
#'     as the day index `d` used in the Stan model), containing column `day_length`
#'     (numeric, hours). If your column is named differently, adjust `day_length_col`.
#' @param inputs_bss List. The BSS inputs list for this catch group, used to
#'   recover the mapping from positional indices to section_num and angler_final
#'   labels. Specifically, `inputs_bss$index_section` and `inputs_bss$index_gear`
#'   (or equivalent — see `section_map` / `gear_map` arguments if your
#'   `prep_inputs_bss()` uses different names).
#' @param ecg Character. The catch group label (e.g. "Coho Adult UM Released"),
#'   attached to every row of the output for joining with other results.
#' @param section_map Optional named character vector mapping positional section
#'   indices (as character) to section labels. E.g. `c("1"="Section 1", "2"="Section 2")`.
#'   If NULL, the function attempts to infer this from `inputs_bss`. If that also
#'   fails, integer indices are used as-is.
#' @param gear_map Optional named character vector mapping positional gear indices
#'   to `angler_final` labels. E.g. `c("1"="bank", "2"="boat")`.
#'   If NULL, the function attempts to infer this from `inputs_bss`.
#' @param day_length_col Character. Name of the day-length column in `dwg$days`.
#'   Defaults to `"day_length"`.
#' @param probs Numeric vector. Quantile probabilities for credible intervals.
#'   Defaults to `c(0.025, 0.975)` (95% CI). Change to e.g. `c(0.05, 0.95)`
#'   for 90% CI.
#' @param collapse Logical. If `TRUE`, append rows giving grand totals collapsed
#'   across all sections and gear types (`section_num = "all"`,
#'   `angler_final = "all"`). Aggregation is done draw-by-draw. Default `FALSE`.
#' @param day_groups Optional data frame specifying day-level grouping. Two
#'   formats accepted — see Details. Must contain exactly one grouping column
#'   beyond any `section_num` or date columns. Default `NULL` (no grouping).
#' @param return_draws Logical. If `TRUE`, return raw per-draw season totals in
#'   long format instead of summarized moments. Output columns are `est_cg`,
#'   `section_num`, `angler_final`, `quantity`, `draw` (integer), `value`
#'   (numeric), plus the grouping column if `day_groups` is supplied. Useful
#'   when you need to aggregate across strata with correct CIs — sum the `value`
#'   column across strata within each `draw`, then take quantiles of that summed
#'   vector. Default `FALSE`.
#' @param collapse_within_groups Logical. When both `collapse = TRUE` and
#'   `day_groups` is supplied, also compute collapsed totals within each group
#'   level. Default `TRUE`.
#'
#' @return A tibble with one row per section × gear × quantity combination
#'   (× group level if `day_groups` supplied), with columns:
#'   \describe{
#'     \item{est_cg}{Catch group label}
#'     \item{section_num}{Section label, or `"all"` for collapsed rows}
#'     \item{angler_final}{Gear/angler type label, or `"all"` for collapsed rows}
#'     \item{<group_col>}{Present only when `day_groups` is supplied; the grouping
#'       column name and values from that data frame. Season-total rows (not
#'       grouped) carry `NA` in this column.}
#'     \item{quantity}{"effort" (angler-hours) or "catch" (fish)}
#'     \item{mean}{Posterior mean of the season total}
#'     \item{median}{Posterior median of the season total}
#'     \item{lo95}{Lower bound of credible interval (default 2.5th percentile)}
#'     \item{hi95}{Upper bound of credible interval (default 97.5th percentile)}
#'     \item{sd}{Posterior standard deviation of the season total}
#'     \item{n_draws}{Number of posterior draws used}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic usage — season totals by section and gear:
#' estimates_bss[[ecg]]$summary_by_section_gear <- get_bss_summary_by_section_gear(
#'   draws_bss  = draws_bss,
#'   dwg        = dwg,
#'   inputs_bss = inputs_bss[[ecg]],
#'   ecg        = ecg
#' )
#'
#' # With grand totals collapsed across all sections and gears:
#' estimates_bss[[ecg]]$summary_by_section_gear <- get_bss_summary_by_section_gear(
#'   draws_bss  = draws_bss,
#'   dwg        = dwg,
#'   inputs_bss = inputs_bss[[ecg]],
#'   ecg        = ecg,
#'   collapse   = TRUE
#' )
#'
#' # Uniform day grouping — same group value applies to all sections each day.
#' # dwg$days$open_section_* reflects fishery open/close status (already in the
#' # Stan model via O[d,s]) and should NOT be used here. Use this format for
#' # time-only regulatory overlays that apply uniformly across all sections
#' # (e.g. an emergency closure window affecting the whole fishery):
#' day_groups <- dwg$days |>
#'   filter(between(event_date, as.Date(params$est_date_start), as.Date(params$est_date_end))) |>
#'   left_join(closure_periods, by = "event_date") |>
#'   select(emergency_closure)  # single grouping column; TRUE/FALSE or "open"/"closed"
#'
#' estimates_bss[[ecg]]$summary_by_section_gear <- get_bss_summary_by_section_gear(
#'   draws_bss  = draws_bss,
#'   dwg        = dwg,
#'   inputs_bss = inputs_bss[[ecg]],
#'   ecg        = ecg,
#'   collapse   = TRUE,
#'   day_groups = day_groups
#' )
#'
#' # Hoh River 2023-24 example — boat prohibition varies by section AND day of
#' # week (Table 1 of the study design). Constructed from dwg$days$day (day-of-
#' # week name) and dwg$days$event_date as the date spine, crossed with section.
#' # NOTE: open_section_* columns in dwg$days indicate fishery open/close status
#' # and are intentionally not used here — that is already captured in the Stan
#' # model via O[d,s]. This flag captures only the boat-prohibition overlay.
#' # tidyr::crossing() guarantees every section gets exactly D rows in day order
#' # regardless of any gaps in dwg$days.
#' hoh_boat_reg <- dwg$days |>
#'   filter(between(event_date, as.Date(params$est_date_start), as.Date(params$est_date_end))) |>
#'   select(event_date, day) |>
#'   tidyr::crossing(section_num = c(1L, 2L, 3L)) |>
#'   dplyr::mutate(
#'     # Table 1: regulation permitting boat-based fishing by date and section
#'     # Section 1 (Lower Hoh, RM 0-14.4):    prohibited Sun-Tue, allowed Wed-Sat
#'     # Section 2 (Middle Hoh, RM 14.4-23.0): allowed Sun-Tue, prohibited Wed-Sat
#'     # Section 3 (Upper Hoh, RM 23.0-29.7):  prohibited all days
#'     boat_allowed = dplyr::case_when(
#'       section_num == 1L & day %in% c("Sunday", "Monday", "Tuesday")                      ~ FALSE,
#'       section_num == 1L & day %in% c("Wednesday", "Thursday", "Friday", "Saturday")      ~ TRUE,
#'       section_num == 2L & day %in% c("Sunday", "Monday", "Tuesday")                      ~ TRUE,
#'       section_num == 2L & day %in% c("Wednesday", "Thursday", "Friday", "Saturday")      ~ FALSE,
#'       section_num == 3L                                                                   ~ FALSE
#'     )
#'   ) |>
#'   dplyr::arrange(section_num, event_date) |>
#'   dplyr::select(section_num, boat_allowed)  # drop event_date; row order within
#'                                             # each section matches dwg$days order
#'
#' estimates_bss[[ecg]]$summary_by_section_gear <- get_bss_summary_by_section_gear(
#'   draws_bss  = draws_bss,
#'   dwg        = dwg,
#'   inputs_bss = inputs_bss[[ecg]],
#'   ecg        = ecg,
#'   collapse   = TRUE,
#'   day_groups = hoh_boat_reg
#' )
#' }
#'
#' @importFrom dplyr tibble bind_rows
#' @importFrom stringr str_match
get_bss_summary_by_section_gear <- function(
    draws_bss,
    dwg,
    inputs_bss,
    ecg,
    section_map            = NULL,
    gear_map               = NULL,
    day_length_col         = "day_length",
    probs                  = c(0.025, 0.975),
    collapse               = FALSE,
    day_groups             = NULL,
    collapse_within_groups = TRUE,
    return_draws           = FALSE
) {

  # ---- 0. Input validation ----
  stopifnot(
    "draws_bss must be a named list"         = is.list(draws_bss) && !is.data.frame(draws_bss),
    "ecg must be a scalar character"         = is.character(ecg) && length(ecg) == 1,
    "ecg not found in draws_bss"             = ecg %in% names(draws_bss),
    "draws_bss[[ecg]]$draws_df must exist"   = is.data.frame(draws_bss[[ecg]]$draws_df),
    "dwg$days must exist"                    = !is.null(dwg$days),
    "day_length_col not found in dwg$days"   = day_length_col %in% names(dwg$days),
    "collapse must be logical"               = is.logical(collapse) && length(collapse) == 1,
    "collapse_within_groups must be logical" = is.logical(collapse_within_groups) && length(collapse_within_groups) == 1
  )

  if (length(probs) != 2 || !all(probs >= 0 & probs <= 1) || probs[1] >= probs[2]) {
    stop("`probs` must be a length-2 vector with 0 <= probs[1] < probs[2] <= 1")
  }

  # ---- 1. Extract draws data frame for this catch group ----
  draws_df <- draws_bss[[ecg]]$draws_df

  # ---- 2. Identify and parse lambda columns ----
  all_cols  <- names(draws_df)
  e_pattern <- "^lambda_E_S\\[(\\d+),(\\d+),(\\d+)\\]$"
  c_pattern <- "^lambda_C_S\\[(\\d+),(\\d+),(\\d+)\\]$"

  e_cols <- grep(e_pattern, all_cols, value = TRUE)
  c_cols <- grep(c_pattern, all_cols, value = TRUE)

  if (length(e_cols) == 0) stop("No lambda_E_S columns found in draws_bss[[ecg]]$draws_df")
  if (length(c_cols) == 0) stop("No lambda_C_S columns found in draws_bss[[ecg]]$draws_df")

  parse_indices <- function(cols, pattern) {
    m <- stringr::str_match(cols, pattern)
    data.frame(
      col = cols,
      s   = as.integer(m[, 2]),
      d   = as.integer(m[, 3]),
      g   = as.integer(m[, 4]),
      stringsAsFactors = FALSE
    )
  }

  e_idx <- parse_indices(e_cols, e_pattern)
  c_idx <- parse_indices(c_cols, c_pattern)

  s_vals <- sort(unique(e_idx$s))
  d_vals <- sort(unique(e_idx$d))
  g_vals <- sort(unique(e_idx$g))
  D      <- length(d_vals)
  n_draws <- nrow(draws_df)

  # ---- 3. Day lengths L[d] ----
  L <- dwg$days[[day_length_col]]

  if (length(L) != D) {
    stop(paste0(
      "Mismatch: draws_bss[[ecg]]$draws_df implies D = ", D, " days, but ",
      "dwg$days has ", length(L), " rows. Ensure dwg$days is filtered to the ",
      "same date range used to build inputs_bss."
    ))
  }

  # ---- 4. Resolve section and gear labels ----
  resolve_map <- function(user_map, vals, inputs_obj, key, fallback_prefix) {
    if (!is.null(user_map)) return(user_map)
    if (!is.null(inputs_obj[[key]])) {
      labels <- inputs_obj[[key]]
      if (length(labels) == length(vals)) {
        return(setNames(as.character(labels), as.character(vals)))
      }
    }
    message(
      "Could not resolve ", fallback_prefix, " labels from inputs_bss$", key,
      ". Using integer indices. Supply `", fallback_prefix, "_map` to override."
    )
    setNames(as.character(vals), as.character(vals))
  }

  s_map <- resolve_map(section_map, s_vals, inputs_bss, "index_section", "section")
  g_map <- resolve_map(gear_map,    g_vals, inputs_bss, "index_gear",    "gear")

  # ---- 5. Parse and validate day_groups ----
  # Detects format (uniform vs section-specific) and builds mask structures.
  #
  # Uniform format:    day_mask[[lev]]       — length-D logical, same for all sections
  # Section-specific:  day_mask_s[[s]][[lev]] — length-D logical, varies by section
  #
  # has_section_groups flags which structure to use downstream.

  has_section_groups <- FALSE

  if (!is.null(day_groups)) {
    stopifnot("day_groups must be a data frame" = is.data.frame(day_groups))

    reserved_cols <- c("section_num", "date", "event_date")
    group_cols    <- setdiff(names(day_groups), reserved_cols)

    if (length(group_cols) != 1) {
      stop(
        "`day_groups` must contain exactly one grouping column (excluding ",
        "`section_num` and any date columns). Found non-reserved columns: ",
        paste(group_cols, collapse = ", ")
      )
    }
    group_col <- group_cols

    if ("section_num" %in% names(day_groups)) {
      # ---- Section-specific format ----
      has_section_groups <- TRUE

      # Validate: each section should appear exactly D times
      sec_labels_in_map <- as.character(s_map)  # mapped labels (e.g. "1","2","3")
      sec_labels_in_dg  <- as.character(unique(day_groups$section_num))

      missing_secs <- setdiff(sec_labels_in_map, sec_labels_in_dg)
      if (length(missing_secs) > 0) {
        warning(
          "These section labels from s_map are absent from day_groups$section_num: ",
          paste(missing_secs, collapse = ", "),
          ". Those sections will have no group-level rows."
        )
      }

      rows_per_sec <- table(as.character(day_groups$section_num))
      wrong_count  <- rows_per_sec[rows_per_sec != D]
      if (length(wrong_count) > 0) {
        stop(
          "In section-specific day_groups, each section must have exactly D = ", D,
          " rows (one per model day, in day order). Sections with wrong counts: ",
          paste(names(wrong_count), collapse = ", ")
        )
      }

      # Build day_mask_s: list keyed by positional section index s,
      # each element a list keyed by group level with a length-D logical mask.
      grp_levels <- sort(unique(day_groups[[group_col]][!is.na(day_groups[[group_col]])]))

      day_mask_s <- setNames(lapply(s_vals, function(s) {
        sec_label <- s_map[as.character(s)]
        grp_sec   <- day_groups[[group_col]][as.character(day_groups$section_num) == sec_label]

        na_days <- is.na(grp_sec)
        if (any(na_days)) {
          warning(
            sum(na_days), " day(s) for section '", sec_label,
            "' have NA in `day_groups$", group_col,
            "` and will be excluded from group-level totals for that section."
          )
        }

        setNames(
          lapply(grp_levels, function(lev) !is.na(grp_sec) & grp_sec == lev),
          as.character(grp_levels)
        )
      }), as.character(s_vals))

      # For collapsed group totals: union mask across sections
      # A day contributes to a group level's collapsed total if any section
      # has that group value on that day.
      day_mask_collapsed <- setNames(
        lapply(grp_levels, function(lev) {
          masks <- lapply(s_vals, function(s) day_mask_s[[as.character(s)]][[as.character(lev)]])
          Reduce(`|`, masks)
        }),
        as.character(grp_levels)
      )

    } else {
      # ---- Uniform format ----
      if (nrow(day_groups) != D) {
        stop(paste0(
          "`day_groups` (uniform format) has ", nrow(day_groups), " rows but ",
          "D = ", D, " model days. They must match."
        ))
      }

      grp_vec    <- day_groups[[group_col]]
      na_days    <- is.na(grp_vec)
      if (any(na_days)) {
        warning(
          sum(na_days), " day(s) have NA in `day_groups$", group_col,
          "` and will be excluded from group-level totals."
        )
      }
      grp_levels <- sort(unique(grp_vec[!na_days]))

      # Uniform mask: same length-D logical for every section
      day_mask <- setNames(
        lapply(grp_levels, function(lev) !is.na(grp_vec) & grp_vec == lev),
        as.character(grp_levels)
      )
      day_mask_collapsed <- day_mask  # identical for collapsed totals
    }
  }

  # ---- 7. Build per-(s,g) draw-level matrices ----
  # Cache [n_draws x D] effort-hour and catch matrices so the collapse step
  # can sum across (s,g) draw-by-draw without re-reading draws_df.
  sg_matrices <- list()

  for (s in s_vals) {
    for (g in g_vals) {
      key <- paste0(s, "_", g)

      e_cols_sg   <- e_idx$col[e_idx$s == s & e_idx$g == g]
      c_cols_sg   <- c_idx$col[c_idx$s == s & c_idx$g == g]
      e_day_order <- e_idx$d[e_idx$s == s & e_idx$g == g]
      c_day_order <- c_idx$d[c_idx$s == s & c_idx$g == g]

      e_cols_sg <- e_cols_sg[order(e_day_order)]
      c_cols_sg <- c_cols_sg[order(c_day_order)]

      e_mat     <- as.matrix(draws_df[, e_cols_sg])
      c_mat     <- as.matrix(draws_df[, c_cols_sg])
      e_hrs_mat <- sweep(e_mat, 2, L, `*`)  # [n_draws x D], angler-hours

      sg_matrices[[key]] <- list(e_hrs = e_hrs_mat, c_rate = c_mat)
    }
  }

  # ---- 8. Helpers ----

  # Summarize a per-draw vector into one summary row
  summarize_draws_vec <- function(vals, qty_label, sec_label, gear_label,
                                  group_val = NA) {
    row <- dplyr::tibble(
      est_cg       = ecg,
      section_num  = sec_label,
      angler_final = gear_label,
      quantity     = qty_label,
      mean         = mean(vals),
      median       = stats::median(vals),
      lo95         = stats::quantile(vals, probs[1]),
      hi95         = stats::quantile(vals, probs[2]),
      sd           = stats::sd(vals),
      n_draws      = n_draws
    )
    if (!is.null(day_groups)) {
      row[[group_col]] <- group_val
      row <- row[, c("est_cg", "section_num", "angler_final", group_col,
                     "quantity", "mean", "median", "lo95", "hi95", "sd", "n_draws")]
    }
    row
  }

  # Return a per-draw long-format tibble (used when return_draws = TRUE)
  draws_vec_to_long <- function(vals, qty_label, sec_label, gear_label,
                                group_val = NA) {
    row <- dplyr::tibble(
      est_cg       = ecg,
      section_num  = sec_label,
      angler_final = gear_label,
      quantity     = qty_label,
      draw         = seq_along(vals),
      value        = vals
    )
    if (!is.null(day_groups)) {
      row[[group_col]] <- group_val
      row <- row[, c("est_cg", "section_num", "angler_final", group_col,
                     "quantity", "draw", "value")]
    }
    row
  }

  # Dispatch to the right output format
  emit_rows <- function(vals_effort, vals_catch, sec_label, gear_label,
                        group_val = NA) {
    if (return_draws) {
      dplyr::bind_rows(
        draws_vec_to_long(vals_effort, "effort", sec_label, gear_label, group_val),
        draws_vec_to_long(vals_catch,  "catch",  sec_label, gear_label, group_val)
      )
    } else {
      dplyr::bind_rows(
        summarize_draws_vec(vals_effort, "effort", sec_label, gear_label, group_val),
        summarize_draws_vec(vals_catch,  "catch",  sec_label, gear_label, group_val)
      )
    }
  }

  # Compute season totals from matrix slice and emit
  compute_rows <- function(e_hrs, c_rate, sec_label, gear_label,
                           day_mask_vec = NULL, group_val = NA) {
    if (!is.null(day_mask_vec)) {
      e_hrs  <- e_hrs[,  day_mask_vec, drop = FALSE]
      c_rate <- c_rate[, day_mask_vec, drop = FALSE]
    }
    emit_rows(
      vals_effort = rowSums(e_hrs),
      vals_catch  = rowSums(e_hrs * c_rate),
      sec_label   = sec_label,
      gear_label  = gear_label,
      group_val   = group_val
    )
  }

  # ---- 9. Main output loop ----
  results <- list()

  for (s in s_vals) {
    for (g in g_vals) {
      key        <- paste0(s, "_", g)
      e_hrs      <- sg_matrices[[key]]$e_hrs
      c_rate     <- sg_matrices[[key]]$c_rate
      sec_label  <- s_map[as.character(s)]
      gear_label <- g_map[as.character(g)]

      # Season total — all days, no group column
      results[[length(results) + 1]] <- compute_rows(e_hrs, c_rate, sec_label, gear_label)

      # Group-level totals
      if (!is.null(day_groups)) {
        for (lev in grp_levels) {
          mask <- if (has_section_groups) {
            day_mask_s[[as.character(s)]][[as.character(lev)]]
          } else {
            day_mask[[as.character(lev)]]
          }
          results[[length(results) + 1]] <- compute_rows(
            e_hrs, c_rate, sec_label, gear_label,
            day_mask_vec = mask,
            group_val    = lev
          )
        }
      }
    }
  }

  # ---- 10. Collapsed totals (all sections x all gears) ----
  if (collapse) {
    e_hrs_all <- Reduce(`+`, lapply(sg_matrices, `[[`, "e_hrs"))
    ec_all    <- Reduce(`+`, lapply(sg_matrices, function(m) m$e_hrs * m$c_rate))

    results[[length(results) + 1]] <- emit_rows(
      rowSums(e_hrs_all), rowSums(ec_all), "all", "all"
    )

    if (!is.null(day_groups) && collapse_within_groups) {
      for (lev in grp_levels) {
        mask <- day_mask_collapsed[[as.character(lev)]]
        results[[length(results) + 1]] <- emit_rows(
          rowSums(e_hrs_all[, mask, drop = FALSE]),
          rowSums(ec_all[,    mask, drop = FALSE]),
          "all", "all", group_val = lev
        )
      }
    }
  }

  # ---- 11. Combine and return ----
  out <- dplyr::bind_rows(results)
  rownames(out) <- NULL
  out
}
