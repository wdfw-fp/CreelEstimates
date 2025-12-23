# Function 2: Generate PPC plots and interval coverage statistics
generate_ppc_plots <- function(bss_fit, inputs_bss, ecg, dwg, params) {
  
  # Get sections reference
  sections <- dwg$effort |>
    filter(location_type == "Section") |>
    distinct(water_body, section_num, location) |>
    arrange(section_num) |>
    select(water_body, section = section_num, location)
  
  # Helper function to create summary with totals
  add_summary_totals <- function(data, group_vars) {
    summary_data <- data |>
      group_by(across(all_of(group_vars))) |>
      summarise(
        fit_count = sum(fit == "fit"),
        no_fit_count = sum(fit == "no fit"),
        total_count = fit_count + no_fit_count,
        p_fit = if_else(no_fit_count == 0, 1, fit_count / total_count),
        .groups = "drop"
      )
    
    # Create total row
    total_row <- summary_data |>
      summarise(
        across(all_of(group_vars), ~"Total"),
        fit_count = sum(fit_count),
        no_fit_count = sum(no_fit_count),
        total_count = sum(total_count),
        p_fit = if_else(sum(no_fit_count) == 0, 1, sum(fit_count) / sum(total_count))
      )
    
    bind_rows(summary_data, total_row)
  }
  
  # V_I PPC
  V_I_PPC <- extract(bss_fit)$V_I_rep |>
    apply(2, \(x) quantile(x, c(0.025, 0.975))) |>
    t() |>
    as_tibble() |>
    rename(`2.5%` = 1, `97.5%` = 2) |>
    bind_cols(tibble(
      V_I = inputs_bss[[ecg]]$V_I,
      section = inputs_bss[[ecg]]$section_V,
      day = inputs_bss[[ecg]]$day_V,
      countnum = inputs_bss[[ecg]]$countnum_V
    )) |>
    left_join(sections, by = "section") |>
    mutate(
      section = as.factor(water_body),
      day = as.factor(day),
      countnum = as.factor(countnum),
      fit = factor(
        if_else(V_I >= `2.5%` & V_I <= `97.5%`, "fit", "no fit"), 
        levels = c("fit", "no fit")
      )
    )
  
  p_fit <- add_summary_totals(V_I_PPC, "section") |>
    mutate(Parameter = "V_I_PPC")
  
  V_I_PPC_plt <- ggplot(V_I_PPC, aes(x = V_I, y = V_I, color = fit)) +
    facet_grid(cols = vars(section)) +
    geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0, 
                  position = position_jitter(width = 0.5, height = 0)) +
    labs(
      x = "Observed Vehicle Counts", 
      y = "95% PPI",
      title = paste("Vehicle Counts PPC:", ecg)
    ) +
    geom_line(color = "black") +
    coord_cartesian(xlim = c(0, max(V_I_PPC$`97.5%`)), 
                    ylim = c(0, max(V_I_PPC$`97.5%`))) +
    theme_bw() +
    scale_colour_manual(values = c("#F8766D", "#00BFC4"), 
                        labels = c("fit", "no fit"), drop = FALSE)
  # invisible(
  # ggsave(
  #   filename = file.path(here("fishery_analyses", params$project_name, 
  #                             params$fishery_name), paste0("V_I_PPC_", ecg, ".png")),
  #   plot = V_I_PPC_plt, width = 6.5, height = 6.5, dpi = 300, 
  #   limitsize = TRUE, scale = 1.75, units = "in"
  # )
  # )
  
  # T_I PPC
  T_I_PPC <- extract(bss_fit)$T_I_rep |>
    apply(2, \(x) quantile(x, c(0.025, 0.975))) |>
    t() |>
    as_tibble() |>
    rename(`2.5%` = 1, `97.5%` = 2) |>
    bind_cols(tibble(
      T_I = inputs_bss[[ecg]]$T_I,
      section = inputs_bss[[ecg]]$section_T,
      day = inputs_bss[[ecg]]$day_T,
      countnum = inputs_bss[[ecg]]$countnum_T
    )) |>
    left_join(sections, by = "section") |>
    mutate(
      section = as.factor(water_body),
      day = as.factor(day),
      countnum = as.factor(countnum),
      fit = factor(
        if_else(T_I >= `2.5%` & T_I <= `97.5%`, "fit", "no fit"), 
        levels = c("fit", "no fit")
      )
    )
  
  p_fit <- p_fit |>
    bind_rows(
      add_summary_totals(T_I_PPC, "section") |>
        mutate(Parameter = "T_I_PPC")
    )
  
  T_I_PPC_plt <- ggplot(T_I_PPC, aes(x = T_I, y = T_I, color = fit)) +
    facet_grid(cols = vars(section)) +
    geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0, 
                  position = position_jitter(width = 0.5, height = 0)) +
    labs(
      x = "Observed Trailer Counts", 
      y = "95% PPI",
      title = paste("Trailer Counts PPC:", ecg)
    ) +
    geom_line(color = "black") +
    coord_cartesian(xlim = c(0, max(T_I_PPC$`97.5%`)), 
                    ylim = c(0, max(T_I_PPC$`97.5%`))) +
    theme_bw() +
    scale_colour_manual(values = c("#F8766D", "#00BFC4"), 
                        labels = c("fit", "no fit"), drop = FALSE)
  # invisible(
  # ggsave(
  #   filename = file.path(here("fishery_analyses", params$project_name, 
  #                             params$fishery_name), paste0("T_I_PPC_", ecg, ".png")),
  #   plot = T_I_PPC_plt, width = 6.5, height = 6.5, dpi = 300, 
  #   limitsize = TRUE, scale = 1.75, units = "in"
  # )
  # )
  
  # E_s PPC
  E_s_PPC <- extract(bss_fit)$E_s_rep |>
    apply(2, \(x) quantile(x, c(0.025, 0.975))) |>
    t() |>
    as_tibble() |>
    rename(`2.5%` = 1, `97.5%` = 2) |>
    bind_cols(tibble(
      E_s = inputs_bss[[ecg]]$E_s,
      section = inputs_bss[[ecg]]$section_E,
      day = inputs_bss[[ecg]]$day_E,
      gear = inputs_bss[[ecg]]$gear_E,
      countnum = inputs_bss[[ecg]]$countnum_E
    )) |>
    left_join(sections, by = "section") |>
    mutate(
      section = as.factor(water_body),
      day = as.factor(day),
      gear = as.factor(case_match(
        gear,
        1 ~ "Bank Anglers",
        2 ~ "Boat Anglers",
        .default = as.character(gear)
      )),
      countnum = as.factor(countnum),
      fit = factor(
        if_else(E_s >= `2.5%` & E_s <= `97.5%`, "fit", "no fit"), 
        levels = c("fit", "no fit")
      )
    )
  
  p_fit <- p_fit |>
    bind_rows(
      add_summary_totals(E_s_PPC, c("section", "gear")) |>
        mutate(Parameter = "E_s_PPC")
    )
  
  E_s_PPC_plt <- ggplot(E_s_PPC, aes(x = E_s, y = E_s, color = fit)) +
    facet_grid(rows = vars(gear), cols = vars(section)) +
    geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0, 
                  position = position_jitter(width = 0.5, height = 0)) +
    labs(
      x = "Observed Angler Counts", 
      y = "95% PPI",
      title = paste("Angler Counts PPC:", ecg)
    ) +
    geom_line(color = "black") +
    coord_cartesian(xlim = c(0, max(E_s_PPC$`97.5%`)), 
                    ylim = c(0, max(E_s_PPC$`97.5%`))) +
    theme_bw() +
    scale_colour_manual(values = c("#F8766D", "#00BFC4"), 
                        labels = c("fit", "no fit"), drop = FALSE)
  # invisible(
  # ggsave(
  #   filename = file.path(here("fishery_analyses", params$project_name, 
  #                             params$fishery_name), paste0("E_s_PPC_", ecg, ".png")),
  #   plot = E_s_PPC_plt, width = 6.5, height = 6.5, dpi = 300, 
  #   limitsize = TRUE, scale = 1.75, units = "in"
  # )
  # )
  
  # c PPC
  c_PPC <- extract(bss_fit)$c_rep |>
    apply(2, \(x) quantile(x, c(0.025, 0.975))) |>
    t() |>
    as_tibble() |>
    rename(`2.5%` = 1, `97.5%` = 2) |>
    bind_cols(tibble(
      c = inputs_bss[[ecg]]$c,
      section = inputs_bss[[ecg]]$section_IntC,
      day = inputs_bss[[ecg]]$day_IntC,
      gear = inputs_bss[[ecg]]$gear_IntC
    )) |>
    left_join(sections, by = "section") |>
    mutate(
      section = as.factor(water_body),
      day = as.factor(day),
      gear = as.factor(case_match(
        gear,
        1 ~ "Bank Anglers",
        2 ~ "Boat Anglers",
        .default = as.character(gear)
      )),
      fit = factor(
        if_else(c >= `2.5%` & c <= `97.5%`, "fit", "no fit"),
        levels = c("fit", "no fit")
      )
    )
  
  p_fit <- p_fit |>
    bind_rows(
      add_summary_totals(c_PPC, c("section", "gear")) |>
        mutate(Parameter = "c_PPC")
    )
  
  c_PPC_plt <- ggplot(c_PPC, aes(x = c + 1, y = c + 1, color = fit)) +
    facet_grid(rows = vars(gear), cols = vars(section)) +
    geom_errorbar(aes(ymin = `2.5%` + 1, ymax = `97.5%` + 1), width = 0, 
                  position = position_jitter(width = 0.2, height = 0)) +
    labs(
      x = "Observed Catch", 
      y = "95% PPI",
      title = paste("Catch PPC:", ecg)
    ) +
    geom_line(color = "black") +
    scale_y_continuous(breaks = seq(1, 10, by = 1)) +
    scale_x_continuous(breaks = seq(1, 10, by = 1)) +
    theme_bw()
  
  # invisible(
  # ggsave(
  #   filename = file.path(here("fishery_analyses", params$project_name, 
  #                             params$fishery_name), paste0("c_PPC_", ecg, ".png")),
  #   plot = c_PPC_plt, width = 6.5, height = 6.5, dpi = 300, 
  #   limitsize = TRUE, scale = 1.75, units = "in"
  # )
  # )
  
  # V_A PPC
  V_A_PPC <- extract(bss_fit)$V_A_rep |>
    apply(2, \(x) quantile(x, c(0.025, 0.975))) |>
    t() |>
    as_tibble() |>
    rename(`2.5%` = 1, `97.5%` = 2) |>
    bind_cols(tibble(
      V_A = inputs_bss[[ecg]]$V_A,
      gear = inputs_bss[[ecg]]$gear_IntA
    )) |>
    mutate(
      gear = as.factor(case_match(
        gear,
        1 ~ "Bank Anglers",
        2 ~ "Boat Anglers",
        .default = as.character(gear)
      )),
      fit = factor(
        if_else(V_A >= `2.5%` & V_A <= `97.5%`, "fit", "no fit"), 
        levels = c("fit", "no fit")
      )
    )
  
  p_fit <- p_fit |>
    bind_rows(
      add_summary_totals(V_A_PPC, "gear") |>
        mutate(Parameter = "V_A_PPC")
    )
  
  V_A_PPC_plt <- ggplot(V_A_PPC, aes(x = V_A, y = V_A, color = fit)) +
    facet_grid(cols = vars(gear)) +
    geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0, 
                  position = position_jitter(width = 0.2, height = 0)) +
    labs(
      x = "Observed Angler Group Vehicles", 
      y = "95% PPI",
      title = paste("Angler Group Vehicles PPC:", ecg)
    ) +
    geom_line(color = "black") +
    coord_cartesian(xlim = c(0, max(V_A_PPC$`97.5%`)), 
                    ylim = c(0, max(V_A_PPC$`97.5%`))) +
    theme_bw() +
    scale_colour_manual(values = c("#F8766D", "#00BFC4"), 
                        labels = c("fit", "no fit"), drop = FALSE)
  # invisible(
  # ggsave(
  #   filename = file.path(here("fishery_analyses", params$project_name, 
  #                             params$fishery_name), paste0("V_A_PPC_", ecg, ".png")),
  #   plot = V_A_PPC_plt, width = 6.5, height = 6.5, dpi = 300, 
  #   limitsize = TRUE, scale = 1.75, units = "in"
  # )
  # )
  # T_A PPC
  T_A_PPC <- extract(bss_fit)$T_A_rep |>
    apply(2, \(x) quantile(x, c(0.025, 0.975))) |>
    t() |>
    as_tibble() |>
    rename(`2.5%` = 1, `97.5%` = 2) |>
    bind_cols(tibble(
      T_A = inputs_bss[[ecg]]$T_A,
      gear = inputs_bss[[ecg]]$gear_IntA
    )) |>
    mutate(
      gear = as.factor(case_match(
        gear,
        1 ~ "Bank Anglers",
        2 ~ "Boat Anglers",
        .default = as.character(gear)
      )),
      fit = factor(
        if_else(T_A >= `2.5%` & T_A <= `97.5%`, "fit", "no fit"), 
        levels = c("fit", "no fit")
      )
    )
  
  p_fit <- p_fit |>
    bind_rows(
      add_summary_totals(T_A_PPC, "gear") |>
        mutate(Parameter = "T_A_PPC")
    )
  
  T_A_PPC_plt <- ggplot(T_A_PPC, aes(x = T_A, y = T_A, color = fit)) +
    facet_grid(cols = vars(gear)) +
    geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0, 
                  position = position_jitter(width = 0.2, height = 0)) +
    labs(
      x = "Observed Angler Group Trailers", 
      y = "95% PPI",
      title = paste("Angler Group Trailers PPC:", ecg)
    ) +
    geom_line(color = "black") +
    coord_cartesian(xlim = c(0, max(T_A_PPC$`97.5%`)), 
                    ylim = c(0, max(T_A_PPC$`97.5%`))) +
    theme_bw() +
    scale_colour_manual(values = c("#F8766D", "#00BFC4"), 
                        labels = c("fit", "no fit"), drop = FALSE)
  # invisible(
  # ggsave(
  #   filename = file.path(here("fishery_analyses", params$project_name, 
  #                             params$fishery_name), paste0("T_A_PPC_", ecg, ".png")),
  #   plot = T_A_PPC_plt, width = 6.5, height = 6.5, dpi = 300, 
  #   limitsize = TRUE, scale = 1.75, units = "in"
  # )
  # )
  
  # Combined plots
  p_combined <- gridExtra::arrangeGrob(V_I_PPC_plt, T_I_PPC_plt, nrow = 2)
  p_combined2 <- gridExtra::arrangeGrob(V_A_PPC_plt, T_A_PPC_plt, nrow = 2)
  
  # invisible(
  # ggsave(
  #   filename = file.path(here("fishery_analyses", params$project_name, 
  #                             params$fishery_name), paste0("VI_TI_PPC_", ecg, ".png")),
  #   plot = p_combined, width = 6.5, height = 6.5, dpi = 300, 
  #   limitsize = TRUE, scale = 1.75, units = "in"
  # )
  # )
  # 
  # invisible(
  # ggsave(
  #   filename = file.path(here("fishery_analyses", params$project_name, 
  #                             params$fishery_name), paste0("VA_TA_PPC_", ecg, ".png")),
  #   plot = p_combined2, width = 6.5, height = 6.5, dpi = 300, 
  #   limitsize = TRUE, scale = 1.75, units = "in"
  # )
  # )
  
  # Save p_fit table
  # p_fit |>
  #   select(
  #     Parameter,
  #     Section = section,
  #     Gear = gear,
  #     `Fit count` = fit_count,
  #     `No fit count` = no_fit_count,
  #     `Total n` = total_count,
  #     `2.5% Q > Pr.(obs) < 97.5% Q` = p_fit
  #   ) |>
  #   write.csv(
  #     file.path(here("fishery_analyses", params$project_name, 
  #                    params$fishery_name), paste0("p_fit_", ecg, ".csv")), 
  #     row.names = FALSE
  #   )
  
  # Return list of results
  list(
    p_fit = p_fit,
    plots = list(
      V_I_PPC_plt = V_I_PPC_plt,
      T_I_PPC_plt = T_I_PPC_plt,
      E_s_PPC_plt = E_s_PPC_plt,
      c_PPC_plt = c_PPC_plt,
      V_A_PPC_plt = V_A_PPC_plt,
      T_A_PPC_plt = T_A_PPC_plt
    )
  )
}