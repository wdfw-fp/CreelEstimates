# Function 1: Calculate p-values for posterior predictive checks
calculate_ppc_pvalues <- function(bss_fit, inputs_bss, ecg) {
  ecg_fit <- bss_fit
  # V_I p-value
  V_I_pvalue <- extract(ecg_fit)$V_I_rep |>
    as_tibble() |>
    mutate(iterations = row_number()) |>
    pivot_longer(
      cols = -iterations,
      names_to = "V_n",
      values_to = "value"
    ) |>
    mutate(V_n = as.integer(str_remove(V_n, "V"))) |>
    left_join(
      tibble(
        V_I = inputs_bss[[ecg]]$V_I,
        V_n = seq_along(inputs_bss[[ecg]]$V_I)
      ),
      by = "V_n"
    ) |>
    group_by(iterations) |>
    summarise(
      mean_gt = if_else(mean(value) > mean(V_I), 1, 0),
      sd_gt = if_else(sd(value) > sd(V_I), 1, 0),
      .groups = "drop"
    ) |>
    summarise(
      mean_gt = sum(mean_gt) / n(),
      sd_gt = sum(sd_gt) / n()
    )
  
  # T_I p-value
  T_I_pvalue <- extract(ecg_fit)$T_I_rep |>
    as_tibble() |>
    mutate(iterations = row_number()) |>
    pivot_longer(
      cols = -iterations,
      names_to = "T_n",
      values_to = "value"
    ) |>
    mutate(T_n = as.integer(str_remove(T_n, "V"))) |>
    left_join(
      tibble(
        T_I = inputs_bss[[ecg]]$T_I,
        T_n = seq_along(inputs_bss[[ecg]]$T_I)
      ),
      by = "T_n"
    ) |>
    group_by(iterations) |>
    summarise(
      mean_gt = if_else(mean(value) > mean(T_I), 1, 0),
      sd_gt = if_else(sd(value) > sd(T_I), 1, 0),
      .groups = "drop"
    ) |>
    summarise(
      mean_gt = sum(mean_gt) / n(),
      sd_gt = sum(sd_gt) / n()
    )
  
  # E_s p-value
  E_s_pvalue <- extract(ecg_fit)$E_s_rep |>
    as_tibble() |>
    mutate(iterations = row_number()) |>
    pivot_longer(
      cols = -iterations,
      names_to = "E_n",
      values_to = "value"
    ) |>
    mutate(E_n = as.integer(str_remove(E_n, "V"))) |>
    left_join(
      tibble(
        E_s = inputs_bss[[ecg]]$E_s,
        E_n = seq_along(inputs_bss[[ecg]]$E_s)
      ),
      by = "E_n"
    ) |>
    group_by(iterations) |>
    summarise(
      mean_gt = if_else(mean(value) > mean(E_s), 1, 0),
      sd_gt = if_else(sd(value) > sd(E_s), 1, 0),
      .groups = "drop"
    ) |>
    summarise(
      mean_gt = sum(mean_gt) / n(),
      sd_gt = sum(sd_gt) / n()
    )
  
  # c p-value
  c_pvalue <- extract(ecg_fit)$c_rep |>
    as_tibble() |>
    mutate(iterations = row_number()) |>
    pivot_longer(
      cols = -iterations,
      names_to = "IntC",
      values_to = "value"
    ) |>
    mutate(IntC = as.integer(str_remove(IntC, "V"))) |>
    left_join(
      tibble(
        c = inputs_bss[[ecg]]$c,
        IntC = seq_along(inputs_bss[[ecg]]$c)
      ),
      by = "IntC"
    ) |>
    group_by(iterations) |>
    summarise(
      mean_gt = if_else(mean(value) > mean(c), 1, 0),
      sd_gt = if_else(sd(value) > sd(c), 1, 0),
      .groups = "drop"
    ) |>
    summarise(
      mean_gt = sum(mean_gt) / n(),
      sd_gt = sum(sd_gt) / n()
    )
  
  # V_A p-value
  V_A_pvalue <- extract(ecg_fit)$V_A_rep |>
    as_tibble() |>
    mutate(iterations = row_number()) |>
    pivot_longer(
      cols = -iterations,
      names_to = "IntA",
      values_to = "value"
    ) |>
    mutate(IntA = as.integer(str_remove(IntA, "V"))) |>
    left_join(
      tibble(
        V_A = inputs_bss[[ecg]]$V_A,
        IntA = seq_along(inputs_bss[[ecg]]$V_A)
      ),
      by = "IntA"
    ) |>
    group_by(iterations) |>
    summarise(
      mean_gt = if_else(mean(value) > mean(V_A), 1, 0),
      sd_gt = if_else(sd(value) > sd(V_A), 1, 0),
      .groups = "drop"
    ) |>
    summarise(
      mean_gt = sum(mean_gt) / n(),
      sd_gt = sum(sd_gt) / n()
    )
  
  # T_A p-value
  T_A_pvalue <- extract(ecg_fit)$T_A_rep |>
    as_tibble() |>
    mutate(iterations = row_number()) |>
    pivot_longer(
      cols = -iterations,
      names_to = "IntA",
      values_to = "value"
    ) |>
    mutate(IntA = as.integer(str_remove(IntA, "V"))) |>
    left_join(
      tibble(
        T_A = inputs_bss[[ecg]]$T_A,
        IntA = seq_along(inputs_bss[[ecg]]$T_A)
      ),
      by = "IntA"
    ) |>
    group_by(iterations) |>
    summarise(
      mean_gt = if_else(mean(value) > mean(T_A), 1, 0),
      sd_gt = if_else(sd(value) > sd(T_A), 1, 0),
      .groups = "drop"
    ) |>
    summarise(
      mean_gt = sum(mean_gt) / n(),
      sd_gt = sum(sd_gt) / n()
    )
  
  # Return list of all p-values
  list(
    V_I_pvalue = V_I_pvalue,
    T_I_pvalue = T_I_pvalue,
    E_s_pvalue = E_s_pvalue,
    c_pvalue = c_pvalue,
    V_A_pvalue = V_A_pvalue,
    T_A_pvalue = T_A_pvalue
  )
}
