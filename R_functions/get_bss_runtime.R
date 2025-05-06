get_bss_runtime <- function(stan_fit, output_file) {
  # Get sampling time
  raw_times <- rstan::get_elapsed_time(stan_fit)
  chain_times <- as.data.frame(raw_times)
  chain_times$chain <- paste0("Chain ", seq_len(nrow(chain_times)))
  chain_times$total <- rowSums(chain_times[, c("warmup", "sample")])
  
  # Convert to minutes
  chain_times_min <- chain_times |>
    dplyr::mutate(
      dplyr::across(c(warmup, sample, total), ~ round(.x / 60, 1))
    )
  
  total_elapsed <- max(chain_times$total) / 60
  
  # --- Console output ---
  cli::cli_h1("Stan model sampling summary")
  cli::cli_text("Runtime in minutes")
  
  for (i in seq_len(nrow(chain_times_min))) {
    row <- chain_times_min[i, ]
    cli::cli_li(
      "{.strong {row$chain}} — Warmup: {row$warmup} | Sample: {row$sample} | Total: {row$total}"
    )
  }
  
  cli::cli_alert_info("Total elapsed time (longest chain): {.val {round(total_elapsed, 1)}} minutes")
  
  # --- Write output file ---
  summary_str <- sprintf("Stan model sampling summary — %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  summary_str <- c(summary_str, "Runtime in minutes:")
  
  for (i in seq_len(nrow(chain_times_min))) {
    row <- chain_times_min[i, ]
    summary_str <- c(
      summary_str,
      sprintf(
        "• %s — Warmup: %.1f | Sample: %.1f | Total: %.1f",
        row$chain, row$warmup, row$sample, row$total
      )
    )
  }
  
  summary_str <- c(
    summary_str,
    sprintf("Total elapsed time (longest chain): %.1f minutes", total_elapsed)
  )
  
  writeLines(summary_str, output_file)
  
  invisible(list(
    timing_table = chain_times_min,
    total_elapsed_min = round(total_elapsed, 1)
  ))
}

get_bss_runtime(ecg_fit, output_file = "text2.txt")
