#DataExplorer::plot_missing() function modified for specific use-case
#Accessed: 10/11/2023, Modified 10/11/2023 CH.
#Link to source: https://github.com/boxuancui/DataExplorer/blob/master/R/plot_missing.r
#Edit source: https://stackoverflow.com/questions/55941265/how-to-change-colors-and-band-labels-in-data-explorers-plot-missing-function
plot_freq_missing <- function(data,
                         group = list("Good" = 0.05, "Okay" = 0.2, "Poor" = 0.6, "Scarce" = 1),
                         missing_only = FALSE,
                         geom_label_args = list(),
                         title = NULL,
                         ggtheme = theme_gray(),
                         theme_config = list("legend.position" = c("bottom"))) {
  ## Declare variable first to pass R CMD check
  num_missing <- pct_missing <- Band <- NULL
  ## Profile missing values
  missing_value <- data.table(profile_missing(data))
  if (missing_only) missing_value <- missing_value[num_missing > 0]
  ## Sort group based on value
  group <- group[sort.list(unlist(group))]
  invisible(lapply(seq_along(group), function(i) {
    if (i == 1) {
      missing_value[pct_missing <= group[[i]], Band := names(group)[i]]
    } else {
      missing_value[pct_missing > group[[i-1]] & pct_missing <= group[[i]], Band := names(group)[i]]
    }
  }))
  
  # Determine ordinal levels from group supplied
  ordinal_levels <- names(group[sort.list(unlist(group))])
  
  # Convert character to ordered factor to support ordinal legend
  missing_value[, Band := factor(Band,
                                 levels=ordinal_levels,
                                 ordered = T)]
  
  ## Create ggplot object
  output <- ggplot(missing_value, aes_string(x = "feature", y = "num_missing", fill = "Band")) +
    geom_bar(stat = "identity") +
    scale_fill_manual("Band",
                      values = c("Good" = "green2","Okay" = "gold",
                      "Poor" = "darkorange", "Scarce" = "firebrick2")) +
    coord_flip() +
    xlab("Features") + ylab("Missing Rows") +
    guides(fill = guide_legend(override.aes = aes(label = "")))
  geom_label_args_list <- list("mapping" = aes(label = paste0(round(100 * pct_missing, 2), "%")))
  output <- output +
    do.call("geom_label", c(geom_label_args_list, geom_label_args))
  ## Plot object
  class(output) <- c("single", class(output))
  plotDataExplorer(
    plot_obj = output,
    title = title,
    ggtheme = ggtheme,
    theme_config = theme_config
  )
}