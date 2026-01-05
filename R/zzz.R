.onLoad <- function(...){
  # Save current option values
  old_quietly <- getOption('quietly')
  old_showWarnCalls <- getOption('showWarnCalls')

  # Restore options on exit (CRAN requirement)
  on.exit(options(quietly = old_quietly, showWarnCalls = old_showWarnCalls), add = TRUE)

  # Set temporary options
  options(quietly = TRUE, showWarnCalls = FALSE)

}



utils::globalVariables(c(
  "x_time_series",
  "bar_labels_percent",
  "cumul",
  "lower_lim_stacked",
  "upper_lim_stacked",
  "axis_flip",
  "bar_labels_pos",
  "bar_labels_font_colour",
  "bar_labels_font_size",
  "ci_upper_diff",
  "fill_colour",
  "bar_order",
  "date_sum",
  "x_time_series_bar_labels"
))


