.onLoad <- function(...){
  # Store original options
  quietly <- getOption('quietly')
  showWarnCalls <- getOption('showWarnCalls')
  
  # Restore options when function exits
  on.exit({
    options(quietly = quietly)
    options(showWarnCalls = showWarnCalls)
  })
  
  # Temporarily change options
  options(quietly = TRUE)
  options(showWarnCalls = FALSE)
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