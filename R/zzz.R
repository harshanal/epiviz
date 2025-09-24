.onLoad <- function(...){
  quietly <- getOption('quietly')
  options(quietly = T)

  options(showWarnCalls=F)
  options(quietly = quietly)

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


