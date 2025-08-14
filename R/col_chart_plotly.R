#' col_chart
#'
#' @description A function for producing either a static (ggplot) or dynamic (plotly)
#' column chart.
#'
#' @param dynamic Logical indicating whether to produce a dynamic (plotly) output.
#' Default is \code{FALSE}, which will return a static ggplot output.
#' @param params A named list containing arguements used to create the plot.
#' \describe{
#'    \item{df}{A data frame containing data used to create the epi curve.}
#'    \item{date_var}{character, Name of the variable in \code{df} containing the dates used
#'    to populate the x-axis.}
#'    \item{y}{If data is pre-aggregated, the name of the variable in \code{df} containing the
#'    aggregated values (i.e. the values used to populate the y-axis.)}
#'    \item{date_start}{A date that will determine the minimum value along the x-axis. Any rows
#'    with \code{date_var < date_start} will be excluded from aggregates.}
#'    \item{date_end}{A date that will determine the maximum value along the x-axis. Any rows
#'    with \code{date_var > date_end} will be excluded from aggregates.}
#'    \item{time_period}{The time period to be used along the x-axis. Options include
#'    \code{c("day","year","month","quarter","year_month","year_quarter",
#'      "iso_year","iso_week","start_iso_year_week","iso_year_week")}. Default = \code{"day"}}
#'    \item{group_var}{Name of the variable in df used to define separate groups within each bar,
#'    e.g. species or region.}
#'    \item{group_var_barmode}{Indicates how grouped bar data should be plotted. Options include
#'    \code{c("group","stack")}. Default = \code{"stack"}.}
#'    \item{fill_colours}{Colours used to fill bars on chart. If \code{group_var} has not been
#'    provided, then \code{fill_colours} must be a character containing a single colour (default =
#'    \code{"lightblue"}). If \code{group_var} has been provided, then \code{fill_colours} must be
#'    a character vector of colours with a number of elements equal to the number of unique groups
#'    in \code{group_var}. If a named character vector is provided where the names are values within
#'    \code{group_var}, then each colour will be mapped to it's corresponding value in \code{group_var}
#'    on the output chart and legend (e.g. \code{c("KLEBSIELLA PNEUMONIAE" = "#007C91", "STAPHYLOCOCCUS
#'    AUREUS" = "#8A1B61", "PSEUDOMONAS AERUGINOSA" = "#FF7F32")} or \code{setNames(c("#007C91",
#'    "#8A1B61","#FF7F32"), c("KLEBSIELLA PNEUMONIAE","STAPHYLOCOCCUS AUREUS","PSEUDOMONAS AERUGINOSA"))})}
#'    \item{bar_border_colour}{Colour of the border around each bar. No border colour is drawn as default.}
#'    \item{bar_labels}{character, Name of the variable in \code{df} containing the labels to be used
#'    for each bar.}
#'    \item{bar_labels_pos}{character, The position on the bars that labels will be plotted, permitted
#'    values are \code{c('bar_above','bar_base','bar_centre','above_errorbar')} default = \code{'bar_top'}.}
#'    \item{bar_labels_percent}{cboolean, If \code{bar_labels_percent = TRUE} then the values in \code{bar_labels}
#'    will be converted into a percentage before plotting.}
#'    \item{case_boxes}{boolean, If \code{case_boxes = TRUE} then a boundary box will be drawn around
#'    each case within each bar. Defaults to \code{case_boxes = FALSE}.}
#'    \item{case_boxes_colour}{The colour of the border around each case box if if \code{case_boxes =
#'    TRUE}. Default = \code{"white"}.}
#'    \item{chart_title}{Text to use as chart title.}
#'    \item{chart_title_size}{Font size of chart title.}
#'    \item{chart_title_colour}{Font colour of chart title.}
#'    \item{chart_footer}{Text to use as chart footer.}
#'    \item{chart_footer_size}{Font size of chart footer.}
#'    \item{chart_footer_colour}{Font colour of chart footer.}
#'    \item{x_axis_title}{Text used for x-axis title. Defaults to name of x-variable if
#'    not stated.}
#'    \item{y_axis_title}{Text used for y-axis title. Defaults to name of y-variable if
#'    not stated.}
#'    \item{x_axis_title_font_size}{Font size of the x-axis title.}
#'    \item{y_axis_title_font_size}{Font size of the y-axis title.}
#'    \item{x_axis_label_angle}{Angle for x-axis label text.}
#'    \item{y_axis_label_angle}{Angle for y-axis label text.}
#'    \item{x_axis_label_font_size}{Font size for the x-axis tick labels.}
#'    \item{y_axis_label_font_size}{Font size for the y-axis tick labels.}
#'    \item{y_limit_min}{Lower limit for the y-axis. Default used if not provided.}
#'    \item{y_limit_max}{Upper limit for the y-axis. Default used if not provided.}
#'    \item{x_axis_break_labels}{Vector of values to use for x-axis breaks. Defaults
#'    used if not provided. Values provided must match the formatting of \code{time_period}.}
#'    \item{y_axis_break_labels}{Vector of values to use for y-axis breaks. Defaults
#'    used if not provided.}
#'    \item{y_axis_n_breaks}{Scales y-axis with approximately n breaks. Cannot be used
#'    if \code{y_axis_break_labels} is also provided.}
#'    \item{show_gridlines}{Logical to show chart gridlines. Default = \code{TRUE}.}
#'    \item{show_axislines}{Logical to show chart axis lines. Default = \code{TRUE}.}
#'    \item{legend_title}{Text used for legend title.}
#'    \item{legend_pos}{Position of the legend. Permitted values = c("top","bottom","right","left")}
#'    \item{legend_font_size}{Font size used in the legend.}
#'    \item{legend_title_font_size}{Font size used for the legend title.}
#'    \item{hline}{Adds horizontal line across the chart at the corresponding y-value. Multiple
#'    values may be provided as a vector to add multiple horizontal lines.}
#'    \item{hline_colour}{Colour of the horizontal lines if \code{hline} is provided. A vector of colours
#'    can be provided to colour individual hlines if multiple hlines have been provided.}
#'    \item{hline_width}{Numerical width of the horizontal lines if \code{hline} is provided. A vector of numerical widths
#'    can be provided for individual hlines if multiple hlines have been provided.}
#'    \item{hline_type}{Line style of the horizontal lines if \code{hline} is provided. A vector of line styles
#'    can be provided to style hlines if multiple hlines have been provided. Permitted values = c("solid", "dotted",
#'    "dashed", "longdash", "dotdash").}
#'    \item{hline_label}{Text to label the horizontal lines if \code{hline} is provided. A vector of text strings
#'    can be provided to label individual hlines if multiple hlines have been provided.}
#'    \item{hline_label_colour}{Colour of the horizontal line labels if \code{hline_labels} is provided.
#'    A vector of colours can be provided to colour individual hline_labels if multiple hline_labels have been provided.}
#'    \item{hover_labels}{string, Text to be used in the hover-over labels in a dynamic chart.
#'    Accepts html, use \code{'\%{x}'} to reference corresponding x-axis values (i.e. date intervals)
#'    and \code{'\%{y}'} to reference y-axis values, e.g. \code{hover_labels = "<b>Date:</b>
#'    \%{x}<br><b>Count:</b> \%{y}"}.}
#'
#'  }
#'
#'
#'
#' @import dplyr
#' @import grDevices
#' @import scales
#' @import tidyr
#' @import lubridate
#' @import ISOweek
#' @import forcats
#' @import slider
#' @importFrom graphics box
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @return A ggplot or plotly object.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Example 1: Basic epi curve
#'
#' # Create a basic epi curve using the epiviz::lab_data dataset
#' library(epiviz)
#'
#' basic_epi_curve <- epi_curve(
#'   params = list(
#'     df = lab_data,
#'     date_var = "specimen_date",
#'     date_start = "2020-01-01",
#'     date_end = "2023-12-31",
#'     time_period = "year_month",
#'     fill_colours = "#007C91",
#'     rolling_average_line = TRUE,
#'     rolling_average_line_lookback = 3,
#'     rolling_average_line_legend_label = "3-month rolling average",
#'     chart_title = "Laboratory Detections per Month",
#'     x_axis_title = "Year - Month",
#'     y_axis_title = "Number of detections",
#'     x_axis_label_angle = -90
#'   )
#' )
#'
#' basic_epi_curve
#'
#'
#'
#'
#' # Example 2: Create both static and dynamic epi curves using grouped data
#'
#' library(epiviz)
#'
#' # Define list of date breaks for x-axis; use every other ISO week in date range
#' week_seq <- seq(as.Date("2021-01-01"),as.Date("2022-05-31"), by = '2 week')
#' week_breaks <- paste0(lubridate::isoyear(week_seq),'-W',lubridate::isoweek(week_seq))
#'
#' # Create parameter list
#' params_list <- list(
#'   df = lab_data,
#'   date_var = "specimen_date",
#'   date_start = "2021-01-01",
#'   date_end = "2022-05-31",
#'   time_period = "iso_year_week",
#'   group_var = "organism_species_name",
#'   group_var_barmode = "stack",
#'   fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
#'                    "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
#'                    "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
#'   rolling_average_line = TRUE,
#'   rolling_average_line_legend_label = "7-week rolling average",
#'   chart_title = "Laboratory detections by species \n 2021-01 - 2022-05",
#'   chart_footer = "This chart has been created using simulated data.",
#'   x_axis_title = "Year - ISO Week",
#'   y_axis_title = "Number of detections",
#'   x_axis_label_angle = -90,
#'   x_axis_break_labels = week_breaks,
#'   y_axis_break_labels = seq(0, 250, 20),
#'   chart_title_colour = "#007C91",
#'   chart_footer_colour = "#007C91"
#' )
#'
#' # Create static epi curve
#' static_curve <- epi_curve(params = params_list, dynamic = FALSE)
#'
#' # Create dynamic epi curve
#' dynamic_curve <- epi_curve(params = params_list, dynamic = TRUE)
#'
#' # View both simultaneously using shiny app
#' library(shiny)
#' library(plotly)
#' ui <- fluidPage(
#'   plotOutput('static_curve'),
#'   plotlyOutput('dynamic_curve')
#' )
#' server <- function(input, output, session) {
#'   output$static_curve <- renderPlot(static_curve)
#'   output$dynamic_curve <- renderPlotly(dynamic_curve)
#' }
#' shinyApp(ui, server)
#'
#'
#'
#'
#' # Example 3: Create static and dynamic curves using grouped data, include cumulative
#' # sum line and boxes around each case
#'
#' library(epiviz)
#'
#' # Create parameter list
#' params_cases <- list(
#'   df = lab_data,
#'   date_var = "specimen_date",
#'   date_start = "2021-06-01",
#'   date_end = "2021-07-31",
#'   time_period = "day",
#'   group_var = "organism_species_name",
#'   group_var_barmode = "stack",
#'   fill_colours = c("#007C91","#8A1B61","#FF7F32"),
#'   case_boxes = TRUE,
#'   rolling_average_line = TRUE,
#'   rolling_average_line_legend_label = "7-day rolling average",
#'   cumulative_sum_line = TRUE,
#'   chart_title = "Laboratory detections by species \n June - July 2021",
#'   chart_title_colour = "#007C91",
#'   hline = c(35),
#'   hline_label = "Threshold",
#'   hline_width = 0.5,
#'   hline_colour = "orange",
#'   hline_label_colour = "orange",
#'   hline_type = "dotdash",
#'   legend_title = "Detected organisms",
#'   legend_pos = "right",
#'   y_limit_max = 40,
#'   x_axis_break_labels = as.character(seq(as.Date("2021-06-01"),
#'                                          as.Date("2021-07-31"),
#'                                          by = '2 days')),
#'   y_axis_break_labels = seq(0, 40, 5),
#'   x_axis_title = "Date",
#'   y_axis_title = "Number of detections",
#'   x_axis_label_angle = -90,
#'   y_axis_label_angle = 90
#' )
#'
#'
#' # Create static and dynamic curves
#' static_curve <- epi_curve(params = params_cases, dynamic = FALSE)
#' dynamic_curve <- epi_curve(params = params_cases, dynamic = TRUE)
#'
#' # View both simultaneously using shiny app
#' library(shiny)
#' library(plotly)
#' ui <- fluidPage(
#'   plotOutput('static_curve'),
#'   plotlyOutput('dynamic_curve')
#' )
#' server <- function(input, output, session) {
#'   output$static_curve <- renderPlot(static_curve)
#'   output$dynamic_curve <- renderPlotly(dynamic_curve)
#' }
#' shinyApp(ui, server)
#'
#'
#'
#'
#' # Example 4: Create static and dynamic curves using pre-aggregated data
#'
#' library(epiviz)
#'
#' # Define a dataframe containing the number of detections by region
#' regional_detections <- lab_data |>
#'   group_by(specimen_date, region) |>
#'   summarise(detections = n()) |>
#'   ungroup()
#'
#'
#' # Create parameter list
#' params_regions <- list(
#'   df = regional_detections,
#'   y = "detections",
#'   date_var = "specimen_date",
#'   date_start = "2021-10-01",
#'   date_end = "2022-03-31",
#'   time_period = "iso_year_week",
#'   group_var = "region",
#'   group_var_barmode = "stack",
#'   rolling_average_line = TRUE,
#'   rolling_average_line_lookback = 3,
#'   rolling_average_line_legend_label = "3-week rolling average",
#'   rolling_average_line_colour = "#007C91",
#'   rolling_average_line_width = 1.5,
#'   cumulative_sum_line = TRUE,
#'   cumulative_sum_line_colour = "orange",
#'   chart_title = "Laboratory Detections by Region \nWinter 2021-22",
#'   chart_title_colour = "#007C91",
#'   legend_title = "Region",
#'   legend_pos = "right",
#'   y_axis_break_labels = seq(0, 300, 50),
#'   x_axis_title = "ISO Week",
#'   y_axis_title = "Number of detections",
#'   x_axis_label_angle = -90,
#'   hover_labels = "<b>Week:</b> %{x}<br><b>Count:</b> %{y}"
#' )
#'
#'
#' # Create static and dynamic curves
#' static_curve <- epi_curve(params = params_regions, dynamic = FALSE)
#' dynamic_curve <- epi_curve(params = params_regions, dynamic = TRUE)
#'
#' # View both simultaneously using shiny app
#' library(shiny)
#' library(plotly)
#' ui <- fluidPage(
#'   plotOutput('static_curve'),
#'   plotlyOutput('dynamic_curve')
#' )
#' server <- function(input, output, session) {
#'   output$static_curve <- renderPlot(static_curve)
#'   output$dynamic_curve <- renderPlotly(dynamic_curve)
#' }
#' shinyApp(ui, server)
#'
#'
#' }
#'
col_chart <- function(
    dynamic = FALSE,
    params = list(
      df = NULL,
      x = NULL,
      y = NULL,
      x_time_series = FALSE,
      time_period = "day",
      group_var = NULL,
      group_var_barmode = 'stack',
      fill_colours = "lightblue",
      bar_border_colour = "transparent",
      bar_labels = NULL,
      bar_labels_pos = 'bar_above',
      bar_labels_font_size = 8,
      bar_labels_font_colour = 'black',
      bar_labels_angle = 0,
      bar_labels_percent = FALSE,
      case_boxes = FALSE,
      case_boxes_colour = "white",
      axis_flip = FALSE,
      ci = NULL,
      ci_upper = NULL,
      ci_lower = NULL,
      ci_legend = TRUE,
      ci_legend_title = "Confidence interval",
      ci_colours = "red",
      errorbar_width = NULL,
      chart_title = NULL,
      chart_title_size = 13,
      chart_title_colour = "black",
      chart_footer = NULL,
      chart_footer_size = 12,
      chart_footer_colour = "black",
      x_axis_title = NULL,
      y_axis_title = NULL,
      x_axis_title_font_size = 11,
      y_axis_title_font_size = 11,
      x_axis_label_angle = NULL,
      y_axis_label_angle = NULL,
      x_axis_label_font_size = 9,
      y_axis_label_font_size = 9,
      x_limit_min = NULL,
      x_limit_max = NULL,
      y_limit_min = NULL,
      y_limit_max = NULL,
      x_axis_break_labels = NULL,
      y_axis_break_labels = NULL,
      y_axis_n_breaks = NULL,
      x_axis_reverse = FALSE,
      show_gridlines = TRUE,
      show_axislines = TRUE,
      legend_title = "",
      legend_pos = "right",
      legend_font_size = 8,
      legend_title_font_size = 8,
      hline = NULL,
      hline_colour = "black",
      hline_width = 0.5,
      hline_type = "dashed",
      hline_label = NULL,
      hline_label_colour = "black",
      hover_labels = NULL
    )
) {


  # Solve warnings regarding font family not found using utils/set_Arial() function
  set_Arial()


  # Where relevant, assign defaults to any parameters not specified by the user
  if(!exists('group_var_barmode',where=params)) params$group_var_barmode <- "stack"
  if(!exists('x_time_series',where=params)) params$x_time_series <- FALSE
  if(!exists('time_period',where=params)) params$time_period <- "day"
  if(!exists('fill_colours',where=params)) params$fill_colours <- "lightblue"
  if(!exists('bar_border_colour',where=params)) params$bar_border_colour <- "transparent"
  if(!exists('bar_labels_pos',where=params)) params$bar_labels_pos <- "bar_above"
  if(!exists('bar_labels_font_size',where=params)) params$bar_labels_font_size <- 8
  if(!exists('bar_labels_font_colour',where=params)) params$bar_labels_font_colour <- 'black'
  if(!exists('bar_labels_angle',where=params)) params$bar_labels_angle <- 0
  if(!exists('bar_labels_percent',where=params)) params$bar_labels_percent <- FALSE
  if(!exists('case_boxes',where=params)) params$case_boxes <- FALSE
  if(!exists('case_boxes_colour',where=params)) params$case_boxes_colour <- "white"
  if(!exists('axis_flip',where=params)) params$axis_flip <- FALSE
  if(!exists('ci_legend',where=params)) params$ci_legend <- TRUE
  if(!exists('ci_legend_title',where=params)) params$ci_legend_title <- "Confidence interval"
  if(!exists('ci_colours',where=params)) params$ci_colours <- "red"
  if(!exists('errorbar_width',where=params)) params$errorbar_width <- NULL
  if(!exists('chart_title_size',where=params)) params$chart_title_size <- 12
  if(!exists('chart_title_colour',where=params)) params$chart_title_colour <- "black"
  if(!exists('chart_footer_size',where=params)) params$chart_footer_size <- 10
  if(!exists('chart_footer_colour',where=params)) params$chart_footer_colour <- "black"
  if(!exists('x_axis_title_font_size',where=params)) params$x_axis_title_font_size <- 11
  if(!exists('y_axis_title_font_size',where=params)) params$y_axis_title_font_size <- 11
  if(!exists('x_axis_label_angle',where=params)) params$x_axis_label_angle <- 0
  if(!exists('y_axis_label_angle',where=params)) params$y_axis_label_angle <- 0
  if(!exists('x_axis_label_font_size',where=params)) params$x_axis_label_font_size <- 9
  if(!exists('y_axis_label_font_size',where=params)) params$y_axis_label_font_size <- 9
  if(!exists('x_axis_reverse',where=params)) params$x_axis_reverse <- FALSE
  if(!exists('show_gridlines',where=params)) params$show_gridlines <- TRUE
  if(!exists('show_axislines',where=params)) params$show_axislines <- TRUE
  if(!exists('legend_title',where=params)) params$legend_title <- ""
  if(!exists('legend_pos',where=params)) params$legend_pos <- "right"
  if(!exists('legend_font_size',where=params)) params$legend_font_size <- 8
  if(!exists('legend_title_font_size',where=params)) params$legend_title_font_size <- 8
  if(!exists('hline_colour',where=params)) params$hline_colour <- "black"
  if(!exists('hline_width',where=params)) params$hline_width <- 0.5
  if(!exists('hline_type',where=params)) params$hline_type <- "dashed"
  if(!exists('hline_label_colour',where=params)) params$hline_label_colour <- "black"
  # The following are not input parameters for epi_curve, but are needed
  #   for base_gg and base_plotly so set defaults
  params$y_percent <- FALSE
  params$y_sec_axis <- FALSE
  params$y_sec_axis_no_shift <- TRUE
  params$y_sec_axis_percent_full <- FALSE


  # Rename certain parameters so that they will be recognised
  #   by base_gg() and base_plotly()

  # params <- param_rename(params,"date_start","x_limit_min")
  # params <- param_rename(params,"date_end","x_limit_max")


  # 'base' not a user define arguement for col_chart, so set to NULL
  #   for base_gg() and base_plotly()
  base <- NULL
### DEV




  # ##### Checks and warnings
  #
  # # Check if df is is.null
  # if (!exists('df',where=params)) stop("A data frame argument is required")
  #
  # # Check df is a df class
  # if(!is.data.frame(params$df)) stop("df is not a data frame object")
  #
  # # Check df is empty
  # if(!not_empty(params$df)) stop("df is empty")
  #
  # # Check if date_var argument is null (date_var renamed to 'x' at this stage)
  # if ((is.null(params$x)) | !exists('x',where=params))
  #   stop("Please include a variable from df for date_var, i.e. date_var = \"variable_name\"")
  #
  # # # Check if date_var is in df (date_var renamed to 'x' at this stage)
  # # if (!params$x %in% colnames(params$df))
  # #   stop("date_var not found within df. Please include a variable from df for date_var, i.e. date_var = \"variable_name\"")
  #
  # # Check group_var_barmode valid
  # if (!is.null(params$group_var) & !(params$group_var_barmode %in% c('stack', 'group'))) {
  #   stop("group_var_barmode must equal 'stack' or 'group'")
  # }
  #
  # # Check time_period valid
  # if (!(params$time_period %in% c("day","year","month","quarter","year_month","year_quarter",
  #                                 "iso_year","iso_week","start_iso_year_week","iso_year_week"))) {
  #   stop("time_period must equal 'day', 'year', 'month', 'quarter', 'year_month', 'year_quarter',
  #             'iso_year', 'iso_week', 'start_iso_year_week', or 'iso_year_week'")
  # }
  #
  #
  # # If provided, check if y is in df
  # if (exists('y',where=params)) {
  #   if (!params$y %in% colnames(params$df))
  #     stop("y not found within df. Please include a variable from df for y, i.e. y = \"variable_name\"")
  # }
  #
  # # If provided, check if group_var is in df
  # if ("group_var" %in% names(params)) {
  #   if (!params$group_var %in% colnames(params$df))
  #     stop("group_var not found within df. Please include a variable from df for group_var, i.e. group_var = \"variable_name\"")
  # }
  #
  # # Check if number of groups and number of point colours are the same
  # if (exists('group_var', where=params) & length(params$fill_colours) > 1) {
  #   if (length(params$fill_colours) != length(unique(params$df[[params$group_var]])))
  #     stop("The number of fill_colours provided must equal the number of unique groups in group_var")
  # }
  #
  #
  # # Warn that multiple colours have been provided but group_var absent
  # if (length(params$fill_colours) >1 & !exists('group_var',where=params))
  #   warning("Multiple fill_colours have been provided but group_var is absent")
  #

  # CI = 'RIBBON' NOT ACCEPTED FOR COL_CHART()







  ##### Parameter assignment

  # Define parameters as variables using utils/param_assign() function
  #   -Takes input list, compares it ro a reference vector of expected
  #     list elements, assigns each element to a variable within the
  #     parent environment, and allocates a value of 'NULL' to anything
  #     it can't find within the reference list.
  param_assign(params,
               c("df",
                 "x",
                 "y",
                 "x_time_series",
                 "time_period",
                 "group_var",
                 "group_var_barmode",
                 "fill_colours",
                 "bar_border_colour",
                 "bar_labels",
                 "bar_labels_pos",
                 "bar_labels_font_size",
                 "bar_labels_font_colour",
                 "bar_labels_angle",
                 "bar_labels_percent",
                 "case_boxes",
                 "case_boxes_colour",
                 "axis_flip",
                 "ci",
                 "ci_legend",
                 "ci_legend_title",
                 "ci_lower",
                 "ci_upper",
                 "ci_colours",
                 "errorbar_width",
                 "chart_title",
                 "chart_footer",
                 "chart_title_size",
                 "chart_title_colour",
                 "chart_footer_size",
                 "chart_footer_colour",
                 "x_axis_title",
                 "y_axis_title",
                 "x_axis_title_font_size",
                 "y_axis_title_font_size",
                 "x_axis_label_angle",
                 "y_axis_label_angle",
                 "x_axis_label_font_size",
                 "y_axis_label_font_size",
                 "st_theme",
                 "x_axis_reverse",
                 "y_limit_min",
                 "y_limit_max",
                 "x_limit_min",
                 "x_limit_max",
                 "x_axis_break_labels",
                 "y_axis_break_labels",
                 "x_axis_n_breaks",
                 "y_axis_n_breaks",
                 "x_axis_date_breaks",
                 "show_gridlines",
                 "show_axislines",
                 "legend_title",
                 "legend_pos",
                 "legend_font_size",
                 "legend_title_font_size",
                 "hline",
                 "hline_colour",
                 "hline_width",
                 "hline_type",
                 "hline_label",
                 "hline_label_colour",
                 "hover_labels",
                 "ci_lower",   # required as NULL for base_gg() / base_plotly()
                 "ci_upper"    # required as NULL for base_gg() / base_plotly()
               )
  )


  # Set a default colour palette when group_var is set but fill_colour is not
  if(!is.null(group_var) & length(fill_colours) == 1) {
    fill_colours <- hue_pal()(length(unique(df[[group_var]])))
  }




  #################### DATA MANIPULATION #########################

  ### Define full date range for time series if x_time_series = TRUE

  if(x_time_series == TRUE) {


    # Define start and end dates if not provided
    if(is.null(x_limit_min)){
      date_start <- min(df[[x]]) - 5
    } else {
      date_start <- as.Date(x_limit_min)
    }

    if(is.null(x_limit_max)){
      date_end <- max(df[[x]]) + 5
    } else {
      date_end <- as.Date(x_limit_max)
    }

    # Filter out any rows outside date range so that they aren't included in aggregates
    df <- df |>
      filter(get(x) >= date_start & get(x) <= date_end) # & date_factor <= x_limit_max




    ### Add time periods to df

    # Use utils/adorn_dates() function to add additional date variables to df
    df <- adorn_dates(df, x)

    # Redefine x so that it points towards the relevant date column
    x <- time_period



    ### Create date factor for x-axis

    # Define sequence of all dates in full date range
    all_dates_seq <- data.frame(date_seq = seq(date_start, date_end, 1))

    # Expand with additional time periods using utils/adorn_dates()
    all_dates <- adorn_dates(all_dates_seq, "date_seq")

    # Pull only values corresponding to x (i.e. user specified time_period), turn
    #    into vector of unique values for use as factor levels
    unique_dates <- unique(all_dates[[x]])

    # Add date factor column to df for use in chart
    df <- df |>
      mutate(date_factor = factor(as.character(get(x)), levels = unique_dates)) |>
      filter(!is.na(date_factor)) # filter out NAs, i.e. dates that fall outside of range

    # Redefine x so that it points towards date_factor
    x <- "date_factor"



    ### Change x_limit_max and x_limit_min to match time_period choice

    # Create df of limits and apply utils/adorn_dates() to find time_period limits
    df_x_limits <- data.frame(x_limit = as.Date(c(x_limit_min,x_limit_max))) |>
      adorn_dates("x_limit") |>
      select(x_limit, any_of(time_period))

    # Define limits converted to time_period
    x_limit_min <- df_x_limits[[time_period]][1]
    x_limit_max <- df_x_limits[[time_period]][2]



    ### Add totals by date to df (i.e. defining y-axis)

    if (is.null(y)) {

      # Count rows by date if y is not specified (i.e. count rows by date if data is not pre-aggregated)

      if(is.null(group_var)) {

        # Un-grouped
        df <- df |>
          group_by(date_factor) |>
          summarise(n = n()) |>
          ungroup()

      } else {

        # Grouped
        df <- df |>
          group_by(date_factor, .data[[group_var]]) |>
          summarise(n = n()) |>
          ungroup()

      }

    } else {
      # Sum values by date if y is specified (i.e. sum values by date if data is pre-aggregated)

      if(is.null(group_var)) {

        # Un-grouped
        df <- df |>
          group_by(date_factor) |>
          summarise(n = sum(.data[[y]])) |>
          ungroup()

      } else {

        # Grouped
        df <- df |>
          group_by(date_factor, .data[[group_var]]) |>
          summarise(n = sum(.data[[y]])) |>
          ungroup()

      }

    }

    # Re-define y so that it points towards n
    y <- "n"


  }



  ##### Handle categorical x-axis not stored as factor
  if(is.character(df[[x]])) {
    df[[x]] <- as.factor(df[[x]])
  }
 ### DEV: Apply this to base_gg to generalise to all functions?




  #################### COL CHART #################################

  ##### CREATE STATIC CHART

  if (!dynamic) {
    # produce ggplot object if 'dynamic' is set to FALSE




    ##### Create base ggplot object

    # Define base ggplot object using R/base_gg() function
    #    -Force base_gg() to run in calling environment so it can find variables
    #     (lexical scoping will cause it to look for unfound variables in /R where
    #     it's stored rather than within the calling function's environment)
    environment(base_gg) <- environment()
    base_return <- base_gg()

    # base_gg() returns a list containing base and df; extract here
    base <- base_return$base
    df <- base_return$df
    xlim <- base_return$xlim
    ylim <- base_return$ylim



    # Supress spurious 'Scale for X is already present' messages
    #   -Will suppress other messages
    suppressMessages({



      ##### Build the column chart


      # Alter bar border colour to match case boxes border colour if case_boxes = TRUE
      if(case_boxes == TRUE) {bar_border_colour <- case_boxes_colour}


      # Build according to whether plotting variables are grouped or not
      if(is.null(group_var)) {

        # Create column chart without groups

        base <-
          base + geom_bar(
            data = df,
            mapping = aes(x = .data[[x]],
                          y = .data[[y]]
            ),
            fill = fill_colours[1],
            stat = 'identity',
            color = bar_border_colour,
            linewidth = 0.25
          )

      } else {

        # Create column chart with groups

        # Rename group_var_barmode for ggplot and generate group_var_barpos variable
        if(group_var_barmode == "group") {
          group_var_barmode <- "dodge"
          group_var_barpos <- position_dodge(preserve = 'single')  # ensures that bar widths are not stretched to fill when certain groups are empty
          df <- df |> complete(.data[[x]], .data[[group_var]])     # Fill in blank groups to make sure they're plotted as empty bars
        }

        base <-
          base + geom_bar(
            data = df,
            mapping = aes(x = .data[[x]],
                          y = .data[[y]],
                          group = .data[[group_var]],
                          fill = .data[[group_var]]
            ),
            stat = 'identity',
            color = bar_border_colour,
            linewidth = 0.25,
            position = if(group_var_barmode == 'dodge') {group_var_barpos} else {'stack'},
            na.rm = if(group_var_barmode == 'dodge') {TRUE} else {FALSE}
          ) +
          scale_fill_manual(values = fill_colours)

      }



      ##### Apply legend parameters

      # Legend title
      if (!is.null(legend_title)) {
        base <-  base + labs(name = legend_title,
                             fill = legend_title,
                             colour = legend_title)
      }

      # Legend position
      if (!is.null(legend_pos)) {
        base <-  base + theme(legend.position = legend_pos)
      }



      ##### Add boxes around each case if specified

      if (case_boxes == TRUE) {

        # Redefine df with one row per case to draw stacked bar chart where each row
        #    is one of the stacking elements 1-unit high.Uncount df to get 1 row per
        #    case, add box = 1 column for y-vals to create 1 box per case.
        df_box <- df |>
                 mutate(across(y, .fns = ~replace_na(.,0))) |>
                 uncount(get(y)) |>
                 mutate(box = 1)

        # When bars are dodged rather than stacked, slice by max value of y in
        #    each group else y-axis assumes the stacked value and over-scales accordingly
        if(group_var_barmode == 'dodge') {
          df_box <- df_box |>
                     slice_max(order_by = get(y), by = any_of(x)) |>
                     slice_max(order_by = get(group_var), by = any_of(x))   # second slice resolves draws if 2 groups have equal values
        }

        # Add transparent stacked bar plot with external borders to create
        #   boxes around each case.
        base <-
          base + geom_bar(
            data = df_box,
            mapping = aes(x = .data[[x]],
                          y = box
            ),
            fill = "transparent",
            stat = 'identity',
            #position = if(group_var_barmode == 'dodge') {group_var_barpos} else {'stack'},
            color = case_boxes_colour,
            linewidth = 0.5
          )

      }


      ##### Apply confidence intervals

      # Apply after col chart so that errorbars appear in front of columns.

      # Add conf intervals if arguments for ci and ci_upper+ci_lower bounds are provided.
      if(!is.null(ci)) {

        if(!is.null(ci_lower) && !is.null(ci_upper)) {

          # Add error bars

          # If errorbar_width not provided; define default based on x-axis limits
          if (is.null(errorbar_width)) {
            if (is.null(base$coordinates$limits$x)) {
              errorbar_width <- as.numeric((max(df[[x]]) - min(df[[x]])) / 100)
            } else {
              errorbar_width <- as.numeric((base$coordinates$limits$x[[2]] - base$coordinates$limits$x[[1]]) / 100)
            }
          }

          # Account for geom_ribbon show.legend parameter accepting values of 'NA' or 'FALSE'
          show_ci_leg <- ifelse(ci_legend == TRUE, NA, FALSE)

          # Plot for no group_var
          if (is.null(group_var)) {

            # Add error bars without grouping variable
            if(ci == 'errorbar') {

              base <-
                base + ggplot2::geom_errorbar(
                  data = df,
                  mapping = aes(
                    x = .data[[x]],
                    ymin = .data[[ci_lower]],
                    ymax = .data[[ci_upper]],
                    colour = ci_legend_title
                  ),
                  width = errorbar_width,
                  linewidth = 0.5,
                  show.legend = show_ci_leg
                ) +
                scale_color_manual("",values=ci_colours[[1]])

            }

          # Plot for group_var provided
          } else if (!is.null(group_var)) {

            # Add error bars with grouping variable
            if(ci == 'errorbar') {

              # Generate offset position for errorbars when group_var_barmode = "dodge"
              errorbar_offset <- position_dodge(resolution(as.numeric(df[[x]]))*0.9)

              # geom_errorbar does not support stacked bar charts (https://github.com/tidyverse/ggplot2/issues/1079)
              #   Positions of errorbars for stacked plots must be calculated manually, create new
              #   dataframe to manage this.
              df_errbar <- df |>
                group_by(.data[[x]]) |>
                arrange(.data[[x]], desc(.data[[group_var]])) |>
                mutate(cumul = cumsum(.data[[y]]),
                       lower_lim_stacked = cumul - (.data[[y]] - .data[[ci_lower]]),
                       upper_lim_stacked = cumul + (.data[[ci_upper]] - .data[[y]]))


              base <-
                base + ggplot2::geom_errorbar(
                  data = df_errbar,
                  mapping = aes(
                    x = .data[[x]],
                    ymin = if(group_var_barmode != "stack") {.data[[ci_lower]]} else {lower_lim_stacked},
                    ymax = if(group_var_barmode != "stack") {.data[[ci_upper]]} else {upper_lim_stacked},
                    group = .data[[group_var]],
                    colour =  .data[[group_var]]
                  ),
                  width = errorbar_width,
                  linewidth = .5,
                  position = if(group_var_barmode == "dodge") {errorbar_offset} else {"identity"},
                  na.rm = if(group_var_barmode == "dodge") {TRUE} else {FALSE}
                  #position = position_dodge(resolution(as.numeric(df[[x]]))*0.9)
                )

              # Add ci_colours if provided
              if (length(ci_colours) > 1) {
                base <- base +
                  scale_colour_manual(values = ci_colours)
              }

            }

          # Stop if ci_upper and/or ci_lower limit isn't provided
        } else {
          stop("Please provide arguements for 'ci_upper' and 'ci_lower' when ci is specified.")
        }

      }
    }



      ##### Add bar labels
      if(!is.null(bar_labels)) {

        # # Handle the various permutations of label rotation to match plotly
        # if(bar_labels_angle %in% c(0,360)) {
        #   v1 <- -0.5
        #   h1 <- 0.5
        # } else if (bar_labels_angle > 0 & bar_labels_angle < 90) {
        #   v1 <- 0
        #   h1 <- 0
        # } else if (bar_labels_angle == 90) {
        #   v1 <- 0.5
        #   h1 <- -0.3
        # } else if (bar_labels_angle > 90 & bar_labels_angle < 180) {
        #   v1 <- 0
        #   h1 <- -0.3
        # } else if (bar_labels_angle == 180) {
        #   v1 <- 1.5
        #   h1 <- 0.5
        # } else if (bar_labels_angle > 180 & bar_labels_angle < 270) {
        #   v1 <- 1.5
        #   h1 <- 1
        # } else if (bar_labels_angle == 270) {
        #   v1 <- 0.5
        #   h1 <- 1.3
        # } else if (bar_labels_angle > 270 & bar_labels_angle < 360) {
        #   v1 <- 0
        #   h1 <- 1.3
        # }

        # Reverse label angle direction to match plotly (i.e. make parameter tilt labels clockwise)
        bar_labels_angle <- -bar_labels_angle

        # Define ynudge for non-right-angles (same value can be used in plotly)
        ylength <- if (!is.na(ylim[2])) {ylim[2]} else {ggplot_build(base)$layout$panel_params[[1]]$y.range[2]}
        ynudge <- if(-bar_labels_angle %in% c(0,90,180,270,360)) {0} else {ylength * 0.02} # 2% of y-axis length, matches vjust distance

        # Define vjust and hjust
        #   - Keep as 0.5/0.5 (i.e. centre/centre) except when using a right angle, then other
        #     alignments are optimal.
        if(-bar_labels_angle %in% c(0,360)) {
          v <- -0.5
          h <- 0.5
        } else if (-bar_labels_angle == 90) {
          v <- 0.5
          h <- 1.3
        } else if (-bar_labels_angle == 180) {
          v <- 1.5
          h <- 0.5
        } else if (-bar_labels_angle == 270) {
          v <- 0.5
          h <- -0.3
        } else {
          v <- 0.5
          h <- 0.5
        }

        # Define plot label positions depending on bar_labels_pos choice
        if(bar_labels_pos == 'bar_above') {
          x_labpos <- df[[x]]
          y_labpos <- df[[y]]
        } else if (bar_labels_pos == 'bar_base') {
          x_labpos <- df[[x]]
          y_labpos <- if (is.na(ylim[1])) {0} else {ylim[1]}  # position at bottom of y-range
          ynudge <- if(-bar_labels_angle %in% c(90,270)) {ynudge} else {ynudge + (0.02 * ylength)} # adjust ynudge for 90/270 rotations to keep congruent with plotly output
        } else if (bar_labels_pos == 'bar_centre') {
          x_labpos <- df[[x]]
          y_labpos <- df[[y]] / 2
        } else if (bar_labels_pos == 'above_errorbar') {
          x_labpos <- df[[x]]
          y_labpos <- df[[ci_upper]]
        }


        base <- base + geom_text(
                        data = df,
                        aes(x = x_labpos,
                            y = y_labpos,
                            vjust = v,
                            hjust = h,
                            label = .data[[bar_labels]]),
                        colour = bar_labels_font_colour,
                        size = bar_labels_font_size * (5/14), # apply scaling ratio to font size
                        angle = bar_labels_angle,
                        nudge_y = ynudge
                        )

      }


      ##### Flip axes if axis_flip = true
      if(axis_flip == TRUE) {
        base <- base + coord_flip(xlim = xlim, ylim = ylim, expand = FALSE)
      }


      ##### Redefine elements of base_gg specific to col_chart()

      # Redefine x-axis label so that default = x
      if (!is.null(x_axis_title)) {
        base <- base + labs(x = x_axis_title)
      } else {
        base <- base + labs(x = x) # default to x if label not provided
      }


      # Redefine y-axis scale to remove gap between bars and x-axis

      if (!is.null(y_axis_break_labels)) {
        base <- base + scale_y_continuous(breaks = y_axis_break_labels,
                                          expand = c(0,0))
      } else {
        base <- base + scale_y_continuous(expand = c(0,0))
      }



      # Re-apply hline so that it doesn't appear behind the bars

      # Adds hline
      if (!is.null(hline)) {

        base <-
          base + geom_hline(yintercept = hline,
                            colour = hline_colour,
                            linewidth = hline_width,
                            linetype = hline_type)

      }

      # Apply hline label to plot
      if (!is.null(hline) && !is.null(hline_label)) {

        # Get hline_xpos from base_return list from base_gg
        hline_xpos <- base_return$hline_xpos

        # Plot hline label(s)
        base <- base +
          geom_text(
            aes(
              x = hline_xpos,
              y = hline,
              label = hline_label,
              vjust = -0.5,
              hjust = 0,
              angle = if (axis_flip == FALSE) {0} else {270}
            ),
            #position = position_nudge(y = if (axis_flip == FALSE) {0} else {0.005 * ggplot_build(base)$layout$panel_params[[1]]$x.range[2]}), # nudge label by 0.5% of axis so that it's not sitting in contact with hline when aex are flipped
            colour = hline_label_colour)

      }


      ##### Return final output
      return(base)

    }) # suppressMessage() end

    ### STATIC CHART END



  } else {

    ##### CREATE DYNAMIC CHART

    # Produce plotly object if 'dynamic' is set to TRUE


    ##### Define base min/max x & y values for axis ranges

    #   -It is not currently possible to access range/autorange values from
    #    a plotly object, so define a ggplot object showing the same information
    #    and use it's autoranges as a basis. This also keeps the formatting the
    #    same as the static chart.
    #   -Must be done outside of base_plotly(), as the ggplot geoms will be
    #      different depending upon the nature of the chart

    # Define ggplot object to harvest axis ranges from
    # if(is.null(group_var)) {
    #   ggobj <- ggplot() +
    #     geom_bar(data = df, aes(x = .data[[x]],y = .data[[y]]), stat = 'identity') +
    #     geom_hline(yintercept = hline)
    # } else {
    #   if(group_var_barmode == "group") {group_var_barmode <- "dodge"}
    #   ggobj <- ggplot() +
    #     geom_bar(data = df, aes(x=.data[[x]],y=.data[[y]],group=.data[[group_var]]),
    #              stat = 'identity',position = group_var_barmode) +
    #     geom_hline(yintercept = hline)
    # }
    # if(axis_flip == TRUE) {ggobj <- ggobj + coord_flip()} else {ggobj}

    # Harvest ggplot object from static version of chart using col_chart() itself
    ggobj <- col_chart(params = params, dynamic = FALSE)
    # Harvest axis limits from ggobj so that plotly output matches ggplot output
    x_min <- ggplot_build(ggobj)$layout$panel_params[[1]]$x.range[1]
    x_max <- ggplot_build(ggobj)$layout$panel_params[[1]]$x.range[2]
    if(axis_flip == FALSE) {y_min <- 0} else {y_min <- ggplot_build(ggobj)$layout$panel_params[[1]]$y.range[1]}
    y_max <- ggplot_build(ggobj)$layout$panel_params[[1]]$y.range[2]
    # Harvest ggplot x-axis resolution to use as offset for dodged errorbars
    errorbar_offset <- resolution(as.numeric(df[[x]]))*0.9


    # # Handle dates converting to numeric when extracted from ggplot axis range
    # x_min <- if(lubridate::is.Date(df[[x]])) {as.Date.numeric(x_min)} else {x_min}
    # x_max <- if(lubridate::is.Date(df[[x]])) {as.Date.numeric(x_max)} else {x_max}
    # y_min <- if(lubridate::is.Date(df[[y]])) {as.Date.numeric(y_min)} else {y_min}
    # y_max <- if(lubridate::is.Date(df[[y]])) {as.Date.numeric(y_max)} else {y_max}


    ##### To flip axis in plotly, all x and y variables must be exchanged
    if(axis_flip == TRUE) {
      # Use utils/swap_object_names() function to swap all the x/y variable names
      swap_object_names('x', 'y')
      swap_object_names('x_axis_title', 'y_axis_title')
      swap_object_names('x_axis_title_font_size', 'y_axis_title_font_size')
      #swap_object_names('x_axis_label_angle', 'y_axis_label_angle')
      swap_object_names('x_axis_label_font_size', 'y_axis_label_font_size')
      swap_object_names('x_limit_min', 'y_limit_min')
      swap_object_names('x_limit_max', 'y_limit_max')
      swap_object_names('x_axis_break_labels', 'y_axis_break_labels')
    }



    ##### Create base plotly object

    # Define base plotly object using R/base_plotly() function
    #    -Force base_plotly() to run in calling environment so it can find variables
    #     (lexical scoping will cause it to look for unfound variables in /R where
    #     it's stored rather than within the calling function's environment)
    environment(base_plotly) <- environment()
    base_return <- base_plotly()

    # base_plotly() returns a list containing base, df, y_axis_choice, and axis_label_font; extract here
    base <- base_return$base
    df <- base_return$df
    y_axis_choice <- base_return$y_axis_choice
    x_axis_title_font <- base_return$x_axis_title_font
    y_axis_title_font <- base_return$y_axis_title_font


    ##### Define colour parameters for bar plot

    # Ungrouped data
    if (is.null(group_var)) {

      # Define colour map for single colour
      colormap <- setNames(fill_colours[1], fill_colours[1])

      # Add colour field to df
      df <- df |>
        mutate(fill_colour = fill_colours[1])

      # Grouped data
    } else {

      # Define colour map for multiple colours if not already defined by user
      if (is.null(names(fill_colours))) {
        colormap <- setNames(fill_colours, unique(df[[group_var]]))
      } else {
        colormap <- fill_colours
      }

      # Add colour field to df
      df <- df |>
        mutate(fill_colour = if (is.factor(get(group_var))) {get(group_var)} else {as.factor(get(group_var))} )

    }

    # Define colour_field parameter for bar plot
    #    Note:- Stacked bar colours need reversing to match with ggplot output
    if (is.factor(df$fill_colour) & group_var_barmode == "stack") {
      colour_field <- forcats::fct_rev(df$fill_colour)
    } else {
      colour_field <- df$fill_colour
    }




    ##### Define hover labels

    # Define defaults if user does not define hover_labels
    if (is.null(hover_labels)) {

      # Define total to display depending upon whether case boxes are enabled
      hover_n <- if (case_boxes == FALSE) {'{y}'} else {'{customdata}'}

      # Define hover label text
      hoverlabels <- paste0('<b>%{x}</b>',
                            '<br>%',hover_n)

      # Add upper and lower limits to label if ci is defined
      if (!is.null(ci)) {
        hoverlabels <- paste0(hoverlabels,'%{text}')  # leverage 'text' parameter in add_trace to pass additional info to hoverlabels
      }

      # Remove tooltip for ungrouped data
      if (is.null(group_var)) {
        hoverlabels <- paste0(hoverlabels, '<extra></extra>')
      }

    } else {

      # Hover labels specified by user input parameter hover_labels
      if (case_boxes == FALSE) {
        hoverlabels <- hover_labels
      } else {
        hoverlabels <- gsub('\\{y\\}','{customdata}',hover_labels)
      }

    }



    ##### Build col_chart

    if (case_boxes == FALSE) {

      # Add bar plot without boxes around each case
      base <- base |>
        add_trace(
          df,
          x = ~ df[[x]],
          y = ~ df[[y]],
          # x = ~ if(axis_flip == FALSE) {df[[x]]} else {df[[y]]},
          # y = ~ if(axis_flip == FALSE) {df[[y]]} else {df[[x]]},
          type = 'bar',
          color = ~ colour_field, #df$fill_colour,
          colors = colormap,
          marker = list(
            #color = fill_colours[1]
            line = list(color = bar_border_colour,
                        width = 0.5)
          ),
          text = if(is.null(ci)) {''} else {paste0('<br><i>Upper: ',df[[ci_upper]],'</i>',   # leverage 'text' parameter in add_trace to pass additional info to hoverlabels
                                                   '<br><i>Lower: ',df[[ci_lower]],'</i>')},
          hovertemplate = hoverlabels,
          #legendgroup = 'bars',
          orientation = if(axis_flip == TRUE) {'h'} else {'v'}, #set orientation to horizontal if axis_flip = TRUE
          showlegend = if (is.null(group_var)) {F} else {T}
        ) |>
        layout(barmode = group_var_barmode)


      # Add bar plot with boxes around each case
    } else if (case_boxes == TRUE) {

      # Uncount data to get one row per case for one box per case.
      df_case_boxes <- df |> uncount(get(y)) |> mutate(box = 1)

      # Re-define colour_field parameter for bar plot
      #    Note:- Stacked bar colours need reversing to match with ggplot output
      if (is.factor(df_case_boxes$fill_colour) & group_var_barmode == "stack") {
        colour_field <- forcats::fct_rev(df_case_boxes$fill_colour)
      } else {
        colour_field <- df_case_boxes$fill_colour
      }

      # Bar plot with boxes
      base <- base |>
        add_trace(
          df_case_boxes,
          x = ~ df_case_boxes[[x]],
          y = ~ df_case_boxes$box,
          type = 'bar',
          color = ~ colour_field, #df$fill_colour,
          colors = colormap,
          marker = list(
            line = list(color = case_boxes_colour,
                        width = 0.5)
          ),
          text = if(is.null(ci)) {''} else {paste0('<br><i>Upper: ',df_case_boxes[[ci_upper]],'</i>',   # leverage 'text' parameter in add_trace to pass additional info to hoverlabels
                                                   '<br><i>Lower: ',df_case_boxes[[ci_lower]],'</i>')},
          customdata = df_case_boxes[[y]], # for hoverlabels
          hovertemplate = hoverlabels,
          #legendgroup = 'bars',
          orientation = if(axis_flip == TRUE) {'h'} else {'v'}, #set orientation to horizontal if axis_flip = TRUE
          showlegend = if (is.null(group_var)) {F} else {T}
        ) |>
        layout(barmode = group_var_barmode)

    }





    ##### Apply confidence intervals

    # Apply after main bar-plot so that errorbars appear in front of columns.

    # Add conf intervals if arguments for ci and ci_upper+ci_lower bounds are provided.
    if(!is.null(ci)) {

      # Stop if ci_upper and/or ci_lower limit isn't provided
      if(is.null(ci_lower) | is.null(ci_upper)) {
        stop("Please provide arguements for 'ci_upper' and 'ci_lower' when ci is specified.")
      }


      # Plot for no group_var
      if (is.null(group_var)) {

        # Add error bars without grouping variable
        if(ci == 'errorbar') {

          # Ensure that there's only a single colour in the input vector
          ci_single_colour <- ci_colours[1]

          # Plotly error bars require upper and lower error divergence rather
          #   than values, so calculate
          if(axis_flip == TRUE) {swap_object_names('x', 'y')} # temporarily swap back axis names for calculation if axes are flipped

          df <- df |>
            mutate(diff_ci_lower = get(y) - get(ci_lower),
                   diff_ci_upper = get(ci_upper) - get(y))

          if(axis_flip == TRUE) {swap_object_names('x', 'y')}


          # Add error bars as trace
          base <- base |>
            add_trace(
              data = df,
              x = ~ df[[x]],
              y = ~ df[[y]],
              type = 'scatter',
              mode = 'markers',
              yaxis = y_axis_choice,
              name = ci_legend_title,
              showlegend = ci_legend,
              legendgroup = 'ci',
              marker = list(
                color = ci_single_colour,
                line = list(colour = '#ffffff00', width = 0),
                opacity = 0,
                symbol = 'line-ew-open' # use hozizontal line markers so that horizontal line symbol will be used in legend to match ggplot legend formatting
              ),
              hoverinfo='none',
              error_y = list(   # normal vertical error bars, not visible if axes are flipped
                type = "data",
                symmetric = FALSE,
                color = ci_single_colour,
                thickness = 1,
                arrayminus = ~ diff_ci_lower,
                array = ~ diff_ci_upper,
                visible = if (axis_flip == TRUE) {F} else {T}
              ),
              error_x = list(   # horizontal error bars, only visible if axes are flipped
                type = "data",
                symmetric = FALSE,
                color = ci_single_colour,
                thickness = 1,
                arrayminus = ~ diff_ci_lower,
                array = ~ diff_ci_upper,
                visible = if (axis_flip == TRUE) {T} else {F}
              )
            )
        }



        # Plot for group_var provided
      } else if (!is.null(group_var)) {

        # Add error bars with grouping variable
        if(ci == 'errorbar') {

          # Errorbar traces must be defined individually for each group

          # Define unique groups
          unique_groups <- unique(df[[group_var]])

          if(axis_flip == TRUE) {swap_object_names('x', 'y')} # temporarily swap back axis names for calculation if aex are flipped

          # y-axis positions of errorbars for stacked plots must be calculated
          #   manually, create new dataframe to manage this.
          df_errbar <- df |>
            group_by(.data[[x]]) |>
            arrange(.data[[x]], desc(.data[[group_var]])) |>
            mutate(cumul = cumsum(.data[[y]]),
                   lower_lim_stacked = cumul - (.data[[y]] - .data[[ci_lower]]),
                   upper_lim_stacked = cumul + (.data[[ci_upper]] - .data[[y]]))


          # x-axis position of errorbars for grouped/dodged plots must be manually offset;
          #    offset is dependent on number of unique groups and x-axis resolution (calculated
          #    from ggobj above = errorbar_offset)
          offset_multiplier <- seq(length(unique_groups)) - ceiling(length(unique_groups)/2) # generate vector of integars to multiply the base offset by
          if(length(unique_groups) %%2 != 0) {offset_multiplier - 0.5}                       # if the number of groups is odd, offset the multiplier by 0.5 as one of the bars will be in the middle (for even numbers, the middle will be between the 2 central bars)
          offset_multiplier <- offset_multiplier * 0.9                                       # multiply this offset by 0.9 to match the ggplot defaults
          x_offset <- offset_multiplier * (errorbar_offset/length(unique_groups))            # divide the x-axis resolution by the number of groups and multiply this by the multiplier to get the actual x-values of each bar
          if(is.Date(df[[x]])) {                                                             # if x is a date then plotly will shunt the errorbars to the beginning of the nearest whole day (i.e. not in the middle of the barchart bars) so convert axis to time axis if this is the case
            df[[x]] <- as.POSIXct(df[[x]])
            x_offset <- x_offset * 24*60*60   # unit value for time is seconds rather than days as for date, so convert
          }

          if(axis_flip == TRUE) {swap_object_names('x', 'y')} # flip axis variables back


          # Iterate over each group
          for (i in 1:length(unique_groups)) {

            if(axis_flip == TRUE) {swap_object_names('x', 'y')} # flip access variables within each iteration for calculation if needed

            # Plotly error bars require upper and lower error divergence rather
            #   than values, so create df for each group and calculate
            if(group_var_barmode == "group") {
              df_group <- df |>
                filter(get(group_var) == unique_groups[i]) |>
                mutate(diff_ci_lower = get(y) - get(ci_lower),
                       diff_ci_upper = get(ci_upper) - get(y)) |>
                mutate(x_grouped = get(x) + x_offset[i]) # offset x values for grouped bars
                # mutate(x_grouped = get(x) + ((-errorbar_offset/2) + (i+0.5)*(errorbar_offset/length(unique_groups)) )) |>
                # mutate(x_grouped = x_grouped - errorbar_offset/length(unique_groups))

            } else if(group_var_barmode == "stack") {
              # Use stack values when group_var_barmode == "stack"
              df_group <- df_errbar |>
                filter(get(group_var) == unique_groups[i]) |>
                mutate(diff_ci_lower = cumul - lower_lim_stacked,
                       diff_ci_upper = upper_lim_stacked - cumul)
            }

            if(axis_flip == TRUE) {swap_object_names('x', 'y')}  # flip access variables back after calculation

            # Add error bars as trace with invisible markers
            base <- base |>
              add_trace(
                data = df_group,
                # Select x and y based on grouped barmode and whether axes are flipped
                #    Note:- There is probably a neater way of coding this.
                x = if(group_var_barmode == "group" & axis_flip == FALSE) {df_group$x_grouped} else if(group_var_barmode == "stack" & axis_flip == TRUE) {df_group$cumul} else {df_group[[x]]},
                y = if(group_var_barmode == "stack" & axis_flip == FALSE) {df_group$cumul} else if(group_var_barmode == "group" & axis_flip == TRUE) {df_group$x_grouped} else {df_group[[y]]},
                type = 'scatter',
                mode = 'markers',
                yaxis = y_axis_choice,
                name = unique_groups[[i]],
                showlegend = F,
                marker = list(
                  color = '#ffffff00',
                  opacity = 0,
                  line = list(colour = '#ffffff00', width = 0)
                ),
                hoverinfo='none',
                error_y = list(   # normal vertical error bars, not visible if axes are flipped
                  type = "data",
                  symmetric = FALSE,
                  color = ci_colours[[i]],
                  thickness = 1,
                  arrayminus = ~ diff_ci_lower,
                  array = ~ diff_ci_upper,
                  visible = if (axis_flip == TRUE) {F} else {T}
                ),
                error_x = list(   # horizontal error bars, only visible if axes are flipped
                  type = "data",
                  symmetric = FALSE,
                  color = ci_colours[[i]],
                  thickness = 1,
                  arrayminus = ~ diff_ci_lower,
                  array = ~ diff_ci_upper,
                  visible = if (axis_flip == TRUE) {T} else {F}
                )
              )


          }

        }

      }

    }



    ##### Apply bar labels

    # Apply after main bar-plot and errorbars so that labels appear in front.

    if(!is.null(bar_labels)) {

      # Define ynudge value to make labels appear in same position as in ggplot output
      ynudge <- if (bar_labels_angle %in% c(90,270)) {0} else {0.02 * y_max}


      # Label according to whether or not bars are grouped

      # Plot for no group_var
      if (is.null(group_var)) {

        # Define plot label parameters for bar_labels_pos choice
        if(bar_labels_pos == 'bar_above') {
          x_labpos <- df[[x]]
          y_labpos <- df[[y]]
        } else if (bar_labels_pos == 'bar_base') {
          x_labpos <- df[[x]]
          y_labpos <- if(bar_labels_angle %in% c(90,270)) {y_min + (0.015*y_max)} else {y_min + (1.5*ynudge)}
        } else if (bar_labels_pos == 'bar_centre') {
          x_labpos <- df[[x]]
          y_labpos <- df[[y]] / 2
        } else if (bar_labels_pos == 'above_errorbar') {
          x_labpos <- df[[x]]
          y_labpos <- if(bar_labels_angle %in% c(90,270)) {df[[ci_upper]] + (0.01*y_max)} else {df[[ci_upper]]}
        }

        base <- base |>
          add_annotations(text = ~ df[[bar_labels]],
                          x = ~ x_labpos,
                          y = ~ y_labpos + ynudge,
                          textangle = bar_labels_angle,
                          yanchor = if (bar_labels_angle %in% c(90,270)) {"bottom"} else {"centre"},
                          showarrow = FALSE)


      }

    }


    ##### Apply legend parameters

    # Legend position
    if (!is.null(legend_pos)) {

      # Let utils/plotly_legend_pos() function run in calling environment so that
      #    it can access point_chart arguements
      environment(plotly_legend_pos) <- environment()

      if (legend_pos != "none") {
        base <- base |> layout(legend = plotly_legend_pos(legend_pos))  # use utils/plotly_legend_pos() function to switch between ggplot and plotly legend params
      } else {
        base <- base |> layout(showlegend = F)
      }

    }

    # Legend title + font
    base <- base |>
      layout(
        legend = list(
          #traceorder = "grouped+reversed",
          title=list(text = legend_title, font = list(size = legend_title_font_size)),
          font=list(size = legend_font_size)
        )
      )




    # return base plot
    return(base)

  } ### DYNAMIC CHART END


}






