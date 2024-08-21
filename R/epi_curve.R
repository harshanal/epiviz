#' epi_curse
#'
#' @description A function for producing either a static (ggplot) or dynamic (plotly)
#' epidemic curve.
#'
#' @param dynamic Logical indicating whether to produce a dynamic (plotly) output.
#' Default is \code{FALSE}, which will return a static ggplot output.
#' @param params A named list containing arguements used to create the plot.
#' ' \describe{
#'    \item{df}{A data frame containing values used to create the point chart.}
#'    \item{y}{Name of the variable in \code{df} used to populate the y-axis.}
#'    \item{date_var}{Name of the variable in \code{df} containing the dates used
#'    to populate the x-axis.}
#'    \item{time_period}{The time period to be used along the x-axis. Options include
#'    \code{c("day","year","month","quarter","year_month","year_quarter",
#'      "iso_year","iso_week","start_iso_year_week","iso_year_week",
#'      "use_date_var")}. Default = \code{"use_date_var"}, which indicates that the dates
#'      as they appear in the date_var column will be used to populate the x-axis.}
#'    \item{group_var}{Name of the variable in df used to define separate groups of points
#'    in the chart.}
#'    \item{ci} {Confidence interval. If \code{ci = "errorbar"} then confidence intervals be
#'    be plotted with each point as errorbars, and if \code{ci = "ribbon"} then confidence
#'    intervals will be added to the chart as a ribbon plot for each group. If \code{ci} is
#'    provided, then \code{ci_upper} and \code{ci_lower} must also be provided.}
#'    \item{ci_upper} {Name of the variable in df used as the upper confidence limit for
#'    each point. Mandatory when \code{ci} is provided.}
#'    \item{ci_lower} {Name of the variable in df used as the lower confidence limit for
#'    each point. Mandatory when \code{ci} is provided.}
#'    \item{ci_legend} {Logical indicating whether a separate legend should be included
#'    in the chart for confidence interval parameters. Only applies when \code{group_var}
#'    is provided. Defaults to \code{FALSE}.}
#'    \item{ci_legend_title} {Text to use as title for separate legend when \code{ci_legend = TRUE}.}
#'    \item{ci_colours} {Colour(s) used for plotting confidence intervals. When \code{ci =
#'    "errorbar"} this will determine the colour of the plotted errorbars, when \code{ci =
#'    "ribbon"} this will determine the colour of the plotted ribbons.}
#'    \item{errorbar_width} {Horizontal width of the plotted error bars when \code{ci =
#'    "errorbar"}.}
#'    \item{y_sec_axis} {Logical to indicate whether data should be plotted on the
#'    secondary (right) y-axis. Default = \code{FALSE}.}
#'    \item{y_sec_axis_no_shift} {Forces the secondary y-axis scale to begin at 0. Default = \code{TRUE}.}
#'    \item{y_sec_axis_percent_full} {Forces the secondary y-axis scale to range from 0-100%
#'    when \code{y_percent = TRUE}}
#'    \item{chart_title} {Text to use as chart title.}
#'    \item{chart_title_size} {Font size of chart title.}
#'    \item{chart_title_colour} {Font colour of chart title.}
#'    \item{chart_footer} {Text to use as chart footer.}
#'    \item{chart_footer_size} {Font size of chart footer.}
#'    \item{chart_footer_colour} {Font colour of chart footer.}
#'    \item{x_axis_title} {Text used for x-axis title. Defaults to name of x-variable if
#'    not stated.}
#'    \item{y_axis_title} {Text used for y-axis title. Defaults to name of y-variable if
#'    not stated.}
#'    \item{x_axis_label_angle} {Angle for x-axis label text.}
#'    \item{y_axis_label_angle} {Angle for y-axis label text.}
#'    \item{x_axis_reverse} {Reverses x-axis scale if \code{x_axis_reverse = TRUE}.}
#'    \item{y_percent} {Converts y-axis to percentage scale if \code{y_percent = TRUE}.}
#'    \item{x_limit_min} {Lower limit for the x-axis. Default used if not provided.}
#'    \item{x_limit_max} {Upper limit for the x-axis. Default used if not provided.}
#'    \item{y_limit_min} {Lower limit for the y-axis. Default used if not provided.}
#'    \item{y_limit_max} {Upper limit for the y-axis. Default used if not provided.}
#'    \item{x_axis_break_labels} {Vector of values to use for x-axis breaks. Defaults used if not provided.}
#'    \item{y_axis_break_labels} {Vector of values to use for y-axis breaks. Defaults used if not provided.}
#'    \item{x_axis_n_breaks} {Scales x-axis with approximately n breaks. Cannot be provided
#'    if \code{x_axis_break_labels} is provided.}
#'    \item{y_axis_n_breaks} {Scales y-axis with approximately n breaks. Cannot be used
#'    if \code{y_axis_break_labels} is also provided.}
#'    \item{x_axis_date_breaks} {A string giving the distance between breaks like "2 weeks", or "10 years".
#'    Valid specifications are 'sec', 'min', 'hour', 'day', 'week', 'month' or 'year', optionally followed
#'    by 's'. Matches ggplot scale_date() conventions (see https://ggplot2.tidyverse.org/reference/scale_date.html).
#'    Cannot be used if \code{y_axis_break_labels} is also provided.}
#'    \item{st_theme} {Name of a ggplot theme to be applied to a static plot. Can only be provided
#'    when \code{dynamic = FALSE}}
#'    \item{show_gridlines} {Logical to show chart gridlines. Default = \code{TRUE}.}
#'    \item{show_axislines} {Logical to show chart axis lines. Default = \code{TRUE}.}
#'    \item{legend_title} {Text used for legend title.}
#'    \item{legend_pos} {Position of the legend. Permitted values = c("top","bootom","right","left")}
#'    \item{hline} {Adds horizontal line across the chart at the corresponding y-value. Multiple
#'    values may be provided as a vector to add multiple horizontal lines.}
#'    \item{hline_colour} {Colour of the horizontal lines if \code{hline} is provided. A vector of colours
#'    can be provided to colour individual hlines if multiple hlines have been provided.}
#'    \item{hline_width} {Numerical width of the horizontal lines if \code{hline} is provided. A vector of numerical widths
#'    can be provided for individual hlines if multiple hlines have been provided.}
#'    \item{hline_type} {Line style of the horizontal lines if \code{hline} is provided. A vector of line styles
#'    can be provided to style hlines if multiple hlines have been provided. Permitted values = c("solid", "dotted",
#'    "dashed", "longdash", "dotdash").}
#'    \item{hline_label} {Text to label the horizontal lines if \code{hline} is provided. A vector of text strings
#'    can be provided to label individual hlines if multiple hlines have been provided.}
#'    \item{hline_label_colour} {Colour of the horizontal line labels if \code{hline_labels} is provided.
#'    A vector of colours can be provided to colour individual hline_labels if multiple hline_labels have been provided.}
#'
#'
#'  }
#'
#'
#'
#' @import dplyr
#' @import magrittr
#' @import grDevices
#' @import scales
#' @import tidyr
#' @import yarrr
#' @import lubridate
#' @import ISOweek
#' @import zoo
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @return A ggplot or plotly object.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'
#'
#' # Example 5: Point chart with additional overlayed chart on secondary y-axis
#' library(epiviz)
#'
#' # Use static chart from Example 1 as a base chart
#' base_chart <- chart_detections_per_month
#'
#' # Define data for overlaying chart
#' # Percentage of overall detections in people over 65 years of age.
#' library(lubridate)
#' detections_over65 <- lab_data |>
#'   mutate(age = year(as.period(lubridate::interval(date_of_birth,Sys.Date()))),
#'          over65 = ifelse(age > 65, 1, 0)) |>
#'   group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
#'   summarise(detections = n(),
#'             detections_over65 = sum(over65)) |>
#'   ungroup() |>
#'   mutate(percent_over65 = detections_over65/detections)
#'
#' # Define parameters list
#' over65_params <- list(
#'   df = detections_over65,
#'   x = "specimen_month",
#'   y = "percent_over65",
#'   y_percent = TRUE,
#'   y_sec_axis = TRUE,
#'   y_sec_axis_percent_full = TRUE,
#'   point_colours = "purple",
#'   point_size = 3,
#'   point_shape = "asterisk",
#'   x_limit_min = "2022-01-01",
#'   x_limit_max = "2023-12-31",
#'   y_limit_max = 1000,
#'   chart_title = "Detections per Month 2022-2023",
#'   x_axis_title = "Month of detection",
#'   y_axis_title = "Percentage of detections in over 65s",
#'   x_axis_date_breaks = "2 months"
#' )
#'
#' # Create point chart
#' over65_chart <- point_chart(base = base_chart,
#'                             params = over65_params,
#'                             dynamic = FALSE)
#'
#'
#' # Legends are not currently implemented for static charts with a supplied
#' #  base chart, so add legend manually using dummy data and an invisible geom_point()
#' over65_chart <- over65_chart +
#'   geom_point(data = data.frame(x=as.Date(c("2020-01-01","2020-01-02")),
#'                                y=c(1,1),
#'                                label=c("Total Detections","% of Detections in Over 65s")),
#'              aes(x=x, y=y, colour=label, shape=label)) +
#'   scale_color_manual(name='', values=c("Total Detections"="#007C91",
#'                                        "% of Detections in Over 65s"="purple")) +
#'   scale_shape_manual(name='', values=c("Total Detections"="triangle",
#'                                        "% of Detections in Over 65s"="asterisk")) +
#'   theme(legend.position="top")
#'
#' over65_chart
#'
#'
#' }
#'
epi_curve <- function(
                        dynamic = FALSE,
                        #base = NULL,
                        params = list(
                          df = NULL,
                          y = NULL,
                          date_var = NULL,
                          date_start = NULL,
                          date_end = NULL,
                          time_period = "use_date_var",
                          group_var = NULL,
                          group_var_barmode = 'stack',
                          fill_colours = "lightblue",
                          bar_border_colour = "transparent",
                          rolling_average_line = FALSE,
                          rolling_average_line_lookback = 7,
                          rolling_average_line_colour = "red",
                          rolling_average_line_width = 1,
                          rolling_average_line_legend_label = "Rolling average",
                          cumulative_sum_line = FALSE,
                          cumulative_sum_line_colour = "darkblue",
                          cumulative_sum_line_width = 1,
                          cumulative_sum_line_legend_label = "Cumulative sum",
                          cumulative_sum_line_axis_title = "Cumulative Sum",
                          chart_title = NULL,
                          chart_title_size = 13,
                          chart_title_colour = "black",
                          chart_footer = NULL,
                          chart_footer_size = 12,
                          chart_footer_colour = "black",
                          x_axis_title = NULL,
                          y_axis_title = NULL,
                          x_axis_label_angle = NULL,
                          y_axis_label_angle = NULL,
                          x_axis_reverse = FALSE,
                          y_limit_min = NULL,
                          y_limit_max = NULL,
                          x_axis_break_labels = NULL,
                          y_axis_break_labels = NULL,
                          x_axis_n_breaks = NULL,
                          y_axis_n_breaks = NULL,
                          x_axis_date_breaks = NULL,
                          st_theme = NULL,
                          show_gridlines = TRUE,
                          show_axislines = TRUE,
                          legend_title = "",
                          legend_pos = "right",
                          hline = NULL,
                          hline_colour = "black",
                          hline_width = 0.5,
                          hline_type = "dashed",
                          hline_label = NULL,
                          hline_label_colour = "black"
                        )
                  ) {


  # Solve warnings regarding font family not found using utils/set_Arial() function
  set_Arial()


  # Where relevant, assign defaults to any parameters not specified by the user
  if(!exists('group_var_barmode',where=params)) params$group_var_barmode <- "stack"
  if(!exists('time_period',where=params)) params$time_period <- "use_date_var"
  if(!exists('fill_colours',where=params)) params$fill_colours <- "lightblue"
  if(!exists('bar_border_colour',where=params)) params$bar_border_colour <- "transparent"
  if(!exists('rolling_average_line',where=params)) params$rolling_average_line <- FALSE
  if(!exists('rolling_average_line_lookback',where=params)) params$rolling_average_line_lookback <- 7
  if(!exists('rolling_average_line_colour',where=params)) params$rolling_average_line_colour <- "red"
  if(!exists('rolling_average_line_width',where=params)) params$rolling_average_line_width <- 1
  if(!exists('rolling_average_line_legend_label',where=params)) params$rolling_average_line_legend_label <- "Rolling average"
  if(!exists('cumulative_sum_line',where=params)) params$cumulative_sum_line <- FALSE
  if(!exists('cumulative_sum_line_colour',where=params)) params$cumulative_sum_line_colour <- "darkblue"
  if(!exists('cumulative_sum_line_width',where=params)) params$cumulative_sum_line_width <- 1
  if(!exists('cumulative_sum_line_legend_label',where=params)) params$cumulative_sum_line_legend_label <- "Cumulative sum"
  if(!exists('cumulative_sum_line_axis_title',where=params)) params$cumulative_sum_line_axis_title <- "Cumulative Sum"
  if(!exists('chart_title_size',where=params)) params$chart_title_size <- 12
  if(!exists('chart_title_colour',where=params)) params$chart_title_colour <- "black"
  if(!exists('chart_footer_size',where=params)) params$chart_footer_size <- 10
  if(!exists('chart_footer_colour',where=params)) params$chart_footer_colour <- "black"
  if(!exists('x_axis_label_angle',where=params)) params$x_axis_label_angle <- 0
  if(!exists('y_axis_label_angle',where=params)) params$y_axis_label_angle <- 0
  if(!exists('x_axis_reverse',where=params)) params$x_axis_reverse <- FALSE
  if(!exists('show_gridlines',where=params)) params$show_gridlines <- TRUE
  if(!exists('show_axislines',where=params)) params$show_axislines <- TRUE
  if(!exists('legend_title',where=params)) params$legend_title <- ""
  if(!exists('legend_pos',where=params)) params$legend_pos <- "right"
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
  params <- param_rename(params,"date_var","x")
  params <- param_rename(params,"date_start","x_limit_min")
  params <- param_rename(params,"date_end","x_limit_max")


  # 'base' not a user define arguement for epi_curve, so set to NULL
  #   for base_gg() and base_plotly()
  base <- NULL



print(params)



  ##### Checks and warnings

  # Check if df is is.null
  if (!exists('df',where=params)) stop("A data frame argument is required")

  # Check df is a df class
  if(!is.data.frame(params$df)) stop("df is not a data frame object")

  # Check df is empty
  if(!not_empty(params$df)) stop("df is empty")

  # Check group_var_barmode valid
  if (!is.null(params$group_var) & !(params$group_var_barmode %in% c('stack', 'group'))) {
    stop("group_var_barmode must equal 'stack' or 'group'")
  }


  # # Check if x argument is is.null
  # if ((is.null(params$x)) | !exists('x',where=params))
  #   stop("Please include a variable from df for x, i.e. x = \"variable_name\"")
  #
  # # Check if y argument is is.null
  # if ((is.null(params$y)) | !exists('y',where=params))
  #   stop("Please include a variable from df for y, i.e. y = \"variable_name\"")
  #
  # # Check if x is in df
  # if (!params$x %in% colnames(params$df))
  #   stop("x not found within df. Please include a variable from df for x, i.e. x = \"variable_name\"")
  #
  # # Check if y is in df
  # if (!params$y %in% colnames(params$df))
  #   stop("y not found within df. Please include a variable from df for y, i.e. y = \"variable_name\"")
  #
  # # Check if number of groups and number of point colours are the same
  # if (exists('group_var', where=params)) {
  #     if (length(params$point_colours) != length(unique(params$df[[params$group_var]])))
  #       stop("The number of point_colours provided must equal the number of unique groups in group_var")
  # }
  #
  # # Check if number of groups and number of error colours are the same
  # if (exists('group_var', where=params) & exists('ci_colours', where=params) & (exists('ci', where=params))) {
  #     if (length(params$ci_colours) != length(unique(params$df[[params$group_var]])))
  #       stop("The number of error_colours provided must equal the number of unique groups in group_var")
  # }
  #
  # # Warn that multiple colours have been provided but group_var absent
  # if (length(params$point_colours) >1 & !exists('group_var',where=params))
  #   warning("Multiple point_colours have been provided but group_var is absent")
  #
  # # Allow axis_break_labels or axis_n_breaks
  # if ((!is.null(params$x_axis_break_labels)) & (!is.null(params$x_axis_n_breaks)))
  #   stop("x_axis_break_labels cannot be provided with x_axis_n_breaks, please provide
  #        x_axis_break_labels OR x_axis_n_breaks")
  # if ((!is.null(params$y_axis_break_labels)) & (!is.null(params$y_axis_n_breaks)))
  #   stop("y_axis_break_labels cannot be provided with y_axis_n_breaks, please provide
  #          y_axis_break_labels OR y_axis_n_breaks")
  #
  # # Allow x_axis_break_labels or x_axis_date_breaks
  # if ((!is.null(params$x_axis_break_labels)) & (!is.null(params$x_axis_date_breaks)))
  #   stop("x_axis_break_labels cannot be provided with x_axis_date_breaks, please provide
  #          x_axis_break_labels OR x_axis_date_breaks")
  #
  # # Warn that x_axis_date_breaks cannot be used with a reversed x-axis
  # if ((params$x_axis_reverse == TRUE) & (!is.null(params$x_axis_date_breaks)))
  #   warning("x_axis_date_breaks cannot be used with a reversed x-axis, consider using
  #             x_axis_break_labels instead")
  #
  # # Warn that point_size_legend is not available for dynamic plot
  # if ((params$point_size_legend == TRUE) & (dynamic == TRUE)) {
  #   warning("point_size_legend is not available for dynamic plots")
  # }
  #
  # # Error that base must be provided if sec_axis = TRUE
  # if ((params$y_sec_axis == TRUE) & is.null(base)) {
  #   stop("base must be provided if y_sec_axis = TRUE")
  # }
  #
  # # Error that base must align with output type
  # #   i.e. if dynamic = TRUE then base must be a plotly object
  # if (!is.null(base)) {
  #     # dynamic
  #     if ((dynamic == TRUE) & !("plotly" %in% class(base))) {
  #       stop("base must be a plotly object if dynamic = TRUE")
  #     }
  #     # static
  #     if ((dynamic == FALSE) & !(is.ggplot(base))) {
  #       stop("base must be a ggplot object if dynamic = FALSE")
  #     }
  # }



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
                 "time_period",
                 "group_var",
                 "group_var_barmode",
                 "fill_colours",
                 "bar_border_colour",
                 "rolling_average_line",
                 "rolling_average_line_lookback",
                 "rolling_average_line_colour",
                 "rolling_average_line_width",
                 "rolling_average_line_legend_label",
                 "cumulative_sum_line",
                 "cumulative_sum_line_colour",
                 "cumulative_sum_line_width",
                 "cumulative_sum_line_legend_label",
                 "cumulative_sum_line_axis_title",
                 "chart_title",
                 "chart_footer",
                 "chart_title_size",
                 "chart_title_colour",
                 "chart_footer_size",
                 "chart_footer_colour",
                 "x_axis_title",
                 "x_axis_label_angle",
                 "y_axis_title",
                 "y_axis_label_angle",
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
                 "hline",
                 "hline_colour",
                 "hline_width",
                 "hline_type",
                 "hline_label",
                 "hline_label_colour",
                 "ci_lower",   # required as NULL for base_gg() / base_plotly()
                 "ci_upper"    # required as NULL for base_gg() / base_plotly()
                 )
               )



  ##### DATA MANIPULATION


  ### Define full date range

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


  ### Add time periods to df

  if (time_period != "use_date_var") {

    # Use utils/adorn_dates() function to add additional date variables to df
    df <- adorn_dates(df, x)

    # Redefine x so that it points towards the relevant date column
    x <- time_period

  }


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


print(unique_dates)
print(head(df))



  ### Add totals by date to df (i.e. defining y-axis)

  if (is.null(y)) {

    if(is.null(group_var)) {

      df <- df |>
        group_by(date_factor) |>
        #group_by(.data[[x]]) |>
        summarise(n = n()) |>
        ungroup()

    } else {

      df <- df |>
        group_by(date_factor, .data[[group_var]]) |>
        #group_by(.data[[x]], .data[[group_var]]) |>
        summarise(n = n()) |>
        ungroup()

    }

    # Redefine x so that it points towards date_factor
    x <- "date_factor"
    # Re-define y so that it points towards n
    y <- "n"

  }



  # Add rolling average df if specified
  if (rolling_average_line == TRUE) {

    # Create df for rolling average depending on whether data is grouped
    if(is.null(group_var)) {

      # Un-grouped
      df_rolling_average <- df |>
        mutate(
               # rolling_averge = zoo::rollmean(x = get(y),
               #                                k = rolling_average_line_lookback,
               #                                fill = NA,
               #                                align = "right"),
               rolling_average = slider::slide_dbl(
                                            .x = get(y),
                                            .f = mean,
                                            .before = rolling_average_line_lookback - 1, # -1 as .before includes current time interval
                                            .complete = FALSE
                                            )
               )

    } else {

      # Grouped
      df_rolling_average <- df |>
        group_by(date_factor) |>
        summarise(n = sum(n)) |>
        ungroup() |>
        mutate(rolling_average = slider::slide_dbl(
                  .x = get(y),
                  .f = mean,
                  .before = rolling_average_line_lookback - 1, # -1 as .before includes current time interval
                  .complete = FALSE
                )
              )

    }

  }



  # Add cumulative sum df if specified
  if (cumulative_sum_line == TRUE) {

    # Create df for rolling average depending on whether data is grouped
    if(is.null(group_var)) {

      # Un-grouped
      df_cumulative_sum <- df |>
        mutate(cumulative_sum = cumsum(get(y)))

    } else {

      # Grouped
      df_cumulative_sum <- df |>
        group_by(date_factor) |>
        summarise(n = sum(n)) |>
        ungroup() |>
        mutate(cumulative_sum = cumsum(get(y)))

    }
print(head(df_cumulative_sum,10))
  }




#print(head(df,30), n = 30)




  #################### EPI CURVE #################################

  ##### CREATE STATIC CHART

  if (!dynamic) {
    # produce ggplot object if 'dynamic' is set to FALSE


  # Supress spurious 'Scale for X is already present' messages
  #   -Will suppress other messages
  ###suppressMessages({



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




  ##### Build epi curve

  # Build according to whether plotting variables are grouped or not
  if(is.null(group_var)) {

    # Create epi curve without groups

      base <-
        base + geom_bar(
          data = df,
          mapping = aes(x = .data[[x]],
                        y = .data[[y]]
                        ),
          fill = fill_colours[1],
          stat = 'identity',
          color = bar_border_colour
        )

  } else {

    # Create epi curve with groups

      # Adjust group_var_barmode for ggplot
      if(group_var_barmode == "group") {group_var_barmode <- "dodge"}

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
          position = group_var_barmode
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



  ##### Add rolling average line if specified

  if (rolling_average_line == TRUE) {

    base <-
      base + geom_line(
        data = df_rolling_average,
        mapping = aes(x = .data[[x]],
                      y = rolling_average,
                      color = rolling_average_line_legend_label,
                      group = 1   # group = 1 as x is a factor
        ),
        size = rolling_average_line_width
      )

  }



  ##### Add cumulative sum line if specified

  if (cumulative_sum_line == TRUE) {

    # Calculate scale factor

    # Get limits of current plotted data, use y_limits instead if provided.
    current_plotted_data_max <-
      if (!is.null(y_limit_max)) {y_limit_max} else {max(layer_scales(base)$y$range$range)}
    current_plotted_data_min <-
      if (!is.null(y_limit_min)) {y_limit_min} else {min(layer_scales(base)$y$range$range)}

    # Get limits of cumsum for secondary axis scale
    cumsum_max <- max(df_cumulative_sum$cumulative_sum, na.rm=T)
    cumsum_min <- min(df_cumulative_sum$cumulative_sum, na.rm=T)

    # Define scale
    scale <- (current_plotted_data_max - current_plotted_data_min) / (cumsum_max - cumsum_min)


    # Plot cumulative sum line
    base <-
      base + geom_line(
        data = df_cumulative_sum,
        mapping = aes(x = .data[[x]],
                      y = cumulative_sum * scale,
                      color = cumulative_sum_line_legend_label,
                      group = 1   # group = 1 as x is a factor
        ),
        size = cumulative_sum_line_width
      )

    # Define variable for use in scale_y_continuous below
    sec_axis_var <- sec_axis(~ . / scale, name = cumulative_sum_line_axis_title)

  }



  ##### Add rolling average + cumsum line legends

  # Rolling average only
  if (rolling_average_line == TRUE & cumulative_sum_line == FALSE) {

    base <- base +
      scale_color_manual(name = "", values = rolling_average_line_colour)

  # Cumulative sum only
  } else if (rolling_average_line == FALSE & cumulative_sum_line == TRUE) {

    base <- base +
      scale_color_manual(name = "", values = cumulative_sum_line_colour)

  # Both
  } else if (rolling_average_line == TRUE & cumulative_sum_line == TRUE) {

    base <- base +
      scale_color_manual(name = "",
                         values = c(rolling_average_line_colour, cumulative_sum_line_colour))

  }



  ##### Redefine elements of base_gg specific to epi_curve

  # Redefine x-axis label so that default = time_period
  if (!is.null(x_axis_title)) {
    base <- base + labs(x = x_axis_title)
  } else {
    base <- base + labs(x = time_period) # default to time_period if label not provided
  }


  # Redefine y-axis scale to remove gap between bars and x-axis

  # Waiver secondary y-axis scale if no cumulative sum line specified
  if (cumulative_sum_line == FALSE) {sec_axis_var <- waiver()}

  if (!is.null(y_axis_break_labels)) {
    base <- base + scale_y_continuous(breaks = y_axis_break_labels,
                                      expand = c(0,0),
                                      sec.axis = sec_axis_var)
  } else {
    base <- base + scale_y_continuous(expand = c(0,0),
                                      sec.axis = sec_axis_var)
  }




  ##### Return final output
  return(base)

  ###}) # suppressMessage() end

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
    ggobj <- ggplot() + geom_point(data=df, aes(x=.data[[x]],y=.data[[y]])) + geom_hline(yintercept = hline)
    x_min <- ggplot_build(ggobj)$layout$panel_params[[1]]$x.range[1]
    x_max <- ggplot_build(ggobj)$layout$panel_params[[1]]$x.range[2]
    y_min <- ggplot_build(ggobj)$layout$panel_params[[1]]$y.range[1]
    y_max <- ggplot_build(ggobj)$layout$panel_params[[1]]$y.range[2]

    # Handle dates converting to numeric when extracted from ggplot axis range
    x_min <- if(lubridate::is.Date(df[[x]])) {as.Date.numeric(x_min)} else {x_min}
    x_max <- if(lubridate::is.Date(df[[x]])) {as.Date.numeric(x_max)} else {x_max}
    y_min <- if(lubridate::is.Date(df[[y]])) {as.Date.numeric(y_min)} else {y_min}
    y_max <- if(lubridate::is.Date(df[[y]])) {as.Date.numeric(y_max)} else {y_max}



    ##### Create base plotly object

    # Define base plotly object using R/base_plotly() function
    #    -Force base_plotly() to run in calling environment so it can find variables
    #     (lexical scoping will cause it to look for unfound variables in /R where
    #     it's stored rather than within the calling function's environment)
    environment(base_plotly) <- environment()
    base_return <- base_plotly()

    # base_plotly() returns a list containing base, df, and the y_axis_choice variable; extract here
    base <- base_return$base
    df <- base_return$df
    y_axis_choice <- base_return$y_axis_choice




    ##### Apply confidence intervals

    # Apply before main point-plot so that points appear in front of bars / ribbons.

    # Add conf intervals if arguments for ci and ci_upper+ci_lower bounds are provided.
    if(!is.null(ci)) {

      # Stop if ci_upper and/or ci_lower limit isn't provided
      if(is.null(ci_lower) | is.null(ci_upper)) {
        stop("Please provide arguements for 'ci_upper' and 'ci_lower' when ci is specified.")
      }


      # Plot for no group_var
      if (is.null(group_var)) {

        # Add error bars or ribbon depending upon ci arguement

        # Add error bars without grouping variable
        if(ci == 'errorbar') {

          # Plotly error bars require upper and lower error divergence rather
          #   than values, so calculate
          df <- df |>
            mutate(diff_ci_lower = get(y) - get(ci_lower),
                   diff_ci_upper = get(ci_upper) - get(y))

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
                color = ci_colours,
                line = list(colour = '#ffffff00', width = 0),
                symbol = 'line-ew-open' # use hozizontal line markers so that horizontal line symbol will be used in legned to match ggplot legend formatting
              ),
              hoverinfo='none',
              error_y = list(
                type = "data",
                symmetric = FALSE,
                color = ci_colours,
                thickness = 1,
                arrayminus = ~ diff_ci_lower,
                array = ~ diff_ci_upper
              )
            ) |>
            # Add additional trace of white horizontal line scatter-markers
            #   to mask original markers
            add_trace(
              data = df,
              x = ~ df[[x]],
              y = ~ df[[y]],
              type = 'scatter',
              mode = 'markers',
              yaxis = y_axis_choice,
              hoverinfo='none',
              showlegend = F,
              marker = list(
                color = 'white',
                line = list(colour = 'white', width=2),
                symbol = 'line-ew-open'
              )
            )

          # Add ribbon without grouping variable
        } else if (ci == 'ribbon') {

          base <- base |>
            add_ribbons(
              x = ~ df[[x]],
              ymin = ~ df[[ci_lower]],
              ymax = ~ df[[ci_upper]],
              yaxis = y_axis_choice,
              hoverinfo='none',
              showlegend = ci_legend,
              legendgroup = 'ci',
              #legendgrouptitle = list(text = ci_legend_title),
              name = ci_legend_title,
              fillcolor = yarrr::transparent(ci_colours, trans.val = .5), # add transparency using 'yarrr' package
              line = list(color = 'transparent')
            )

        }


        # Plot for group_var provided
      } else if (!is.null(group_var)) {

        # Add error bars or ribbon depending upon ci arguement

        # Add error bars with grouping variable
        if(ci == 'errorbar') {

          # Errorbar traces must be defined individually for each group

          # Define unique groups
          unique_groups <- unique(df[[group_var]])

          # Iterate over each group
          for (i in 1:length(unique_groups)) {

            # Plotly error bars require upper and lower error divergence rather
            #   than values, so create df for each group and calculate
            df_group <- df |>
              filter(get(group_var) == unique_groups[i]) |>
              mutate(diff_ci_lower = get(y) - get(ci_lower),
                     diff_ci_upper = get(ci_upper) - get(y))

            # Add error bars as trace with invisible markers
            base <- base |>
              add_trace(
                data = df_group,
                x = df_group[[x]],
                y = df_group[[y]],
                type = 'scatter',
                mode = 'markers',
                yaxis = y_axis_choice,
                name = unique_groups[[i]],
                showlegend = F,
                marker = list(
                  color = '#ffffff00',
                  line = list(colour = '#ffffff00', width = 0)
                ),
                hoverinfo='none',
                error_y = list(
                  type = "data",
                  symmetric = FALSE,
                  color = ci_colours[[i]],
                  thickness = 1,
                  arrayminus = ~ diff_ci_lower,
                  array = ~ diff_ci_upper
                )
              )
          }

        # Add ribbon with grouping variable
        } else if (ci == 'ribbon') {

          # Ribbons must be defined individually for each group

          # Define unique groups
          unique_groups <- unique(df[[group_var]])

          # Iterate over each group
          for (i in 1:length(unique_groups)) {

            #df_group <- df[which(df[[group_var]]==unique_groups[[i]]), ]

            # Need to construct polygon for each ribbon
            #   -Points must be connected in order, so must go in positive direction
            #    along upper limit and negative direction along lower limit. Define
            #    separate dfs for each, reverse order of lower lim df, and bind together.
            df_group_low <- df |>
              filter(get(group_var) == unique_groups[i]) |>
              select(any_of(c(x, ci_lower))) |>     # any_of as x and ci_lower point towards characters rather than variable names
              dplyr::rename("y_val" = 2)

            df_group_up <- df |>
              filter(get(group_var) == unique_groups[i]) |>
              select(any_of(c(x, ci_upper))) |>     # any_of as x and ci_upper point towards characters rather than variable names
              dplyr::rename("y_val" = 2)

            df_group_ribb <- rbind(df_group_low, (df_group_up |> arrange(desc(row_number()))))


            # Add separate ribbon for each group using df defined above
            base <- base |>
              add_trace(
                data = df_group_ribb,
                x = ~ df_group_ribb[[x]],
                y = ~ y_val,
                type = 'scatter',
                mode = 'lines',
                yaxis = y_axis_choice,
                name = unique_groups[[i]],
                line = list(color = 'transparent'),
                fill = 'toself',
                #fillcolor = paste0(ci_colours[[i]],'80')
                fillcolor = yarrr::transparent(ci_colours[[i]], trans.val = .5), # add transparency using 'yarrr' package
                showlegend = ci_legend,
                legendgroup = 'ci',
                legendgrouptitle = list(text = ci_legend_title)
              )

          }

        }

      }

    }



    ##### Resolve point style

    # Note:- Limited to 7 at present; expand

    # Create ggplot -> plotly point-shape key
    ggplot_point_shapes <- c('circle','triangle','square','plus','square cross','asterisk','diamond')
    plotly_point_shapes <- c('circle','triangle-up','square','cross-thin-open','square-x-open','asterisk-open','diamond')
    point_shapes_key <- setNames(as.list(plotly_point_shapes),ggplot_point_shapes)

    # Stop if point_shape is not in list
    if (!(point_shape %in% ggplot_point_shapes)) {
      stop(
        "Invalid point type. Please provide one of the following point types:
           'circle', 'triangle', 'square', 'plus', 'square cross', 'asterisk', 'diamond'"
      )
    }


    ##### Define default hover labels

    if (is.null(ci)) {

        hoverlabels <- paste0('<b>%{x}</b>',
                              '<br>%{y}')
    } else {

      hoverlabels <- paste0('<b>%{x}</b>',
                            '<br>%{y}',
                            '<br><i>Upper: %{text}</i>',
                            '<br><i>Lower: %{customdata}</i>')
    }



    ##### Create point chart

    # Build according to whether plotting variables are grouped or not
    if (is.null(group_var)) {

      # Replace default hoverlabels if point_labels is defined
      if (is.null(ci)) {
        # leverage 'text' and 'customdata' fields to include ci limits in default hover labels
        #     -Included in trace below
        text_upper <- df[[x]]
        text_lower <- df[[x]]  # arbitary table field to keep tibble sizes the same, not included in label here anyway
      } else {
        text_upper <- if(y_percent==TRUE) {scales::percent(df[[ci_upper]])} else {df[[ci_upper]]}
        text_lower <- if(y_percent==TRUE) {scales::percent(df[[ci_lower]])} else {df[[ci_lower]]}
      }

      if(!is.null(point_labels)) {
        hoverlabels <- as.character(df[[point_labels]])
      }

      # Take into account user-defined point size

        # If point size is a fixed number, define a vector containing that number
        if (is.numeric(point_size) & length(point_size) ==1) {

          plotly_point_sizes <- rep(point_size*3, nrow(df))   # *3 to scale ggplot to plotly (approx)

        } else {
        # If point size is a variable

            # Define ggplot geom_point object to harvest point sizes from so that this can
            #   be fed into the plotly chart.
            ggsizeobj <- ggplot() +
              geom_point(data = df, aes(x = .data[[x]], y = .data[[y]], size = .data[[point_size]]))

            plotly_point_sizes <- (ggplot_build(ggsizeobj)$data[[1]]$size)*3   # *3 to scale ggplot to plotly (approx)
        }

      # Add plotly trace without groups
      base <- base |>
        add_trace(
          df,
          x = ~ df[[x]],
          y = ~ df[[y]],
          type = 'scatter',
          mode = 'markers',
          yaxis = y_axis_choice,
          marker = list(
            color = point_colours,
            symbol = point_shapes_key[[point_shape]],
            size = plotly_point_sizes,
            line = list(colour = 'transparent', width = 0)
            ),
          legendgroup = 'data',
          #legendgrouptitle = list(text = legend_title),
          name = if(legend_title != "") {legend_title} else {y},
          # leverage 'text' and 'customdata' fields to include ci limits in default hover labels
          text = text_upper,
          customdata = text_lower,
          hovertemplate = paste0(hoverlabels,"<extra></extra>") # Remove tooltip for ungrouped data
        )

    } else {

      # Add plotly trace with groups

      # base <- base |>
      #   add_trace(
      #     df,
      #     x = ~ df[[x]],
      #     y = ~ df[[y]],
      #     type = "scatter",
      #     mode = "markers",
      #     symbol = ~ df[[group_var]],
      #     symbols = plotly_point_shapes,
      #     color = ~ df[[group_var]],
      #     colors = point_colours
      #   )

      # Define unique groups
      unique_groups <- unique(df[[group_var]])

      # Iterate over each group
      for (i in 1:length(unique_groups)) {

        df_group <- df |>
          filter(get(group_var) == unique_groups[i])

        # Replace default hoverlabels if point_labels is defined
        if (is.null(ci)) {
          # leverage 'text' and 'customdata' fields to include ci limits in default hover labels
          #     -Included in trace below
          text_upper <- df_group[[x]]
          text_lower <- df_group[[x]]  # arbitary table field to keep tibble sizes the same, not included in label here anyway
        } else {
          text_upper <- if(y_percent==TRUE) {scales::percent(df_group[[ci_upper]])} else {df_group[[ci_upper]]}
          text_lower <- if(y_percent==TRUE) {scales::percent(df_group[[ci_lower]])} else {df_group[[ci_lower]]}
        }

        if(!is.null(point_labels)) {
          hoverlabels <- as.character(df_group[[point_labels]])
          }


        # Take into account user-defined point size

          # If point size is a fixed number, define a vector containing that number
          if (is.numeric(point_size) & length(point_size) ==1) {

            plotly_point_sizes_group <- rep(point_size*3, nrow(df_group))   # *3 to scale ggplot to plotly (approx)

          } else {
            # If point size is a variable

            # Define ggplot geom_point object to harvest point sizes from so that this can
            #   be fed into the plotly chart.
            ggsizeobj <- ggplot() +
              geom_point(data = df_group, aes(x = .data[[x]], y = .data[[y]], size = .data[[point_size]]))

            plotly_point_sizes_group <- (ggplot_build(ggsizeobj)$data[[1]]$size)*3   # *3 to scale ggplot to plotly (approx)
          }

        base <- base |>
          add_trace(
            data = df_group,
            x = df_group[[x]],
            y = df_group[[y]],
            type = 'scatter',
            mode = 'markers',
            yaxis = y_axis_choice,
            name = unique_groups[[i]],
            marker = list(
              color = point_colours[[i]],
              symbol = plotly_point_shapes[[i]],
              size = plotly_point_sizes_group,
              line = list(color = 'transparent', width = 0)
              ),
            legendgroup = 'data',
            # leverage 'text' and 'customdata' fields to include ci limits in default hover labels
            #   Account for y_percent = TRUE
            text = text_upper,
            customdata = text_lower,
            legendgrouptitle = list(text = legend_title),
            hovertemplate = hoverlabels
          )

      }

    }


    ##### Apply point legend parameters

    # Legend position
    if (!is.null(legend_pos)) {

      # Let utils/plotly_legend_pos() function run in calling environment so that
      #    it can access point_chart arguements
      environment(plotly_legend_pos) <- environment()

      if (legend_pos!="none") {
        base <- base |> layout(legend = plotly_legend_pos(legend_pos))  # use utils/plotly_legend_pos() function to switch between ggplot and plotly legend params
      } else {
        base <- base |> layout(showlegend = F)
      }

    }

    # Legend font
    base <- base |>
      layout(
        legend = list(
          #traceorder = "grouped+reversed",
          font=list(size=8)
          )
        )


  # return base plot
  return(base)

    } ### DYNAMIC CHART END


}






