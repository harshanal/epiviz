#' Line_chart
#'
#' The chart function has 3 mandatory arguments to plot, with additional arguments as described further below:
#'
#' @param df name of the dataframe plotting, and are mandatory.
#' @param x names of the x axis variable of the plot from the df and are mandatory.
#' @param y names of the y axis variable of the plot from the df and are mandatory.
#' @param grouping_col names of the grouping_col variable of the plot from the df
#' @param plot_mode default is "line" use "line+marker" if needed
#' @param color_theme is array of colours
#' @param y_label y axis title default is nothing , fill in needed
#' @param yaxis_label_angle default is 0, can set to other eg:-45,
#' @param y_rangemode set to "tozero",
#' @param y_min_limit set to the min of the y value, default is null
#' @param y_max_limit set to the max of the y value, default is null
#' @param y_as_percent default is FALSE, Only TRUE when need y as percentage
#' @param x_label xaxis title default is "", add the parameter if needed
#' @param x_axis_reverse default is False, only TRUE when you want to reverse the xaxis
#' @param xaxis_label_angle xaxis angle , for eg: 90,45, -45.
#' @param lower_limit default is NULL,assign the column if it exist in the dataframe otherwise omit
#' @param upper_limit default is NULL,assign the column if it exist in the dataframe otherwise omit
#' @param hover_text default NULL, give the info on values of x and y axis
#'  how fill if if needed.it could be designed as needed for
#' eg:paste("%{x|%Y-%m-%d}","<br>count:%{y}<extra></extra>")#'
#' @param legend_orientation default = 'h' , change to 'v' if needed
#' @param legend_x default = 0.5, changed when needed depending on the legendpostion.
#' @param legend_y default = -0.35, changed when needed depending on the legendpostion.
#' @param show_legend boolean, defaults to true,
#'
#' @return a plotly plot object
#' @export
#'
#' @import dplyr
#'
#' @examples 1: line_chart_plotly(df= df, x = date, y = count,grouping_col = character)
#'
#' @examples 2: line_chart_plotly(df= df, x = date, y = count,grouping_col = character,
#' hover_text = hover_template(paste("%{x|%Y-%m-%d}","<br>count:%{y}<extra></extra>")),
#' y_label = "count/rate", x_label = "date",xaxis_label_angle  = -45/90/45,
#' yaxis_label_angle  = 0(default),x_axis_reverse= default(FALSE),y_rangemode = "tozero",
#' plot_mode = "lines"/"lines+markers",legend_orientation = 'h'/'v',
#' legend_x =0.5, legend_y = -0.35,show_legend = FALSE,
#' lower_limit = "lower_error", upper_limit = "upper_error")
#' # if df has lower and upper limit otherwise can omit it.
#'
#'
#'
#' @examples 3: line_chart_plotly(df= df, x = year(as.factor), y = rate,grouping_col = character,
#' hover_text = "%{x} year, %{y} rate<br>",
#' y_label = "count/rate", x_label = "date",xaxis_label_angle  = -45/90/45,
#' yaxis_label_angle  = 0(default),x_axis_reverse= default(FALSE),y_rangemode = "tozero",
#' plot_mode = "lines"/"lines+markers",legend_orientation = 'h'/'v',
#' legend_x =0.5, legend_y = -0.35,show_legend = FALSE,
#' lower_limit = "lower_error", upper_limit = "upper_error")
#' # if df has lower and upper limit #' otherwise can omit it.

line_chart_plotly <- function (df,
                               x,
                               y,
                               grouping_col,
                               plot_mode = "lines",
                               color_theme = c("skyblue",
                                               "lightgreen",
                                               "lightcoral",
                                               "grey",
                                               "gold",
                                               "plum",
                                               "tan"),
                               y_label = "",
                               yaxis_label_angle = 0,
                               y_rangemode = "tozero",
                               y_min_limit = NULL,
                               y_max_limit = NULL,
                               y_as_percent = FALSE,
                               x_label = "",
                               x_axis_reverse = FALSE,
                               xaxis_label_angle = 90,
                               lower_limit = NULL,
                               upper_limit = NULL,
                               hover_text = NULL,
                               legend_orientation = 'h',
                               legend_x = 0.5,
                               legend_y = -0.35,
                               show_legend = TRUE) {
  # check if df is a dataframe
  if (!is.data.frame(df)) {
    stop (" The 'df' argument must be a dataframe")
  }

  # check if x and y columns exist in the dataframe
  if (!(x %in% names(df) && y %in% names(df))) {
    stop ("columns specified for x and or y do not exist in the dataframe")
  }

  #check if grouping_col exist, otherwise use x
  if (!missing(grouping_col) && grouping_col %in% names(df)) {
    df[[grouping_col]] <- as.factor(df[[grouping_col]])
  } else {
    grouping_col <- x
    df[[x]] <- as.factor(df[[x]])
  }


  # creating the plot
  plot <- plotly::plot_ly(
    data = df ,
    x = ~ get(x),
    y = ~ get(y),
    type = 'scatter',
    mode = plot_mode,
    color = ~ get(grouping_col),
    # a list of color passed in the parameter to select
    colors = color_theme,
    #hover text is how you want the legend to display when you hover the graph
    hovertemplate = hover_text
  )

  #check if lower and upper limits are provided and valid
  if (!is.null(lower_limit) && !is.null(upper_limit)) {
    if (!(lower_limit %in% names(df)) || !(upper_limit %in% names(df))) {
      stop ("lower and upper limit columns not found in the dataframe")
    }

    #add error bars
    plot <- plot %>% plotly::add_trace (
      error_y = list(
        type = "data",
        symmetric = FALSE,
        array = df[[upper_limit]] ,
        arrayminus = df[[lower_limit]]
      ),
      showlegend = FALSE
    )
  }

  #updating layout with yaxis and xaxis labels, angles, fontsize, y axis range,
  # reverse(xaxis), if y axis needed to be presented as percentage
  plot <- plot %>% plotly::layout(
    yaxis = list(
      title = y_label,
      titlefont = list(family = "Arial", size =
                         14),
      tickfont = list(family = "Arial", size =
                        12),
      tickangle = yaxis_label_angle,
      linewidth = 2,
      linecolor = 'black',
      showgrid = FALSE,
      range = if (!is.null(y_min_limit) &&
                  !is.null(y_max_limit))
        c(y_min_limit, y_max_limit)
      else
        NULL,
      rangemode = y_rangemode,
      tickformat = if (y_as_percent)
        ',.0%'
      else
        NULL
    ),
    xaxis = list(
      title = x_label,
      titlefont = list(family = "Arial", size =
                         14),
      tickfont = list(family = "Arial", size =
                        12),
      linewidth = 2,
      linecolor = 'black',
      tickangle = xaxis_label_angle,
      autorange = if (x_axis_reverse)
        "reversed"
      else
        NULL,
      showgrid = FALSE
    ),
    # setting legend requirements with orientation whether 'h' or'v'
    # what to include in the legend.
    legend = list(
      orientation = legend_orientation,
      # show entries horizontally
      # orders legend entries
      x = legend_x,
      y = legend_y,
      xanchor = "center",
      # use center of legend as anchor
      traceorder = "normal",
      showlegend = show_legend
    )
  )

  return(plot)



}


