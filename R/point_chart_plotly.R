#' Point chart
#'
#' The chart function has 3 mandatory arguments to plot, with additional
#' arguments as described further below:
#'
#'
#' @param df name of data frame for plotting
#' @param base base
#' @param x name of x axis variable in data frame
#' @param y name of y axis variable in data frame
#' @param group_var name of group variable to generate multiple lines
#' (if required) in data frame
#' @param y_axis y_axis
#' @param ci indicator for using ribbon or error bar geom (if required),
#' enter 'errorbar' for error bar, enter 'ribbon' for ribbon
#' @param lower lower value for error \ ribbon geom (mandatory if ci argument passed)
#' @param upper upper value for error \ ribbon geom (mandatory if ci argument passed)
#' @param error_colours if not plotting by group this is the colour of the error
#' bars or ribbon
#' @param hline will display a horizontal line if valid inter passed
#' provided where multiple lines generated each will be of different type
#' @param y_label for provision of an y axis label
#' @param x_label for provision of an x axis label
#' @param x_label_angle to adjust the x axis label by the degrees of the
#' integer provided
#' @param y_label_angle to adjust the y axis label by the degrees of the
#' integer provided
#' @param x_labels_reverse enter an argument of any value i.e. 'y' to reverse
#' the x labeling order when using categorical data
#' @param y_min_limit set the limit on the y axis scaling by proving an integer
#' @param y_max_limit set the limit on the x axis scaling by proving an integer
#' @param x_axis_breaks modify the x axis breaks by providing an integer
#' @param legend_pos modify the position of the legend (where applicable) with
#' appropriate value i.e. bottom (default position),
#' top, right, left
#' @param remove_gridlines enter an argument of any value i.e. 'y' to remove
#' the grid lines
#' @param y_percent enter an argument of any value i.e. 'y' to include the %
#' symbol for y axis labels
#' @param chart_footer enter text for a caption to appear below plot
#' @param labels labels to display alongside the plotted geom_points
#' @param labels_hjust the horizontal adjustment of the point labels
#' @param labels_vjust the vertical adjustment of the point labels
#' @param point_shape shape of the points to be plotted (only used when not plotting
#' by group)
#' @param y_sec_axis_no_shift If no shift should be applied to the secondary y-axis
#'
#' @import assertthat
#' @import grDevices
#' @import scales
#'
#' @return the final plot
#' @export
#'
#' @examples
#'
#' # This example will plot the points on the secondary y-axis with error bars and
#' # labels showing
#' # the plotted values that are vertically adjusted from the plotted points
#' \dontrun{
#' point_chart(df = plot_df, x = "age", y = "value", ci = "e", upper = "uppercl",
#' lower = "lowercl", group_var = "ukborn", y_axis = "y2", y_label = "Value",
#' labels = "value", vjust = -3.5)
#' }
#'
point_chart_plotly <- function(
                        dynamic = FALSE,
                        base = NULL,
                        params = list(
                          df = NULL,
                          x = NULL,
                          y = NULL,
                          ci = NULL,
                          ci_legend = FALSE,
                          ci_legend_title = "Confidence interval",
                          lower = NULL,
                          upper = NULL,
                          error_colours = "red",
                          errorbar_width = NULL,
                          group_var = NULL,
                          point_shape = "triangle",
                          point_colours = "blue",
                          point_labels = NULL,
                          point_labels_hjust = 0,
                          point_labels_vjust = 0,
                          point_labels_nudge_x = 0,
                          point_labels_nudge_y = 0,
                          #y_axis = "y1",              ##add to line_chart?
                          y_sec_axis = FALSE,
                          y_sec_axis_no_shift = FALSE,           ##add to line_chart?
                          chart_title = NULL,         # change to chart_title in line_chart
                          chart_footer = NULL,        ##add to line_chart?
                          x_label = NULL,
                          x_label_angle = NULL,
                          y_label = NULL,
                          y_label_angle = NULL,
                          y_percent = NULL,
                          st_theme = NULL,
                          x_labels_reverse = FALSE,    ##add to line_chart?
                          y_limit_min = NULL,         ##add to line_chart?
                          y_limit_max = NULL,          ##add to line_chart?
                          x_limit_min = NULL,         ##add to line_chart?
                          x_limit_max = NULL,          ##add to line_chart?
                          x_axis_breaks = NULL,       ##add to line_chart?
                          show_gridlines = FALSE,
                          show_axislines = TRUE,
                          legend_title = "",
                          legend_pos = "bottom",      # change to legend_pos in line_chart
                          hline = NULL,
                          hline_colour = "black",
                          hline_width = 0.5,          ##add to line_chart?
                          hline_type = "dashed",      ##add to line_chart?
                          hline_label = NULL,
                          hline_label_colour = "black"
                        )
                  ) {


  # Solve warnings regarding font family not found using utils/set_Arial() function
  set_Arial()


  # Assign any null default args to params list
  if(!exists('ci_legend',where=params)) params$ci_legend <- FALSE
  if(!exists('ci_legend_title',where=params)) params$ci_legend_title <- "Confidence interval"
  if(!exists('error_colours',where=params)) params$error_colours <- "red"
  if(!exists('errorbar_width',where=params)) params$errorbar_width <- NULL
  if(!exists('point_shape',where=params)) params$point_shape <- "triangle"
  if(!exists('point_colours',where=params)) params$point_colours <- "blue"
  if(!exists('point_labels_hjust',where=params)) params$point_labels_hjust <- 0
  if(!exists('point_labels_vjust',where=params)) params$point_labels_vjust <- 0
  if(!exists('point_labels_nudge_x',where=params)) params$point_labels_nudge_x <- 0
  if(!exists('point_labels_nudge_y',where=params)) params$point_labels_nudge_y <- 0
  #if(!exists('y_axis',where=params)) params$y_axis <- "y1"
  if(!exists('y_sec_axis',where=params)) params$y_sec_axis <- FALSE
  if(!exists('y_sec_axis_no_shift',where=params)) params$y_sec_axis_no_shift <- FALSE
  if(!exists('x_labels_reverse',where=params)) params$x_labels_reverse <- FALSE
  if(!exists('show_gridlines',where=params)) params$show_gridlines <- FALSE
  if(!exists('show_axislines',where=params)) params$show_axislines <- TRUE
  if(!exists('legend_title',where=params)) params$legend_title <- ""
  if(!exists('legend_pos',where=params)) params$legend_pos <- "bottom"
  if(!exists('hline_colour',where=params)) params$hline_colour <- "black"
  if(!exists('hline_width',where=params)) params$hline_width <- 0.5
  if(!exists('hline_type',where=params)) params$hline_type <- "dashed"
  if(!exists('hline_label_colour',where=params)) params$hline_label_colour <- "black"



print(params) ###


  ##### Checks and warnings

  # Check if df is is.null
  if (!exists('df',where=params)) stop("A data frame argument is required")

  # Check df is a df class
  if(!is.data.frame(params$df)) stop("Argument df is not a data frame object")

  # Check if x argument is is.null
  if ((is.null(params$x)) | !exists('x',where=params))
    stop("Please include a variable from df for x, i.e. x = \"variable_name\"")

  # Check if y argument is is.null
  if ((is.null(params$y)) | !exists('y',where=params))
    stop("Please include a variable from df for y, i.e. y = \"variable_name\"")

  # Check if number of groups and number of line colours the same

  # Warn that multiple colours have been provided but group var absent



  ### Parameter assignment

  # Define parameters as variables using utils/param_assign() function
  #   -Takes input list, compares it ro a reference vector of expected
  #     list elements, assigns each element to a variable within the
  #     parent environment, and allocates a value of 'NULL' to anything
  #     it can't find within the reference list.
  param_assign(params,
               c("df","x","y","ci","ci_legend","ci_legend_title","lower",
                 "upper","error_colours","errorbar_width","group_var","point_shape",
                 "point_colours","point_labels",
                 "point_labels_hjust","point_labels_vjust","point_labels_nudge_x",
                 "point_labels_nudge_y","y_sec_axis","y_sec_axis_no_shift","chart_title",
                 "chart_footer","x_label","x_label_angle","y_label","y_label_angle",
                 "y_percent","st_theme","x_labels_reverse","y_limit_min","y_limit_max",
                 "x_limit_min","x_limit_max",
                 "x_axis_breaks","show_gridlines","show_axislines","legend_title","legend_pos",
                 "hline","hline_colour","hline_width","hline_type","hline_label",
                 "hline_label_colour"))


  # # Define parameters from params list
  # for(i in 1:length(params)) {
  #   assign(names(params)[i], params[[i]])
  # }
  #
  # # Set any unused parameter values to NULL
  # unused <- setdiff(c("df","x","y","ci","ci_legend","ci_legend_title","lower",
  #                     "upper","error_colours","group_var","point_shape","point_colours","point_labels",
  #                     "point_labels_hjust","point_labels_vjust","y_axis","y_sec_axis_no_shift","chart_title",
  #                     "chart_footer","x_label","x_label_angle","y_label","y_label_angle",
  #                     "y_percent","st_theme","x_labels_reverse","y_min_limit","y_max_limit",
  #                     "x_axis_breaks","show_gridlines","show_axislines","legend_title","legend_pos",
  #                     "hline","hline_colour","hline_width","hline_type","hline_label"),
  #                   names(params))
  #
  # if (length(unused) > 0) {
  #   for(i in 1:length(unused)) {
  #     assign(unused[i], NULL)
  #   }
  # }



  # Check that the data frame provided is not empty, else stop
  assertthat::assert_that(not_empty(df))



  # Define base ggplot object using R/base_gg() function
  #    -Force base_gg() to run in calling environment so it can find variables
  #     (lexical scoping will cause it to look for unfound variables in /R where
  #     it's stored rather than within the calling function's environment)
  environment(base_gg) <- environment()
  base_return <- base_gg()

  # base_gg() returns a list containing base and df; extract here
  base <- base_return$base
  df <- base_return$df




  #################### POINT CHART #################################

  ##### Apply confidence intervals

  # Apply before main point-plot so that points appear in front of bars / ribbons.

  # Add conf intervals if arguments for ci and upper+lower bounds are provided.
  if(!is.null(ci)) {

    if(!is.null(lower) && !is.null(upper)) {

      # Add error bars or ribbon depending upon ci arguement

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
                ymin = .data[[lower]],
                ymax = .data[[upper]]
              ),
              colour = error_colours[[1]],
              width = errorbar_width,
              linewidth = 0.5
            )

        # Add ribbon without grouping variable
        } else if (ci == 'ribbon') {

          base <-
            base + ggplot2::geom_ribbon(
              data = df,
              mapping = aes(
                x = .data[[x]],
                ymin = .data[[lower]],
                ymax = .data[[upper]],
                fill = ci_legend_title,
                group = 1
              ),
              alpha = .5,
              show.legend = show_ci_leg
            ) +
            scale_fill_manual("",values=error_colours[[1]])
        }

      # Plot for group_var provided
      } else if (!is.null(group_var)) {

        # Add error bars with grouping variable
        if(ci == 'errorbar') {

          base <-
            base + ggplot2::geom_errorbar(
              data = df,
              mapping = aes(
                x = .data[[x]],
                ymin = .data[[lower]],
                ymax = .data[[upper]],
                group = .data[[group_var]],
                colour =  .data[[group_var]]
              ),
              width = errorbar_width,
              linewidth = .5
            )

          # Add error_colours if provided
          if (length(error_colours) > 1) {
            base <- base +
              scale_colour_manual(values = error_colours)
          }

        # Add ribbon with grouping variable
        } else if (ci == 'ribbon') {

          base <-
            base +
            ggplot2::geom_ribbon(
              data = df,
              mapping = aes(
                x = .data[[x]],
                ymin = .data[[lower]],
                ymax = .data[[upper]],
                group = .data[[group_var]],
                fill = .data[[group_var]]
              ),
              alpha = .5,
              show.legend = show_ci_leg
            ) +
            labs(fill = ci_legend_title)

            # Add error_colours if provided
            if (length(error_colours) > 1) {
              base <- base +
                scale_fill_manual(values = error_colours)
            }

        }
      }

      # Stop if upper and/or lower limit isn't provided
    } else {
      stop("Please provide arguements for 'upper' and 'lower' when ci is specified.")
    }

  }



  ##### Build point chart

  # Build according to whether plotting variables are grouped or not
  if(is.null(group_var)) {

    # create base point chart without groups
    base <-
      base + ggplot2::geom_point(
        data = df,
        mapping = aes(x = .data[[x]],
            y = .data[[y]]),
        color = point_colours[[1]],
        shape = point_shape
      )

  } else {

    # creating base point chart with groups
    base <-
      base + ggplot2::geom_point(
        data = df,
        mapping = aes(
          x = .data[[x]],
          y = .data[[y]],
          group = factor(.data[[group_var]]),
          colour = factor(.data[[group_var]]),
          shape = factor(.data[[group_var]])
        )
      )

    # Add point_colours if provided
    if (length(point_colours) > 1) {
      base <- base +
        scale_colour_manual(values = point_colours)
    }


    ##### Apply point legend parameters

    if (!is.null(legend_title)) {
      base <-  base + labs(name = legend_title,
                           colour = legend_title,
                           shape = legend_title)
    }

    if (!is.null(legend_pos)) {
      base <-  base + theme(legend.position = legend_pos)
    }

  }


  ##### Apply point labels

  if (!is.null(point_labels)) {
    base <-
      base + geom_text(
        data = df,
        mapping = aes(
          x = .data[[x]],
          y = .data[[y]],
          label = .data[[point_labels]]
        ),
        hjust = point_labels_hjust,
        vjust = point_labels_vjust,
        nudge_x = point_labels_nudge_x,
        nudge_y = point_labels_nudge_y
      )
  }


  # ##### Apply secondary axis
  #
  # if (y_sec_axis == TRUE) {
  #   # apply percentage scale if invoked
  #   if (is.null(y_percent)) {
  #     base <- base +
  #       # scale_y_continuous(sec.axis = sec_axis(~scale_function(., scale, shift),
  #       #                                        name = y_label))
  #     scale_y_continuous(sec.axis = sec_axis(~ . * scale + shift,
  #                                            name = y_label))
  #   } else {
  #     base <- base +
  #       # scale_y_continuous(sec.axis = sec_axis(~scale_function(., scale, shift),
  #       #                                        name = y_label,
  #       #                                        labels = scales::label_percent()))
  #     scale_y_continuous(sec.axis = sec_axis(~ . * scale + shift,
  #                                            name = y_label,
  #                                            labels = scales::label_percent()))
  #   }
  # }

  ########################################

  return(base)

}






