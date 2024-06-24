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
#' @param error_colour if not plotting by group this is the colour of the error
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
#' @param no_shift If no shift should be applied to the secondary y-axis
#'
#' @import assertthat
#' @import grDevices
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
                          lower = NULL,
                          upper = NULL,
                          error_colour = "red",
                          group_var = NULL,
                          point_shape = "triangle",
                          point_colour = "blue",
                          labels = NULL,
                          labels_hjust = 0,
                          labels_vjust = 0,
                          y_axis = "y1",              ##add to line_chart?
                          no_shift = FALSE,           ##add to line_chart?
                          chart_title = NULL,         # change to chart_title in line_chart
                          chart_footer = NULL,        ##add to line_chart?
                          x_label = NULL,
                          x_label_angle = NULL,
                          y_label = NULL,
                          y_label_angle = NULL,
                          y_percent = NULL,
                          st_theme = NULL,
                          x_labels_reverse = NULL,    ##add to line_chart?
                          y_min_limit = NULL,         ##add to line_chart?
                          y_max_limit= NULL,          ##add to line_chart?
                          x_axis_breaks = NULL,       ##add to line_chart?
                          show_gridlines = FALSE,
                          show_axislines = TRUE,
                          legend_title = "",
                          legend_pos = "bottom",      # change to legend_pos in line_chart
                          hline = NULL,
                          hline_colour = "black",
                          hline_width = 0.5,          ##add to line_chart?
                          hline_type = "dashed",      ##add to line_chart?
                          hline_label = NULL
                        )
                  ) {


  # Solve warnings regarding font family not found using R/set_Arial.R function
  source("R/set_Arial.R")


  # Assign any is.null default args to params list
  if(!exists('ci_legend',where=params)) params$ci_legend <- FALSE
  if(!exists('error_colour',where=params)) params$error_colour <- "red"
  if(!exists('point_shape',where=params)) params$point_shape <- "triangle"
  if(!exists('point_colour',where=params)) params$point_colour <- "blue"
  if(!exists('labels_hjust',where=params)) params$labels_hjust <- 0
  if(!exists('labels_vjust',where=params)) params$labels_vjust <- 0
  if(!exists('y_axis',where=params)) params$y_axis <- "y1"
  if(!exists('no_shift',where=params)) params$no_shift <- FALSE
  if(!exists('show_gridlines',where=params)) params$show_gridlines <- FALSE
  if(!exists('show_axislines',where=params)) params$show_axislines <- TRUE
  if(!exists('legend_title',where=params)) params$legend_title <- ""
  if(!exists('legend_pos',where=params)) params$legend_pos <- "bottom"
  if(!exists('hline_colour',where=params)) params$hline_colour <- "black"
  if(!exists('hline_width',where=params)) params$hline_width <- 0.5
  if(!exists('hline_type',where=params)) params$hline_type <- "dashed"


print(params)


  ### Checks and warnings

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




  # Define parameters from params list
  for(i in 1:length(params)) {
    assign(names(params)[i], params[[i]])
  }

  # Set any unused parameter values to NULL
  unused <- setdiff(c("df","x","y","ci","ci_legend","lower",
                      "upper","error_colour","group_var","point_shape","point_colour","labels",
                      "labels_hjust","labels_vjust","y_axis","no_shift","chart_title",
                      "chart_footer","x_label","x_label_angle","y_label","y_label_angle",
                      "y_percent","st_theme","x_labels_reverse","y_min_limit","y_max_limit",
                      "x_axis_breaks","show_gridlines","show_axislines","legend_title","legend_pos",
                      "hline","hline_colour","hline_width","hline_type","hline_label"),
                    names(params))

  if (length(unused) > 0) {
    for(i in 1:length(unused)) {
      assign(unused[i], NULL)
    }
  }


  # Check that the data frame provided is not empty, else stop
  assertthat::assert_that(not_empty(df))

  if (is.null(base)) {
    base <- ggplot2::ggplot()
  }


  # If user wants to plot on the secondary y-axis
  if (y_axis == "y2") {
    # Get limits of current plotted data (returns -Inf if no data currently plotted)
    current_plotted_data_max <-
      max(layer_scales(base)$y$range$range)
    current_plotted_data_min <-
      min(layer_scales(base)$y$range$range)
    # Get limits of new data to plot
    y2_max <- max(df[[y]])
    y2_min <- min(df[[y]])

    # If no secondary y data has been plotted yet
    if (is.null(base$secondary_y_shift) &
        is.null(base$secondary_y_scale)) {
      if (is.finite(current_plotted_data_max)) {
        # If data has already been plotted on y1
        # scale and shift variables calculated based on desired mins and maxes
        scale = (y2_max - y2_min) / (current_plotted_data_max - current_plotted_data_min)
        shift = current_plotted_data_min - y2_min
        # Add variables to chart "metadata"
        base$secondary_y_shift <- shift
        base$secondary_y_scale <- scale
        # Get current y1 axis name


      } else {
        # Data hasn't already been plotted on y1
        # Just plot data as normal but on y2
        current_y_axis_name <- NULL
        scale <- 1
        shift <- 0
      }

    } else {
      # If secondary y data has already been plotted
      shift <- base$secondary_y_shift
      scale <- base$secondary_y_scale
    }

  } else {
    scale <- 1
    shift <- 0
  }


  if (no_shift == TRUE) {
    shift <- 0
    base$secondary_y_shift <- 0
  }

  # Apply the inv_scale_function to the values that will be plotted on the
  # scaled secondary y axis (if they've been supplied)
  if (!is.null(y)) {
    df[[y]] <- inv_scale_function(df[[y]], scale, shift)
  }

  if (!is.null(lower)) {
    df[[lower]] <- inv_scale_function(df[[lower]], scale, shift)
  }

  if (!is.null(upper)) {
    df[[upper]] <- inv_scale_function(df[[upper]], scale, shift)
  }

  if (!is.null(hline)) {
    hline <- inv_scale_function(hline, scale, shift)
  }


  # create base plot
  if (is.null(base)) {
    base <- ggplot2::ggplot() + ggplot2::theme_classic()
  }


  # add default theme if theme argument not provided
  if (!is.null(st_theme)) {
    base <- base + st_theme
  } else {
    # If not provided, use the default theme
    base <- base + ggplot2::theme_classic()
  }



  ##### Confidence intervals

  # Add conf intervals if arguments for ci and upper+lower bounds are provided
  # Apply before main point-plot so that points appear in front of bars / ribbons

  if(!is.null(ci)) {

    if(!is.null(lower) && !is.null(upper)) {

      # Add error bars or ribbon depending upon ci arguement

      # Plot for no group_var
      if (is.null(group_var)) {

        # Add error bars without grouping variable
        if(ci == 'errorbar') {

          base <-
            base + ggplot2::geom_errorbar(
              data = df,
              aes(
                x = .data[[x]],
                ymin = .data[[lower]],
                ymax = .data[[upper]]
              ),
              colour = error_colour,
              linewidth = 0.5
            )

        # Add ribbon without grouping variable
          # DOES NOT give additional legend
        } else if (ci == 'ribbon') {

          base <-
            base + ggplot2::geom_ribbon(
              data = df,
              aes(
                x = .data[[x]],
                ymin = .data[[lower]],
                ymax = .data[[upper]],
                group = 1
              ),
              fill = error_colour,
              #show.legend = ci_legend,
              alpha = .5
            ) #+
            #scale_fill_manual("test",values=error_colour)
        }

      # Plot for group_var provided
      } else if (!is.null(group_var)) {

        # Add error bars with grouping variable
        if(ci == 'errorbar') {

          base <-
            base + ggplot2::geom_errorbar(
              data = df,
              aes(
                x = .data[[x]],
                ymin = .data[[lower]],
                ymax = .data[[upper]],
                group = .data[[group_var]],
                colour =  .data[[group_var]]
              ),
              linewidth = .5
            )

        # Add ribbon with grouping variable
          # DOES give additional legend
        } else if (ci == 'ribbon') {

          # Account for geom_ribbon show.legend parameter accepting values of 'NA' or 'FALSE'
          show_ci_leg <- ifelse(ci_legend == TRUE, NA, FALSE)

          base <-
            base +
            ggplot2::geom_ribbon(
              data = df,
              aes(
                x = .data[[x]],
                ymin = .data[[lower]],
                ymax = .data[[upper]],
                group = .data[[group_var]],
                fill = .data[[group_var]]
              ),
              alpha = .5,
              show.legend = show_ci_leg
            )

        }
      }

      # Stop if upper and/or lower limit isn't provided
    } else {
      stop("Please provide arguements for 'upper' and 'lower' when ci is specified.")
    }

  }




  ##### Build base point plot

  # Build according to whether plotting variables are grouped or not
  if(is.null(group_var)) {

    # create base graph without groups
    base <-
      base + ggplot2::geom_point(
        data = df,
        aes(x = .data[[x]],
            y = .data[[y]]),
        color = point_colour,
        shape = point_shape
      )

  } else {

    # creating base graph with groups
    base <-
      base + ggplot2::geom_point(
        data = df,
        aes(
          x = .data[[x]],
          y = .data[[y]],
          group = factor(.data[[group_var]]),
          colour = factor(.data[[group_var]]),
          shape = factor(.data[[group_var]])
        )
      )



    ##### Legend parameters

    if (!is.null(legend_title)) {
      base <-  base + labs(name = legend_title,
                           colour = legend_title,
                           shape = legend_title)
    }

    if (!is.null(legend_pos)) {
      base <-  base + theme(legend.position = legend_pos)
    }

  }



  ##### Apply chart labels

  if (!is.null(labels)) {
    base <-
      base + geom_text(
        data = df,
        aes(
          x = .data[[x]],
          y = .data[[y]],
          label = round(.data$original_label, 0)
        ),
        hjust = labels_hjust,
        vjust = labels_vjust
      )
  }



  ##### Titles and axis labels

  # Add title
  if (!is.null(chart_title)) {
    base <- base + ggplot2::labs(title = chart_title) +
      # centre title
      theme(plot.title = element_text(hjust = 0.5))
  }

  # Apply x label using arguments provided
  if (!is.null(x_label)) {
    base <- base + ggplot2::labs(x = x_label)
  }

  # Apply y label using arguments provided
  if (!is.null(y_label)) {                      # bug in line_chart, change
    base <- base + ggplot2::labs(y = y_label)
  }

  # Rotate axis text if angle is given
  if (!is.null(x_label_angle)) {
    base <- base + ggplot2::theme(axis.text.x = element_text(angle  = x_label_angle, vjust = 0.5))
  }

  if (!is.null(y_label_angle)) {
    base <- base + ggplot2::theme(axis.text.y = element_text(angle  = y_label_angle, vjust = 0.5))
  }




  ##### Grid lines and axes

  # Remove or apply major and minor grid lines
  if (!show_gridlines) {
    base <- base + ggplot2::theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
    )
  } else {
    base <- base + ggplot2::theme(
      panel.grid.major = element_line(colour = "grey83", linewidth = 0.2),
      panel.grid.minor = element_line(colour = "grey93", linewidth = 0.1)
    )

  }


  # Remove or apply axis lines
  if (show_axislines) {
    base <- base + theme(
      axis.line.x = element_line(colour = "black", linewidth = 1),
      axis.line.y = element_line(colour = "black", linewidth = 1)
    )
  } else {
    base <- base + theme(
      axis.line.x = element_blank(),
      axis.line.y = element_blank()
    )
  }







  ##### Apply hlines

  # adds horizontal line at the y value specified for hline
  if (!is.null(hline)) {
    base <-
      base + geom_hline(yintercept = hline,
                        colour = hline_colour,
                        linewidth = hline_width,
                        linetype = hline_type)
  }

  # adds a label specified at the start of the horizontal line
  if (!is.null(hline) && !(is.null(hline_label))) {
    base <-
      base + geom_text(aes(
        x = min(df[[x]]),
        y = hline,
        label = hline_label,
        vjust = -1,  ##d adjust
        hjust = -0.1
      ))

  }


  # # Theme settings
  # base <-
  #   base + ggplot2::theme(
  #     #legend.title = element_blank(),
  #     #legend.position = legend_pos,
  #     #axis.text.x = element_text(angle  = x_label_angle, vjust = 0.5)
  #   )


  # Reverse the x axis scales for discrete variable (factors) if argument provided
  if (!(is.null(x_labels_reverse))) {
    if (is.factor(df[[x]])) {
      base  <-
        base + ggplot2::scale_x_discrete(limits = rev(levels(df[[x]])))

    }
  }



  if (!(is.null(y_min_limit))) {
    base  <-
      base + ggplot2::scale_y_continuous(limits = c(y_min_limit, y_max_limit))

  }



  # Set title styling
  base <-
    base + ggplot2::theme(
      text = element_text(size = 12, family = chart_font),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )



  # #remove major y grid lines
  # if (!(is.null(remove_gridlines))) {
  #   base <-
  #     base + ggplot2::theme(panel.grid.major.y = element_blank())
  #
  # }



  #append percentage labels
  if (!(is.null(y_percent))) {

    ##d change to scales functionality

    base <-
      base + ggplot2::scale_y_continuous(
        labels = function(x)
          paste0(x, "%")
      )

  }

  if (y_axis == "y2") {
    current_y_axis_name <-
      ggplot_build(base)$layout$panel_params[[1]]$y$name
    base <-
      base  + scale_y_continuous(name = current_y_axis_name,
                                 sec.axis = sec_axis( ~ scale_function(., scale, shift),
                                                      name = y_label))
  } else {
    base <- base + ggplot2::labs(y = y_label)
  }

  # # Apply x label using arguments provided
  # if (!(is.null(x_label))) {
  #   base <- base + ggplot2::labs(x = x_label)
  #
  # }

  if (!(is.null(chart_footer))) {
    base  <- base  + ggplot2::labs(caption = chart_footer)

  }

  return(base)}






