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
#' enter 'e' for error bar, enter any other value for ribbon
#' @param lower lower value for error \ ribbon geom (mandatory if ci argument passed)
#' @param upper upper value for error \ ribbon geom (mandatory if ci argument passed)
#' @param error_colour if not plotting by group this is the colour of the error
#' bars or ribbon
#' @param h_line will display a horizontal line if valid inter passed
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
#' @param percent enter an argument of any value i.e. 'y' to include the %
#' symbol for y axis labels
#' @param cap_text enter text for a caption to appear below plot
#' @param labels labels to display alongside the plotted geom_points
#' @param hjust the horizontal adjustment of the point labels
#' @param vjust the vertical adjustment of the point labels
#' @param shape shape of the points to be plotted (only used when not plotting
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
point_chart <- function(df,
                        base = NULL,
                        x = NULL,
                        y = NULL,
                        group_var = NULL,
                        y_axis = "y1",
                        ci = "e",
                        lower = NULL,
                        upper = NULL,
                        error_colour = "red",
                        h_line = NULL,
                        y_label = NULL,
                        x_label = NULL,
                        x_label_angle = NULL,
                        y_label_angle = NULL,
                        x_labels_reverse = NULL,
                        y_min_limit = NULL,
                        y_max_limit= NULL,
                        x_axis_breaks = NULL,
                        legend_pos = "bottom",
                        remove_gridlines = NULL,
                        percent = NULL,
                        cap_text = NULL,
                        labels = NULL,
                        hjust = 0,
                        vjust = 0,
                        shape = "triangle",
                        no_shift = FALSE) {


  #solves warnings regarding font family not found
  windowsFonts("Arial" = windowsFont("Arial"))




  # check for any missing mandatory arguments
  if (missing(df))
    stop("A data frame argument is required")
  if (missing(x))
    stop("Please inlcude argument data frame variable for x axis, ie x = variable_name")
  if (missing(y))
    stop("Please include argument data frame variable for y axis, ie y = variable_name")



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

  if (!is.null(h_line)) {
    hline <- inv_scale_function(hline, scale, shift)
  }

  # if no group variable provided build plot accordingly
  ifelse(
    !missing(group_var),


    base <-
      base + ggplot2::geom_point(
        data = df,
        aes(
          x = .data[[x]],
          y = .data[[y]],
          group = .data[[group_var]],
          colour = .data[[group_var]],
          shape = .data[[group_var]]
        )
      ),

    base <-
      base + ggplot2::geom_point(
        data = df,
        aes(x = .data[[x]], y = .data[[y]]),
        color = "blue",
        shape = shape
      )

  )

  # confidence interval; ribbon \ error bar
  if (!(missing(ci)) && missing(group_var)) {
    # continue if  arguments for ci and bounds are provided
    ifelse(
      !(missing(lower)) && !(missing(upper)),

      # continue if type geom required is error else ribbon
      ifelse(
        ci == 'e',

        # Apply error bar with separate legends for line and ci
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

        ,

        # Apply ribbon with separate legends for line and ci
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
            alpha = .5
          )
      )

    )


  }

  if (!(missing(ci)) && !missing(group_var)) {
    # continue if  arguments for ci and bounds are provided
    ifelse(
      !(missing(lower)) && !(missing(upper)),

      # continue if type geom required is error else ribbon
      ifelse(
        ci == 'e',

        # Apply error bar with separate legends for line and ci
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
        ,

        # Apply ribbon with separate legends for line and ci
        base <-
          base +
          ggplot2::geom_ribbon(
            data = df,
            aes(
              x = .data[[x]],
              ymin = .data[[lower]],
              ymax = .data[[upper]],
              group = .data[[group_var]],
              fill = "ci"
            ),
            alpha = .5
          )
      )

    )
  }


  if (!missing(labels)) {
    base <-
      base + geom_text(
        data = df,
        aes(
          x = .data[[x]],
          y = .data[[y]],
          label = round(.data$original_label, 0)
        ),
        hjust = hjust,
        vjust = vjust
      )
  }


  # apply horizontal line if h_line argument exists
  if (!(missing(h_line))) {
    base <-
      base + ggplot2::geom_hline(yintercept = h_line,
                                 linetype = "dashed",
                                 colour = "black")

  }

  # Theme settings
  base <-
    base + ggplot2::theme(
      legend.title = element_blank(),
      legend.position = legend_pos,
      axis.text.x = element_text(angle  = x_label_angle, vjust = 0.5)
    )

  # Reverse the x axis scales for discrete variable (factors) if argument provided
  if (!(missing(x_labels_reverse))) {
    if (is.factor(df[[x]])) {
      base  <-
        base + ggplot2::scale_x_discrete(limits = rev(levels(df[[x]])))

    }
  }



  if (!(missing(y_min_limit))) {
    base  <-
      base + ggplot2::scale_y_continuous(limits = c(y_min_limit, y_max_limit))

  }



  # Set styling
  base <-
    base + ggplot2::theme(
      text = element_text(size = 12, family = "Arial"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )



  #remove major y grid lines
  if (!(missing(remove_gridlines))) {
    base <-
      base + ggplot2::theme(panel.grid.major.y = element_blank())

  }



  #append percentage labels
  if (!(missing(percent))) {
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

  # Apply x label using arguments provided
  if (!(missing(x_label))) {
    base <- base + ggplot2::labs(x = x_label)

  }

  if (!(missing(cap_text))) {
    base  <- base  + ggplot2::labs(caption = cap_text)

  }

  return(base)}
