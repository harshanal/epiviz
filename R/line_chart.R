#' Line_chart
#'
#' The chart function has 3 mandatory arguments to plot, with additional arguments as described further below:
#'
#' data frame
#' x axis variable name
#' y axis variable name
#'
#'
#' @param df name of data frame for plotting
#' @param base a ggplot instance or NULL if creating a new plot
#' @param x name of x axis variable in data frame
#' @param y name of y axis variable in data frame
#' @param group_var name of group variable to generate multiple lines (if required) in data frame
#' @param line_colour colour (or vector of colours if group_var != NULL) for the line in the chart
#' @param error_colour if ci != NULL - colour (or vector of colours if group_var != NULL) for the error bar or ribbon in the chart
#' @param ci "e" for error bars or "ribbon" for a ribbon - lower and upper become mandatory if ci != NULL
#' @param lower lower value for error \ ribbon geom (mandatory if ci argument passed)
#' @param upper upper value for error \ ribbon geom (mandatory if ci argument passed)
#' @param y_axis y axis to use - y1 or y2 (default is y1 - standard left-hand axis, y2 can be used for dual-axis graphs)
#' @param h_line will display a horizontal line if valid inter passed
#' @param line_type if provided will change all lines used to that type i.e. dotted, dashed, default sitting is solid, when not
#' provided where multiple lines generated each will be of different type
#' @param add_points will simply add points to all lines generated
#' @param line_size will alter line width accordingly for all lines generated, defaults to 1
#' @param x_label for provision of an x axis label
#' @param y_label for provision of an y axis label
#' @param x_label_angle to adjust the x axis label by the degrees of the integer provided
#' @param y_label_angle to adjust the y axis label by the degrees of the integer provided
#' @param x_labels_reverse enter an argument of any value i.e. 'y' to reverse the x labeling order when using categorical data
#' @param y_min_limit set the limit on the y axis scaling by proving an integer
#' @param y_max_limit set the limit on the x axis scaling by proving an integer
#' @param x_axis_breaks modify the x axis breaks by providing an integer
#' @param legend_pos modify the position of the legend (where applicable) with appropriate value i.e. bottom (default position),
#' top, right, left
#' @param remove_gridlines enter an argument of any value i.e. 'y' to remove the grid lines
#' @param percent enter an argument of any value i.e. 'y' to include the % symbol for y axis labels
#' @param cap_text enter text for a caption to appear below plot
#' @param no_shift If no shift should be applied to the secondary y-axis
#'
#' @import assertthat
#'
#' @return the final plot
#' @export
#'
#' @examples
#'
#' Examples are provided below for using this function that utilise the in-built DataVis package dummy data to produce
#' various visualisations, using different data frames and arguments.
#'
#'
#' Example 1: Displays line for each age group, for years across the x axis and count of cases along y axis,
#' adjust rotation of x axis labels, apply a horizontal line, reversed labels, dashed line type, larger line
#' width, set specific x axis breaks.
#' --------------------------------------------------------------------------------------------------------
#' line_chart(df = age_sex, x = "variable", y = "value", group_var = "Age.group",  h_line = 150, x_label_angle = 45,
#'            line_size = 3,  x_axis_breaks = 10,  x_labels_reverse = 'y', line_type = 'dashed')
#'
#'
#' Example 2: Shows line depicting rates with years across the x axis, y axis label provided, line type adjusted,
#' lower and upper values passed along with error bars selected, caption text provided.
#' --------------------------------------------------------------------------------------------------------
#' line_chart(df = rate, x = "year", y = "rate", x_label = 'year', y_label = 'rate', line_type = 'solid',
#'           lower = "lower.ci", upper = "upper.ci", ci = 'e', cap_text = 'Data source: xxx')
#'
#'
#' Example 3: Shows line depicting rates with years across the x axis, y axis label provided, lower and upper
#' values passed along with ribbon selected.
#' --------------------------------------------------------------------------------------------------------
#' line_chart(df = rate, x = "year", y = "rate", x_label = 'year', y_label = 'rate',
#'                    lower = "lower.ci", upper = "upper.ci", ci = 'w')
#'
#'
#' Example 4: Displays line for each local authority name, with years across the x axis and count of cases along y axis,
#' x and y labels provided, line type adjusted, percent symbol add to x axis labels and grid lines removed, reduce line size
#' and add points.
#' --------------------------------------------------------------------------------------------------------
#' line_chart(df = la, x = "year", y = "Counts", group_var = "laname", x_label = 'year', y_label = 'rate', line_type = 'solid',
#' percent = 'y', remove_gridlines = 'y', line_size = .5, add_points = 'y')
#'
#'
line_chart <- function(df,
                       base = NULL,
                       x = NULL,
                       y = NULL,
                       group_var = NULL,
                       line_colour = c("#003b49"),
                       error_colour = c("#f2c75c"),
                       ci = NULL,
                       lower = NULL,
                       upper = NULL,
                       y_axis = "y1",
                       h_line = NULL,
                       line_type = 'solid',
                       add_points = NULL,
                       line_size = 1,
                       x_label = NULL,
                       y_label = NULL,
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
                       no_shift = FALSE){




  #solves warnings regarding font family not found
  windowsFonts("Arial" = windowsFont("Arial"))



  # check for any missing mandatory arguments
  if (missing(df)) stop("A data frame argument is required")
  if (missing(x)) stop("Please include argument data frame variable for x axis, ie x = variable_name")
  if (missing(y)) stop("Please include argument data frame variable for y axis, ie y = variable_name")



  # Check that the data frame provided is not empty, else stop
  assertthat::assert_that(assertthat::not_empty(df))

  if (is.null(base)) {
    base <- ggplot2::ggplot()
  }


  # If user wants to plot on the secondary y-axis
  if (y_axis == "y2") {
    # Get limits of current plotted data (returns -Inf if no data currently plotted)
    current_plotted_data_max <- max(layer_scales(base)$y$range$range)
    current_plotted_data_min <- min(layer_scales(base)$y$range$range)
    # Get limits of new data to plot
    y2_max <- max(df[[y]])
    y2_min <- min(df[[y]])

    # If no secondary y data has been plotted yet
    if (is.null(base$secondary_y_shift) & is.null(base$secondary_y_scale)) {
      if (is.finite(current_plotted_data_max)) {
        # If data has already been plotted on y1
        # scale and shift variables calculated based on desired mins and maxes
        scale = (y2_max - y2_min)/(current_plotted_data_max - current_plotted_data_min)
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

  #### Apply the inv_scale_function to the values that will be plotted on the scaled secondary y axis (if they've been supplied) ####
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

  if (!(missing(group_var)) && !(missing(line_type))) {
    # If user has supplied a group_var and specified a linetype
    base <- base + ggplot2::geom_line(data = df, aes(x = .data[[x]], y = .data[[y]], group = .data[[group_var]], colour = .data[[group_var]]), linetype = line_type, linewidth = line_size) +
      scale_colour_manual(values = line_colour)
  } else if (!(missing(group_var)) && (missing(line_type))) {
    # If user has supplied a group_var but not specified a linetype
    base <- base + ggplot2::geom_line(data = df, aes(x = .data[[x]], y = .data[[y]], group = .data[[group_var]], colour = .data[[group_var]], linetype = .data[[group_var]]), linewidth = line_size) +
      scale_colour_manual(values = line_colour)
  } else if ((missing(group_var)) && !(missing(line_type))) {
    # If user has not supplied a group_var but specified a linetype
    base <- base + ggplot2::geom_line(data = df, aes(x = .data[[x]], y = .data[[y]], linetype = line_type), colour = line_colour, linewidth = line_size)

  } else if ((missing(group_var)) && (missing(line_type))) {
    # If user has not supplied a group_var nor specified a linetype
    base <- base + ggplot2::geom_line(data = df, aes(x = .data[[x]], y = .data[[y]]), linetype = line_type, colour = line_colour, linewidth = line_size)
  }

  # confidence interval; ribbon \ error bar
  if(!(missing(ci)) && missing(group_var)){

    # continue if  arguments for ci and bounds are provided
    ifelse(!(missing(lower)) && !(missing(upper)),

           # continue if type geom required is error else ribbon
           ifelse(ci == 'e',

                  # Apply error bar with same legend and colour for line and ci
                  base <- base + ggplot2::geom_errorbar(data = df, aes(x = .data[[x]], ymin = .data[[lower]], ymax = .data[[upper]]), colour = error_colour, linewidth = 1)
                  ,

                  # Apply ribbon with same legend and colour for line and ci
                  base <- base + ggplot2::geom_ribbon(data = df, aes(x = .data[[x]], ymin = .data[[lower]], ymax = .data[[upper]], group = 1), fill = error_colour, alpha = .5))

    )


  }

  if(!(missing(ci)) && !missing(group_var)){

    # continue if  arguments for ci and bounds are provided
    ifelse(!(missing(lower)) && !(missing(upper)),

           # continue if type geom required is error else ribbon
           ifelse(ci == 'e',

                  # Apply error bar with  same legend and colour for line and ci
                  base <- base + ggplot2::geom_errorbar(data = df, aes(x = .data[[x]], ymin = .data[[lower]], ymax = .data[[upper]],
                                                                       group = .data[[group_var]], colour = .data[[group_var]]), linewidth = 1) +
                    ggplot2::scale_colour_manual(values = error_colour)
                  ,

                  # Apply ribbon with  same legend and colour for line and ci
                  base <- base + ggplot2::geom_ribbon(data = df, aes(x = .data[[x]],
                                                                     ymin = .data[[lower]],
                                                                     ymax = .data[[upper]],
                                                                     group =.data[[group_var]],
                                                                     fill = .data[[group_var]]), alpha = .5) +
                    ggplot2::scale_fill_manual(values = error_colour))

    )}



  # if add points included then add geom
  if(!(missing(add_points))){

    ifelse(!(missing(group_var)),

           base <- base + ggplot2::geom_point(data = df, aes(x = .data[[x]], y = .data[[y]], colour=.data[[group_var]])),

           base <- base + ggplot2::geom_point(data = df, aes(x = .data[[x]], y = .data[[y]]))
    )
  }



  # apply horizontal line if h_line argument exists
  if(!(missing(h_line))){

    base <- base + ggplot2::geom_hline(yintercept = h_line, linetype = "dashed", colour = "black")

  }

  # Theme settings
  base <- base + ggplot2::theme(legend.title=element_blank(), legend.position = legend_pos,
                                axis.text.x = element_text(angle  = x_label_angle, vjust = 0.5))



  # Determine breaks for either discrete or contentious x variable using Scales package function
  #ifelse(is.numeric(df[[x]]),



  # Add x break to scale_x_continuous if argument provided
  #      ifelse(!(missing(x_axis_breaks)),




  #            base  <- base + ggplot2::scale_x_continuous(breaks = scales::breaks_extended()),
  #           base  <- base + ggplot2::scale_x_continuous(breaks = scales::breaks_extended(x_axis_breaks))

  #   ),

  # When factors used it will wrap if labels approx over 6 chars
  #  if(is.factor(df[[x]])){ base  <- base + ggplot2::scale_x_discrete(labels = scales::label_wrap(6)) }

  #)




  # Reverse the x axis scales for discrete variable (factors) if argument provided
  if(!(missing(x_labels_reverse))){


    if(is.factor(df[[x]])){


      base  <- base + ggplot2::scale_x_discrete(limits = rev(levels(df[[x]])))

    }
  }



  if(!(missing(y_min_limit))){


    base  <- base + ggplot2::scale_y_continuous(limits = c(y_min_limit, y_max_limit))

  }



  # Set styling
  base <- base + ggplot2::theme(text = element_text(size=12, family="Arial"),
                                axis.title.x = element_text(face="bold"),
                                axis.title.y = element_text(face="bold")
  )



  #remove major y grid lines
  if(!(missing(remove_gridlines))){

    base <- base + ggplot2::theme(panel.grid.major.y = element_blank())

  }



  #append percentage labels
  if(!(missing(percent))){

    base <- base + ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))

  }



  # ##remove legend
  # if(!(missing(remove_legend))){
  #   base <-   base + theme(legend.position="none")
  #   base <-   base + scale_colour_manual(guide="none")
  #   base <-   base + scale_fill_manual(guide="none")
  #   base <-   base +  guides(fill="none")
  # }

  if(y_axis == "y2") {
    current_y_axis_name <- ggplot_build(base)$layout$panel_params[[1]]$y$name
    base <- base  + scale_y_continuous(name = current_y_axis_name, sec.axis = sec_axis(~scale_function(., scale, shift), name=y_label))
  } else {
    base <- base + ggplot2::labs(y = y_label)
  }

  # Apply x label using arguments provided
  if(!(missing(x_label))){

    base <- base + ggplot2::labs(x = x_label)

  }


  if(!(missing(cap_text))){

    base  <- base  + ggplot2::labs(caption = cap_text)

  }

  return(base)
}
