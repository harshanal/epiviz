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
point_chart_plotly <- function(
                        dynamic = FALSE,
                        base = NULL,
                        df,
                        x = NULL,
                        y = NULL,
                        ci = "e",
                        lower = NULL,
                        upper = NULL,
                        error_colour = "red",
                        group_var = NULL,
                        shape = "triangle", #
                        labels = NULL, #
                        labels_hjust = 0, #
                        labels_vjust = 0, #
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
                        ) {


  #solve warnings regarding font family not found
  if(get_os()[[1]] == "windows") {
    windowsFonts("Arial" = windowsFont("Arial"))
    chart_font <- "Arial"
  } else if(get_os()[[1]] == "osx") {
    chart_font <- "Arial"
  } else {
    # Arial not included with linux as standard, so default to sans
    chart_font <- "sans"
  }




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

  if (!is.null(hline)) {
    hline <- inv_scale_function(hline, scale, shift)
  }


  # create base plot
  if (is.null(base)) {
    base <- ggplot2::ggplot() + ggplot2::theme_classic()
  }


  # add default theme if theme argument not provided
  if (!missing(st_theme)) {
    base <- base + st_theme
  } else {
    # If not provided, use the default theme
    base <- base + ggplot2::theme_classic()
  }



  ### Build base plot
  #   according to whether plotting variables are grouped
  if(missing(group_var)) {

    # create base graph without groups
    base <-
      base + ggplot2::geom_point(
        data = df,
        aes(x = .data[[x]],
            y = .data[[y]]),
        color = "blue",
        shape = shape
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

    # Legend parameters

    if (!missing(legend_title)) {
      base <-  base + labs(name = legend_title,
                           colour = legend_title,
                           shape = legend_title)
    }

    if (!missing(legend_pos)) {
      base <-  base + theme(legend.position = legend_pos)
    }

  }



  ##### Titles and labels

  # Add title
  if (!missing(chart_title)) {
    base <- base + ggplot2::labs(title = chart_title) +
      # centre title
      theme(plot.title = element_text(hjust = 0.5))
  }

  # Apply x label using arguments provided
  if (!missing(x_label)) {
    base <- base + ggplot2::labs(x = x_label)
  }

  # Apply y label using arguments provided
  if (!missing(y_label)) {                      # bug in line_chart, change
    base <- base + ggplot2::labs(y = y_label)
  }

  # Rotate axis text if angle is given
  if (!missing(x_label_angle)) {
    base <- base + ggplot2::theme(axis.text.x = element_text(angle  = x_label_angle, vjust = 0.5))
  }

  if (!missing(y_label_angle)) {
    base <- base + ggplot2::theme(axis.text.y = element_text(angle  = y_label_angle, vjust = 0.5))
  }



  ##### Grid lines and axis

  #remove major and minor grid lines
  if (!show_gridlines) {
    base <- base + ggplot2::theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  }


  # shows axis lines
  if (show_axislines) {
    base <- base + theme(
      axis.line.x = element_line(colour = "black", linewidth = 1),
      axis.line.y = element_line(colour = "black", linewidth = 1)
    )
  }else{
    base <- base + theme(
      axis.line.x = element_blank(),
      axis.line.y = element_blank()
    )
  }



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
        hjust = labels_hjust,
        vjust = labels_vjust
      )
  }


  # adds horizontal line at the y value specified for hline
  if (!missing(hline)) {
    base <-
      base + geom_hline(yintercept = hline,
                        colour = hline_colour,
                        linewidth = hline_width,
                        linetype = hline_type)
  }

  # adds a label specified at the start of the horizontal line
  if (!missing(hline) && !(missing(hline_label))) {
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
      text = element_text(size = 12, family = chart_font),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )



  # #remove major y grid lines
  # if (!(missing(remove_gridlines))) {
  #   base <-
  #     base + ggplot2::theme(panel.grid.major.y = element_blank())
  #
  # }



  #append percentage labels
  if (!(missing(y_percent))) {

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
  # if (!(missing(x_label))) {
  #   base <- base + ggplot2::labs(x = x_label)
  #
  # }

  if (!(missing(chart_footer))) {
    base  <- base  + ggplot2::labs(caption = chart_footer)

  }

  return(base)}





# credit: https://www.r-bloggers.com/2015/06/identifying-the-os-from-r/
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

