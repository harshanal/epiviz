#' Column Chart
#'
#' @param df the dataframe containing the data to be plotted
#' @param base a ggplot instance or NULL if creating a new plot
#' @param x the x value column name to be plotted
#' @param y the y value column name to be plotted
#' @param group_var the variable used to group the bars i.e. region if plotting by region
#' @param fill the colour with which to fill the columns
#' @param y_axis either "y1" for the primary y-axis or "y2" for the secondary y-axis.
#' @param position the positions of the bars to be plotted i.e."dodge", "stack" etc
#' @param ci indicator for using ribbon or error bar geom (if required), enter 'e' for error bar, enter any other value for ribbon
#' @param lower lower value for error \ ribbon geom (mandatory if ci argument passed)
#' @param upper upper upper value for error \ ribbon geom (mandatory if ci argument passed)
#' @param error_colour if not plotting by group this is the colour of the error bars or ribbon
#' @param h_line will display a horizontal line if valid inter passed
#' @param y_label for provision of an y axis label
#' @param x_label for provision of an x axis label
#' @param x_label_angle to adjust the x axis label by the degrees of the integer provided
#' @param y_label_angle to adjust the y axis label by the degrees of the integer provided
#' @param x_labels_reverse enter an argument of any value i.e. 'y' to reverse the x labeling order when using categorical data
#' @param y_min_limit set the limit on the y axis scaling by proving an integer
#' @param y_max_limit set the limit on the x axis scaling by proving an integer
#' @param x_axis_breaks modify the x axis breaks by providing an integer
#' @param legend_pos modify the position of the legend (where applicable) with appropriate value i.e. bottom (default position), top, right, left
#' @param remove_gridlines enter an argument of any value i.e. 'y' to remove the grid lines
#' @param percent enter an argument of any value i.e. 'y' to include the % symbol for y axis labels
#' @param cap_text enter text for a caption to appear below plot
#' @param no_shift If no shift should be applied to the secondary y-axis
#'
#' @import assertthat
#'
#' @return a ggplot instance
#' @export
#'
#' @examples
#'
#'Example: Producing a chart showing Rates on the y-axis and year on the x-axis,
#' we want to show the rate for each local authority as a separate bar so supply the position = "dodge" argument
#' col_chart(df = year_la_rates_dummy, x = "year", y = "Rates", y_axis = "y1", group_var = "laname", y_label = "Rates")
#'
#'
col_chart <- function(df,
                      base = NULL,
                      x = NULL,
                      y = NULL,
                      group_var = NULL,
                      fill = "blue",
                      y_axis = "y1",
                      position = "dodge",
                      ci = NULL,
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
                      no_shift = FALSE
){

  # check for any missing mandatory arguments
  if (missing(df)) stop("A data frame argument is required")
  if (missing(x)) stop('Please inlcude argument data frame variable for x axis, ie x = "variable_name"')
  if (missing(y)) stop('Please include argument data frame variable for y axis, ie y = "variable_name"')



  # Check that the data frame provided is not empty, else stop
  assertthat::assert_that(not_empty(df))



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
        current_y_axis_name <- ggplot_build(base)$layout$panel_params[[1]]$y$name

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
      current_y_axis_name <- ggplot_build(base)$layout$panel_params[[1]]$y$name
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

  # if group variable not provided build plot accordingly
  if(missing(group_var)){

    base <- base + geom_col(data = df, aes(x = .data[[x]], y =  .data[[y]], fill = fill), position = position)

  } else if (!missing(group_var) && missing(fill)) {
    base <- base + geom_col(data = df, aes(x = .data[[x]], y =  .data[[y]], fill = .data[[group_var]]), position = position)
  } else if (!missing(group_var) && !missing(fill)) {
    base <- base + geom_col(data = df, aes(x = .data[[x]], y =  .data[[y]], group = .data[[group_var]], fill = .data[[group_var]]), position = position) #+scale_fill_manual(values = fill)
  }



  # confidence interval; ribbon \ error bar
  if(!(missing(ci)) && missing(group_var)){

    # continue if  arguments for ci and bounds are provided
    ifelse(!(missing(lower)) && !(missing(upper)),

           # continue if type geom required is error else ribbon
           ifelse(ci == 'e',

                  # Apply error bar with separate legends for line and ci
                  base <- base + ggplot2::geom_errorbar(data = df, aes(x = .data[[x]], ymin = .data[[lower]], ymax = .data[[upper]]), linewidth = 1, colour = error_colour)
                    ,

                  # Apply ribbon with separate legends for line and ci
                  base <- base + ggplot2::geom_ribbon(data = df, aes(x = .data[[x]], ymin = .data[[lower]], ymax = .data[[upper]], group = 1), fill = error_colour, alpha = .5))

           )


  }

  if(!(missing(ci)) && !missing(group_var)){

    # continue if  arguments for ci and bounds are provided
    ifelse(!(missing(lower)) && !(missing(upper)),

           # continue if type geom required is error else ribbon
           ifelse(ci == 'e',

                  # Apply error bar with separate legends for line and ci
                  base <- base + ggplot2::geom_errorbar(data = df, aes(x = .data[[x]], ymin = .data[[lower]], ymax = .data[[upper]],group = .data[[group_var]], colour = 'ci'), linewidth = 1)
                    ,

                  # Apply ribbon with separate legends for line and ci
                  base <- base +
                          ggplot2::geom_ribbon(data = df, aes(x = .data[[x]],
                                                              ymin = .data[[lower]],
                                                              ymax = .data[[upper]],
                                                              group =.data[[group_var]],  fill = "ci"), alpha = .5))

           )}



  # apply horizontal line if h_line argument exists
  if(!(missing(h_line))){

    base <- base + ggplot2::geom_hline(yintercept = h_line, linetype = "dashed", colour = "black")

  }

  # Theme settings
  base <- base + ggplot2::theme(legend.title=element_blank(), legend.position = legend_pos,
                                axis.text.x = element_text(angle  = x_label_angle, vjust = 0.5))


  # Set styling
  base <- base + ggplot2::theme(text = element_text(size=12, family="Arial"),
                                axis.title.x = element_text(face="bold"),
                                axis.title.y = element_text(face="bold")
  )

  #remove major y grid lines
  if(!(missing(remove_gridlines))){

    base <-   base + ggplot2::theme(panel.grid.major.y = element_blank())

  }



  #append percentage labels
  if(!(missing(percent))){

    base <- base + ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))

  }


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
