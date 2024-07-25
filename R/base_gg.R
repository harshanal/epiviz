
#' Creates base ggplot object for use across other functions.
#' Parameters are not passed explicitely to the function, so
#' function call needs to be proceeded by environment(base_gg) <- environment()
#'
#' @return ggplot object
#'
#' \dontrun{
#' environment(base_gg) <- environment()
#' base <- base_gg()
#' }
base_gg <- function() {


  ##### Create base ggplot object

  # create base plot
  if (is.null(base)) {
    base <- ggplot()
  }

  # Add theme if theme argument provided
  if (!is.null(st_theme)) {
    base <- base + st_theme
  } else {
    # If not provided, use the default theme
    base <- base + ggplot2::theme_classic()
  }



  ##### Add chart titles, footers, and axis labels

  # Add title
  if (!is.null(chart_title)) {
    base <- base + ggplot2::labs(title = chart_title) +
      # Add title and axis base styling
      theme(plot.title = element_text(hjust = 0.5,
                                      size = chart_title_size,
                                      colour = chart_title_colour,
                                      family = chart_font,
                                      face = "bold"),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"))
  }

  # Add footer
  if (!(is.null(chart_footer))) {
    base  <- base  + ggplot2::labs(caption = chart_footer) +
      # Add title and axis base styling
      theme(plot.caption = element_text(size = chart_footer_size,
                                        colour = chart_footer_colour,
                                        family = chart_font))

  }

  # Apply x-axis label
  if (!is.null(x_label)) {
    base <- base + labs(x = x_label)
  } else {
    base <- base + labs(x = x) # default to variable name if label not provided
  }

  # Apply y-axis label
  if (!is.null(y_label) & (y_sec_axis == FALSE)) {
    base <- base + labs(y = y_label)
  } else {
    base <- base + labs(y = y) # default to variable name if label not provided
  }

  # Rotate axis text
  if (!is.null(x_label_angle)) {
    base <- base + theme(axis.text.x = element_text(angle  = x_label_angle, vjust = 0.5))
  }

  if (!is.null(y_label_angle)) {
    base <- base + theme(axis.text.y = element_text(angle  = y_label_angle, vjust = 0.5))
  }



  ##### Grid and axis lines

  ##dev separate options for major / minor, x / y ?

  # Remove or apply grid lines
  if (show_gridlines) {
    base <- base + ggplot2::theme(
      panel.grid.major = element_line(colour = "grey83", linewidth = 0.2),
      panel.grid.minor = element_line(colour = "grey93", linewidth = 0.1)
    )
  } else {
    base <- base + ggplot2::theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
    )

  }

  # Remove or apply axis lines
  if (show_axislines) {
    base <- base + theme(
      axis.line.x = element_line(colour = "black", linewidth = 0.5),
      axis.line.y = element_line(colour = "black", linewidth = 0.5)
    )
  } else {
    base <- base + theme(
      axis.line.x = element_blank(),
      axis.line.y = element_blank()
    )
  }



  ##### Define secondary axis

  # Applied separately below

  # If user wants to plot on the secondary y-axis
  if (y_sec_axis == TRUE) {

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
        # If data has already been plotted on y1;
        #   Scale and shift variables calculated based on desired mins and maxes
        scale = (y2_max - y2_min) / (current_plotted_data_max - current_plotted_data_min)
        shift = current_plotted_data_min - y2_min
        # Add variables to chart "metadata"
        base$secondary_y_shift <- shift
        base$secondary_y_scale <- scale
        # Get current y1 axis name

      } else {
        # If data hasn't already been plotted on y1;
        #   Just plot data as normal but on y2
        current_y_axis_name <- NULL
        scale <- 1
        shift <- 0
      }

      # If secondary y data has already been plotted
    } else {
      shift <- base$secondary_y_shift
      scale <- base$secondary_y_scale
    }

    # User does not want to plot on secondary axis
  } else {
    scale <- 1
    shift <- 0
  }

  # Force shift to 0 if y_sec_axis_no_shift == TRUE
  if (y_sec_axis_no_shift == TRUE) {
    shift <- 0
    base$secondary_y_shift <- 0
  }

  # Apply utils/inv_scale_function to the values that will be plotted on the
  # scaled secondary y axis (if they've been supplied)
  if (!is.null(y)) {
    df[[y]] <- inv_scale_function(df[[y]], scale, shift)
  }

  if (!is.null(ci_lower)) {
    df[[ci_lower]] <- inv_scale_function(df[[ci_lower]], scale, shift)
  }

  if (!is.null(ci_upper)) {
    df[[ci_upper]] <- inv_scale_function(df[[ci_upper]], scale, shift)
  }

  if (!is.null(hline)) {
    hline <- inv_scale_function(hline, scale, shift)
  }




  ##### Apply axis breaks if provided

  # Apply specified axis_break_labels

  if (!is.null(x_axis_break_labels)) {
    base <- base + scale_x_continuous(breaks = x_axis_break_labels)
  }

  if (!is.null(y_axis_break_labels)) {
    base <- base + scale_y_continuous(breaks = y_axis_break_labels)
  }


  # Apply specified axis_n_breaks

  if (!is.null(x_axis_n_breaks)) {
    # n.breaks will not work for dates, ignore arguement and issue warning
    if (is.Date(df[[x]])) {
      warning("x_axis_n_breaks will be ignored if x is a date variable, consider using
              x_axis_date_breaks or x_axis_break_labels instead")
    } else {
      base <- base + scale_x_continuous(n.breaks = x_axis_n_breaks)
    }
  }

  if (!is.null(y_axis_n_breaks)) {
    base <- base + scale_y_continuous(n.breaks = y_axis_n_breaks)
  }


  # Apply x-axis date intervals

  if (!is.null(x_axis_date_breaks)) {
    # Ignore arguement and issue warning if x is not a date variable
    if (!is.Date(df[[x]])) {
      warning("x is not a date variable, x_axis_date_breaks will be ignored")
    } else {
      base <- base + scale_x_date(date_breaks = x_axis_date_breaks)
    }
  }




  ##### Reverse axes

  # Note: Done at this stage as it alters x/y limits
      # https://github.com/tidyverse/ggplot2/issues/4014

  # x-axis
  if (x_labels_reverse == TRUE) {

    # Limits need to be inverted if axis is reversed

    # 1) Obtain limits from base plot if available
    if (!is.null((base$coordinates$limits$x)[2])) {x_limit_max <- (base$coordinates$limits$x)[2]}
    if (!is.null((base$coordinates$limits$x)[1])) {x_limit_min <- (base$coordinates$limits$x)[1]}

    # 2) Supercede with newly specified limits if provided
    if (!is.null(params$x_limit_max)) {x_limit_max <- params$x_limit_max}
    if (!is.null(params$x_limit_min)) {x_limit_min <- params$x_limit_min}

    # 3) Invert these limits
    x_limit_max_start <- x_limit_max  # Redefine starting vars here else the inversion below
    x_limit_min_start <- x_limit_min  #    will pick up the inverted var and duplicate

    x_limit_max <- x_limit_min_start
    x_limit_min <- x_limit_max_start

    # 4) Convert to dates if needed
    x_limit_max <- if (is.Date(df[[x]]) & !is.null(x_limit_max)) {as.Date(x_limit_max)} else {x_limit_max}
    x_limit_min <- if (is.Date(df[[x]]) & !is.null(x_limit_min)) {as.Date(x_limit_min)} else {x_limit_min}


    # Apply axis reversal
    if (is.Date(df[[x]])) {
      # Handle date axes
        if (!is.null(x_axis_break_labels)) { # take into account x_axis_break_labels if provided
          base <- base + scale_x_continuous(trans = c("date", "reverse"), breaks = x_axis_break_labels)
        } else {
          base <- base + scale_x_continuous(trans = c("date", "reverse"))
        }
    } else if (is.factor(df[[x]])) {
      # Handle discrete axes (i.e. factors)
      base <- base + scale_x_discrete(limits = rev(levels(df[[x]])))
    } else {
      base <- base + scale_x_reverse()
    }

  }




  ##### Apply axis limits

  # Build ylim and xlim vars for coord_cartesian

  # y lower limit only
  if (!is.null(y_limit_min) & is.null(y_limit_max)) {
    ylim <- c(y_limit_min, NA)
    # y upper limit only
  } else if (is.null(y_limit_min) & !is.null(y_limit_max)) {
    ylim <- c(NA, y_limit_max)
    # y upper and lower limits
  } else if (!is.null(y_limit_min) & !is.null(y_limit_max)) {
    ylim <- c(y_limit_min, y_limit_max)
  } else {
    ylim <- c(NA,NA)
  }

  # x lower limit only
  if (!is.null(x_limit_min) & is.null(x_limit_max)) {
    xlim <- c(x_limit_min, NA)
    # x upper limit only
  } else if (is.null(x_limit_min) & !is.null(x_limit_max)) {
    xlim <- c(NA, x_limit_max)
    # x upper and lower limits
  } else if (!is.null(x_limit_min) & !is.null(x_limit_max)) {
    xlim <- c(x_limit_min, x_limit_max)
  } else {
    xlim <- c(NA,NA)
  }

  # Convert any date axis limits to dates if necessary
  if (is.Date(df[[y]]) & !is.null(ylim)) {ylim <- as.Date(ylim)}
  if (is.Date(df[[x]]) & !is.null(xlim)) {xlim <- as.Date(xlim)}

  # Apply axis limits to base plot
  if (!is.null(ylim) & is.null(xlim)) {
    base <- base + coord_cartesian(ylim = ylim)
  } else if (is.null(ylim) & !is.null(xlim)) {
    base <- base + coord_cartesian(xlim = xlim)
  } else if (!is.null(ylim) & !is.null(xlim)) {
    base <- base + coord_cartesian(xlim = xlim, ylim = ylim)
  } else {
    base <- base
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

  # If specified, add label to the start of the horizontal line
  if (!is.null(hline) && !is.null(hline_label)) {

    # Define x-position of hline label
    # If lower x-axis limit present use that, else use min value of x
    if (!is.na(base$coordinates$limits$x[[1]])) {
      hline_xpos <- base$coordinates$limits$x[[1]]
    } else {
      hline_xpos <- min(df[[x]])
    }

    # Apply hline label to plot
    base <- base +
      geom_text(
        aes(
          x = hline_xpos,
          y = hline,
          label = hline_label,
          vjust = -0.5,
          hjust = 0
        ),
        colour = hline_label_colour)

  }





  ##### Change y-axis to percentage scale

  if (!(is.null(y_percent))) {

    base <-
      base + scale_y_continuous(
        ##labels = function(x) paste0(x, "%")
        labels = scales::label_percent()
      )

  }



  ##### Apply secondary axis

  if (y_sec_axis == TRUE) {
    # apply percentage scale if invoked
    if (is.null(y_percent)) {
      base <- base +
        # scale_y_continuous(sec.axis = sec_axis(~scale_function(., scale, shift),
        #                                        name = y_label))
        scale_y_continuous(sec.axis = sec_axis(~ . * scale + shift,
                                               name = y_label))
    } else {
      base <- base +
        # scale_y_continuous(sec.axis = sec_axis(~scale_function(., scale, shift),
        #                                        name = y_label,
        #                                        labels = scales::label_percent()))
        scale_y_continuous(sec.axis = sec_axis(~ . * scale + shift,
                                               name = y_label,
                                               labels = scales::label_percent()))
    }
  }




  ##### Return both df and base
  #       (df$y may have been modified through sec axis scaling)

  return_list <- list("base" = base, "df" = df)

  return(return_list)

  #return(base)

}

