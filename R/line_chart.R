#' Line Chart Function
#'
#' Produce either a static (ggplot) or a dynamic (plotly) line chart output depending on the parameter \code{dynamic}.
#'
#' @param dynamic Logical indicating whether to produce a dynamic (plotly) plot. Default is \code{FALSE}.
#' @param base Optional base plot to customize. Default is NULL.
#' @param df Data frame containing the data.
#' @param x Variable for the x-axis.
#' @param y Variable for the y-axis.
#' @param ci Type of confidence interval. Can be 'e' (error bar) or 'r' (ribbon). Default is NULL.
#' @param lower Lower bound for confidence interval. Default is NULL.
#' @param upper Upper bound for confidence interval. Default is NULL.
#' @param error_colour Color for error bars or ribbons. Default is "#f2c75c".
#' @param group_var Variable for grouping data points. Default is NULL.
#' @param line_colour Vector of colors for lines. Default is "blue".
#' @param line_type Type of line. Options: "solid", "dotted", "dashed", "longdash", "dotdash". Default is "solid".
#' @param width Width of lines. Default is 1.
#' @param title Title of the plot. Default is NULL.
#' @param x_label Label for the x-axis. Default is NULL.
#' @param x_label_angle Angle for x-axis label rotation (counterclockwise). Default is NULL.
#' @param y_label Label for the y-axis. Default is NULL.
#' @param y_label_angle Angle for y-axis label rotation (counterclockwise). Default is NULL.
#' @param y_percent Logical indicating whether to display y-axis labels as percentages. Default is FALSE.
#' @param st_theme Optional theme for ggplot plots. Default is NULL.
#' @param add_points Logical indicating whether to add points to lines. Default is FALSE.
#' @param show_gridlines Logical indicating whether to show gridlines. Default is FALSE.
#' @param show_axislines Logical indicating whether to show axis lines. Default is TRUE.
#' @param legend_title Title for the legend. Default is NULL.
#' @param legend_position Position of the legend. Options: "left", "top", "right", "bottom", "none". Default is NULL.
#' @param hline Horizontal line at specified y-value. Default is NULL.
#' @param hline_colour Color of the horizontal line. Default is "red".
#' @param hline_label Label for the horizontal line. Default is NULL.
#'
#' @import dplyr
#' @import plotly
#' @import ggplot2
#' @import broom
#' @import RColorBrewer
#'
#' @return A static (ggplot) or dynamic (plotly) line chart.
#'
#'
#' @return
#' @export
#'
#' @examples
linechart <-  function(dynamic = FALSE,
                       base = NULL,
                       df,
                       x,
                       y,
                       ci = NULL,
                       lower = NULL,
                       upper = NULL,
                       error_colour = c("#f2c75c"),
                       group_var,
                       line_colour = c("blue"),
                       line_type = "solid",
                       width = 1,
                       title = NULL,
                       x_label = NULL,
                       x_label_angle = NULL, #rotation counterclockwise
                       y_label = NULL,
                       y_label_angle = NULL, #rotation counterclockwise
                       y_percent = FALSE,
                       st_theme = NULL,
                       add_points = FALSE,
                       show_gridlines = FALSE,
                       show_axislines = TRUE,
                       legend_title = NULL,
                       legend_position = NULL,
                       #  “left”,“top”, “right”, “bottom”, “none”.
                       hline = NULL,
                       hline_colour = "red",
                       hline_label = NULL) {
  valid_line_types <-
    c("solid", "dotted", "dashed", "longdash", "dotdash")

  # default RColorBrewer color pallette

  if(!missing(group_var)){
    # creating base graph with groups
    groupvar_levels <- nlevels(df[[group_var]])

    # check if correct number of colours provided for line_colour
    if (length(line_colour) != groupvar_levels) {
      if (groupvar_levels < 10) {
        # checking if max levels is less than
        # what the default palette offers)

        warning(
          paste0(
            "Incorrect number of colours provided for the line_color parameter.",
            groupvar_levels,
            " expected but only ",
            length(line_colour),
            " given. Using a default colour palette."
          ),
          call. = FALSE
        )

        print(paste0("debug: groupvar_levels:", groupvar_levels))

        line_colour = RColorBrewer::brewer.pal(groupvar_levels, name =
                                                 "Set1")

        print(line_colour) ##debug


      } else{
        stop(
          "The dataset has more levels than what the default colour palette supports.
               Please specify a list ",
          groupvar_levels,
          " line colours."
        )
      }

    }
  }

  # Check for any missing mandatory arguments
  missing_args <- c()
  if (missing(df)) {
    missing_args <- c(missing_args, "data frame")
  }
  if (missing(x)) {
    missing_args <- c(missing_args, "x-axis variable")
  }
  if (missing(y)) {
    missing_args <- c(missing_args, "y-axis variable")
  }

  if (length(missing_args) > 0) {
    stop(paste(
      "Please include argument(s) for the following:",
      paste(missing_args, collapse = ", ")
    ))
  }

  # check if dataset provided is not empty
  if (is.null(df) || nrow(df) == 0) {
    stop("Error: The provided data frame is empty.")
  }

  if (!dynamic) {
    # produce ggplot graph if 'dynamic' is set to FALSE


    # validate line type
    if (!(line_type %in% valid_line_types)) {
      stop(
        "Invalid line type. Please provide one of the following line types:
           solid, dotted, dashed, longdash, dotdash"
      )
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

    if (missing(group_var)) {
      # creating base graph without groups
      base <-
        base + ggplot2::geom_line(
          data = df,
          aes(x = .data[[x]], y = .data[[y]]),
          linetype = line_type,
          colour = line_colour,
          linewidth = width
        )
    } else{

      # creating base graph with groups
      base <-
        base + ggplot2::geom_line(
          data = df,
          aes(
            x = .data[[x]],
            y = .data[[y]],
            group = .data[[group_var]],
            colour = .data[[group_var]]
          ),
          linewidth = width,
          linetype = line_type
        ) +
        scale_colour_manual(values = line_colour)


      # Legend parameters

      if (!missing(legend_title)) {
        base <-  base + labs(color = legend_title)
      }

      if (!missing(legend_position)) {
        base <-  base + theme(legend.position = legend_position)
      }

    }


    ##### Titles and labels

    # Add title
    if (!missing(title)) {
      base <- base + ggplot2::labs(title = title) +
        # centre title
        theme(plot.title = element_text(hjust = 0.5))
    }

    # Apply x label using arguments provided
    if (!missing(x_label)) {
      base <- base + ggplot2::labs(x = x_label)
    }

    # Apply y label using arguments provided
    if (!missing(x_label)) {
      base <- base + ggplot2::labs(y = y_label)
    }

    # Rotate axis text if angle is given
    if (!missing(x_label_angle)) {
      base <- base + ggplot2::theme(axis.text.x = element_text(angle  = x_label_angle, vjust = 0.5))
    }

    if (!missing(y_label_angle)) {
      base <- base + ggplot2::theme(axis.text.y = element_text(angle  = y_label_angle, vjust = 0.5))
    }


    ####### Grid lines and axis

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

    # adds horizontal line at the y value specified for hline
    if (!missing(hline)) {
      base <-
        base + geom_hline(yintercept = hline,
                          colour = hline_colour,
                          linewidth = 1.5)
    }

    # adds a label specified at the start of the horizontal line
    if (!missing(hline) && !(missing(hline_label))) {
      base <-
        base + geom_text(aes(
          x = min(df[[x]]),
          y = hline,
          label = hline_label,
          vjust = -1,
          hjust = -0.1
        ))

    }

    # adds percent sign to the y axis values - doesnt change the value
    if (y_percent) {
      base <-
        base +  scale_y_continuous(labels = scales::percent_format(scale = 1))
    }


    ### Points in lines

    # if add points included then add geom
    if(add_points) {
      if(!missing(group_var)){
        base <-
          base + ggplot2::geom_point(data = df, aes(
            x = .data[[x]], y = .data[[y]], colour = .data[[group_var]], size=1
          )) +
          guides(size = 'none') # hides size of point from legend

      }else{

        base <-
          base + ggplot2::geom_point(data = df, aes(x = .data[[x]], y = .data[[y]], size=1
          )) +
          guides(size = FALSE) # hides size of point from legend
      }
    }


    #### confidence interval; ribbon \ error bar
    if (!(missing(ci)) && missing(group_var)) {
      # continue if  arguments for ci and bounds are provided
      ifelse(
        !(missing(lower)) && !(missing(upper)),

        # continue if type geom required is error else ribbon
        ifelse(
          ci == 'e',

          # Apply error bar with same legend and colour for line and ci
          base <-
            base + ggplot2::geom_errorbar(
              data = df,
              aes(
                x = .data[[x]],
                ymin = .data[[lower]],
                ymax = .data[[upper]]
              ),
              colour = error_colour,
              linewidth = 1
            )
          ,

          # Apply ribbon with same legend and colour for line and ci
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

          # Apply error bar with  same legend and colour for line and ci
          base <-
            base + ggplot2::geom_errorbar(
              data = df,
              aes(
                x = .data[[x]],
                ymin = .data[[lower]],
                ymax = .data[[upper]],
                group = .data[[group_var]],
                colour = .data[[group_var]]
              ),

            ) +
            ggplot2::scale_colour_manual(values = line_colour)
          ,

          # Apply ribbon with  same legend and colour for line and ci
          base <-
            base + ggplot2::geom_ribbon(
              data = df,
              aes(
                x = .data[[x]],
                ymin = .data[[lower]],
                ymax = .data[[upper]],
                group =
                  .data[[group_var]],
                fill = .data[[group_var]]
              ),
              alpha = .5
            ) +
            ggplot2::scale_fill_manual(values = line_colour)
        )

      )
    }




  } else{
    # produce plotly graph if 'dynamic' is set to TRUE

    # resolve line style
    if (!(line_type %in% valid_line_types)) {
      stop(
        "Invalid line type. Please provide one of the following line types:
           solid, dotted, dashed, longdash, dotdash"
      )
    } else{
      plotly_line_style <- switch(
        line_type,
        "solid" = "solid",
        "dotted" = "dot",
        "dashed" = "dash",
        "longdash" = "longdash",
        "dotdash" = "dashdot",
        "solid"
      )
    }

    # build list of line attributes

    # Create a list to store line attributes
    line_attributes <- list()

    # Add line modifiers
    if (!missing(width)) {
      line_attributes$width <- width * 2
    }
    if (!missing(line_type)) {
      line_attributes$dash <- plotly_line_style
    }
    if (!missing(line_colour)) {
      line_attributes$color <- line_colour
    }

    if (add_points==TRUE) {
      plt_mode = 'lines+markers'
    } else{
      plt_mode = 'lines'
    }


    if (missing(group_var)) {
      # create plotly base plot without groups
      base <- df |>
        plot_ly(
          x = ~ .data[[x]],
          y = ~ .data[[y]],
          type = 'scatter',
          mode = plt_mode,
          line = line_attributes
        ) |>
        layout(xaxis = list(title = x),
               yaxis = list(title = y))

    } else{
      # create plotly base plot groups

      unique_names <- unique(df[[group_var]])

      base <- df |>
        group_by({
          {
            group_var
          }
        })  |>
        plot_ly(
          x = ~ .data[[x]],
          y = ~ .data[[y]],
          type = "scatter",
          color = ~ .data[[{
            {
              group_var
            }
          }]],
          mode = plt_mode,
          colors = line_colour,
          line = line_attributes
        )


      base <- base |>
        layout(xaxis = list(title = x),
               yaxis = list(title = y))

    }


    ##### Titles and labels

    # add title
    if (!missing(title)) {
      base <- base |> layout(title = list(text=title))
    }

    # add x axis label
    if (!missing(x_label)) {
      base <- base |> layout(xaxis = list(title = x_label))
    }

    # add y axis label
    if (!missing(y_label)) {
      base <- base |> layout(yaxis = list(title = y_label))
    }

    # change x axis angle
    if (!missing(x_label_angle)){
      # angle negated as this function following ggplot rotation direction
      base <- base |>  layout(xaxis = list(tickangle = -x_label_angle))
    }

    # change y axis angle
    if (!missing(y_label_angle)){
      # angle negated as this function following ggplot rotation direction
      base <- base |>  layout(yaxis = list(tickangle = -y_label_angle))
    }


    ####### Grid lines and axis

    if (!show_gridlines) {
      base <- base |> layout(xaxis = list(showgrid = F),
                             yaxis = list(showgrid = F))
    }

    if (show_axislines) {
      base <- base |> layout(
        xaxis = list(showline = TRUE),
        yaxis = list(showline = TRUE,
                     zeroline = FALSE)
      )
    }

    if (!missing(hline)) {
      base <- base |>
        layout(shapes = list(
          type = "line",
          x0 = 0,
          x1 = 1,
          xref = "paper",
          y0 = hline,
          y1 = hline,
          line = list(color = hline_colour)
        ))
    }

    if (!missing(hline_label)) {
      base <- base %>% add_annotations(
        text = hline_label,
        x = min(df[[x]]),
        y = hline,
        showarrow = FALSE,
        bgcolor = "white",
        font = list(weight="bold")
      )
    }

    if (y_percent) {
      # Apply percentage formatting to y-axis labels (inline)
      base <- base |>  layout(yaxis = list(ticksuffix  = "%"))

    }

    ## Legend settings

    if (!missing(legend_position)) {

      legend_orientation <-
        if (legend_position %in% c("top", "bottom"))
          "h"
      else
        "v"

      # configure legend settings
      legend_settings <- switch(
        legend_position,
        "left" = list(
          x = -0.2,
          y = 0.5,
          xanchor = "right",
          yanchor = "middle",
          orientation = legend_orientation
        ),
        "top" = list(
          x = 0.5,
          y = 0.97,
          xanchor = "center",
          yanchor = "bottom",
          orientation = legend_orientation
        ),
        "right" = list(
          x = 1.1,
          y = 0.5,
          xanchor = "left",
          yanchor = "middle",
          orientation = legend_orientation
        ),
        "bottom" = list(
          x = 0.5,
          y = -0.2,
          xanchor = "center",
          yanchor = "top",
          orientation = legend_orientation
        )

      )  # Move legend outside of the plot area)

      if(legend_position!="none"){
        base <- base |> layout(legend = legend_settings)
      }else{
        base <- base |> layout(showlegend = F)
      }
    }

  }

  # return either ggplot or base plot
  return(base)
}
