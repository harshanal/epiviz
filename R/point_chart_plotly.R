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
#' @param ci_lower ci_lower value for error \ ribbon geom (mandatory if ci argument passed)
#' @param ci_upper ci_upper value for error \ ribbon geom (mandatory if ci argument passed)
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
#' @import plotly
#' @import tidyr
#' @import yarrr
#' @rawNamespace import(ggplot2, except = last_plot)
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
#' point_chart(df = plot_df, x = "age", y = "value", ci = "e", ci_upper = "ci_uppercl",
#' ci_lower = "ci_lowercl", group_var = "ukborn", y_axis = "y2", y_label = "Value",
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
                          ci_lower = NULL,
                          ci_upper = NULL,
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
                          x_axis_break_labels = NULL,       ##add to line_chart?
                          y_axis_break_labels = NULL,
                          x_axis_n_breaks = NULL,
                          y_axis_n_breaks = NULL,
                          x_axis_date_breaks = NULL,
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

  # Allow axis_break_labels or axis_n_breaks
  if ((!is.null(params$x_axis_break_labels)) & (!is.null(params$x_axis_n_breaks)))
    stop("x_axis_break_labels cannot be provided with x_axis_n_breaks, please provide
         x_axis_break_labels OR x_axis_n_breaks")
  if ((!is.null(params$y_axis_break_labels)) & (!is.null(params$y_axis_n_breaks)))
    stop("y_axis_break_labels cannot be provided with y_axis_n_breaks, please provide
           y_axis_break_labels OR y_axis_n_breaks")

  # Allow x_axis_break_labels or x_axis_date_breaks
  if ((!is.null(params$x_axis_break_labels)) & (!is.null(params$x_axis_date_breaks)))
    stop("x_axis_break_labels cannot be provided with x_axis_date_breaks, please provide
           x_axis_break_labels OR x_axis_date_breaks")

  # Warn that x_axis_date_breaks cannot be used with a reversed x-axis
  if ((params$x_labels_reverse == TRUE) & (!is.null(params$x_axis_date_breaks)))
    warning("x_axis_date_breaks cannot be used with a reversed x-axis, consider using
              x_axis_break_labels instead")



  ### Parameter assignment

  # Define parameters as variables using utils/param_assign() function
  #   -Takes input list, compares it ro a reference vector of expected
  #     list elements, assigns each element to a variable within the
  #     parent environment, and allocates a value of 'NULL' to anything
  #     it can't find within the reference list.
  param_assign(params,
               c("df","x","y","ci","ci_legend","ci_legend_title","ci_lower",
                 "ci_upper","error_colours","errorbar_width","group_var","point_shape",
                 "point_colours","point_labels",
                 "point_labels_hjust","point_labels_vjust","point_labels_nudge_x",
                 "point_labels_nudge_y","y_sec_axis","y_sec_axis_no_shift","chart_title",
                 "chart_footer","x_label","x_label_angle","y_label","y_label_angle",
                 "y_percent","st_theme","x_labels_reverse","y_limit_min","y_limit_max",
                 "x_limit_min","x_limit_max", "x_axis_break_labels", "y_axis_break_labels",
                 "x_axis_n_breaks", "y_axis_n_breaks", "x_axis_date_breaks",
                 "show_gridlines","show_axislines", "legend_title","legend_pos",
                 "hline","hline_colour","hline_width", "hline_type","hline_label",
                 "hline_label_colour"))



  # # Define parameters from params list
  # for(i in 1:length(params)) {
  #   assign(names(params)[i], params[[i]])
  # }
  #
  # # Set any unused parameter values to NULL
  # unused <- setdiff(c("df","x","y","ci","ci_legend","ci_legend_title","ci_lower",
  #                     "ci_upper","error_colours","group_var","point_shape","point_colours","point_labels",
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




  #################### POINT CHART #################################

  if (!dynamic) {
    # produce ggplot object if 'dynamic' is set to FALSE


  # Define base ggplot object using R/base_gg() function
  #    -Force base_gg() to run in calling environment so it can find variables
  #     (lexical scoping will cause it to look for unfound variables in /R where
  #     it's stored rather than within the calling function's environment)
  environment(base_gg) <- environment()
  base_return <- base_gg()

  # base_gg() returns a list containing base and df; extract here
  base <- base_return$base
  df <- base_return$df



  ##### Apply confidence intervals

  # Apply before main point-plot so that points appear in front of bars / ribbons.

  # Add conf intervals if arguments for ci and ci_upper+ci_lower bounds are provided.
  if(!is.null(ci)) {

    if(!is.null(ci_lower) && !is.null(ci_upper)) {

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
                ymin = .data[[ci_lower]],
                ymax = .data[[ci_upper]]
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
                ymin = .data[[ci_lower]],
                ymax = .data[[ci_upper]],
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
                ymin = .data[[ci_lower]],
                ymax = .data[[ci_upper]],
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
                ymin = .data[[ci_lower]],
                ymax = .data[[ci_upper]],
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

      # Stop if ci_upper and/or ci_lower limit isn't provided
    } else {
      stop("Please provide arguements for 'ci_upper' and 'ci_lower' when ci is specified.")
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


  ##### Add default axis labels if not provided



  ##### Return final output
  return(base)

  ### GGPLOT END



  } else {

  ##### PLOTLY START

  # Produce plotly object if 'dynamic' is set to TRUE


    ##### BASE

    ##### Create base plotly object
    if (!is.null(base)) {
      base <- base
    } else {
      base <- plot_ly()
    }


    ##### Titles and axis labels

    # Define fonts
    title_font <- list(
      family = chart_font,
      size = 12,
      color = "black")

    axis_label_font <- list(
      family = chart_font,
      size = 11,
      color = "black")

    axis_break_font <- list(
      family = chart_font,
      size = 10,
      color = "black")


    # Add title
    if (!is.null(chart_title)) {
      base <- base |>
        layout(title = list(text = chart_title,
                            font = title_font))
    }

    #Note:- utils/html_bold function used to apply <b> </b> tags to axis titles for bold font

    # Add x axis label
    if (!is.null(x_label)) {
      base <- base |>
        layout(xaxis = list(title =
                              list(text = html_bold(x_label),
                                   font = axis_label_font)))
    } else {
      base <- base |>
        layout(xaxis = list(title =
                              list(text = html_bold(x),
                                   font = axis_label_font)))
    }

    # Add y axis label
    if (!is.null(y_label)) {
      base <- base |>
        layout(yaxis = list(title =
                              list(text = html_bold(y_label),
                                   font = axis_label_font)))
    } else {
      base <- base |>
        layout(yaxis = list(title =
                              list(text = html_bold(y),
                                   font = axis_label_font)))
    }


    # Set font for axis break text
    base <- base |> layout(font = axis_break_font)

    # Change x axis text angle
    if (!is.null(x_label_angle)){
      # angle negated as this function following ggplot rotation direction
      base <- base |> layout(xaxis = list(tickangle = -x_label_angle))
    }

    # Change y axis text angle
    if (!is.null(y_label_angle)){
      # angle negated as this function following ggplot rotation direction
      base <- base |> layout(yaxis = list(tickangle = -y_label_angle))
    }



    ####### Set grid lines, axes, and graph margin

    # Set margin to match ggplot
    base <- base |>
      layout(margin = list(r=10,t=30,b=30,l=3))

    # Grid lines
    if (!show_gridlines) {
      base <- base |> layout(xaxis = list(showgrid = F),
                             yaxis = list(showgrid = F))
    }

    # Axis lines
    if (show_axislines) {
      base <- base |> layout(
        xaxis = list(showline = TRUE,
                     linewidth = 1,
                     ticks="outside",
                     ticklen=3),
        yaxis = list(showline = TRUE,
                     zeroline = FALSE,
                     linewidth = 1,
                     ticks="outside",
                     ticklen=3)
      )
    }


    ##### Apply y percentage axis

    if (!is.null(y_percent)) {
      #base <- base |> layout(yaxis = list(ticksuffix  = "%"))
      base <- base |> layout(yaxis = list(tickformat  = ".0%"))
    }



    ##### Apply hline

    # Draw line
    if (!is.null(hline)) {
      base <- base |>
        layout(shapes = list(
          type = "line",
          x0 = 0,
          x1 = 1,
          xref = "paper",
          y0 = hline,
          y1 = hline,
          line = list(color = hline_colour,
                      dash = "dash",          ##d change to hline_type
                      width = hline_width)
        ))
    }

    # Add label
    if (!is.null(hline_label)) {
      base <- base |>
        add_annotations(
          text = hline_label,
          x = min(df[[x]]),
          y = hline,
          xanchor = "left",
          yanchor = "bottom",
          showarrow = FALSE,
          bgcolor = "#ffffff00",
          font = list(color = hline_colour,
                      size = 12)
        )
      # add_text(showlegend = FALSE,
      #          x = min(df[[x]]),
      #          y = hline,
      #          text = hline_label,
      #          textposition = 'top right',
      #          textfont = list(color = hline_colour,
      #                          size = 12)
      #          )
    }


    ##### POINT


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

          # Add error bars as trace with invisible markers
          base <- base |>
            add_trace(
              data = df,
              x = ~ df[[x]],
              y = ~ df[[y]],
              type = 'scatter',
              mode = 'markers',
              showlegend = F,
              marker = list(
                color = '#ffffff00'
              ),
              error_y = list(
                type = "data",
                symmetric = FALSE,
                color = error_colours,
                thickness = 1,
                arrayminus = ~ diff_ci_lower,
                array = ~ diff_ci_upper
              )
            )

          # Add ribbon without grouping variable
        } else if (ci == 'ribbon') {

          base <- base |>
            add_ribbons(
              x = ~ df[[x]],
              ymin = ~ df[[ci_lower]],
              ymax = ~ df[[ci_upper]],
              #name = ???,
              #fillcolor = error_colours,
              fillcolor = yarrr::transparent(error_colours, trans.val = .5), # add transparency using 'yarrr' package
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
                name = unique_groups[[i]],
                showlegend = F,
                marker = list(
                  color = '#ffffff00'
                ),
                error_y = list(
                  type = "data",
                  symmetric = FALSE,
                  color = error_colours[[i]],
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
              select(x, ci_lower) |>
              dplyr::rename("y_val" = 2)

            df_group_up <- df |>
              filter(get(group_var) == unique_groups[i]) |>
              select(x, ci_upper) |>
              dplyr::rename("y_val" = 2)

            df_group_ribb <- rbind(df_group_low, (df_group_up %>% arrange(desc(row_number()))))


            # Add separate ribbon for each group using df defined above
            base <- base |>
              add_trace(
                data = df_group_ribb,
                x = ~ df_group_ribb[[x]],
                y = ~ y_val,
                type = 'scatter',
                mode = 'lines',
                name = unique_groups[[i]],
                line = list(color = 'transparent'),
                fill = 'toself',
                #fillcolor = paste0(error_colours[[i]],'80')
                fillcolor = yarrr::transparent(error_colours[[i]], trans.val = .5) # add transparency using 'yarrr' package
                # showlegend = FALSE,
                # name = 'legend name'
              )

          }

        }

      }

    }



    ##### Resolve point style

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



    ##### Create point chart

    # Build according to whether plotting variables are grouped or not
    if (is.null(group_var)) {

      # Add plotly trace without groups
      base <- base |>
        add_trace(
          df,
          x = ~ df[[x]],
          y = ~ df[[y]],
          type = 'scatter',
          mode = 'markers',
          marker = list(
            color = point_colours,
            symbol = point_shapes_key[[point_shape]]
            )
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

        base <- base |>
          add_trace(
            data = df_group,
            x = df_group[[x]],
            y = df_group[[y]],
            type = 'scatter',
            mode = 'markers',
            name = unique_groups[[i]],
            marker = list(
              color = point_colours[[i]],
              symbol = plotly_point_shapes[[i]]
            )
          )

      }

    }








# legend
# limits
# breaks
# axis reversal
# hover labels


    #
    # ## Legend settings
    #
    # if (!missing(legend_position)) {
    #
    #   legend_orientation <-
    #     if (legend_position %in% c("top", "bottom"))
    #       "h"
    #   else
    #     "v"
    #
    #   # configure legend settings
    #   legend_settings <- switch(
    #     legend_position,
    #     "left" = list(
    #       x = -0.2,
    #       y = 0.5,
    #       xanchor = "right",
    #       yanchor = "middle",
    #       orientation = legend_orientation
    #     ),
    #     "top" = list(
    #       x = 0.5,
    #       y = 0.97,
    #       xanchor = "center",
    #       yanchor = "bottom",
    #       orientation = legend_orientation
    #     ),
    #     "right" = list(
    #       x = 1.1,
    #       y = 0.5,
    #       xanchor = "left",
    #       yanchor = "middle",
    #       orientation = legend_orientation
    #     ),
    #     "bottom" = list(
    #       x = 0.5,
    #       y = -0.2,
    #       xanchor = "center",
    #       yanchor = "top",
    #       orientation = legend_orientation
    #     )
    #
    #   )  # Move legend outside of the plot area)
    #
    #   if(legend_position!="none"){
    #     base <- base |> layout(legend = legend_settings)
    #   }else{
    #     base <- base |> layout(showlegend = F)
    #   }
    # }

  # return base plot
  return(base)

    } ### PLOTLY END


}






