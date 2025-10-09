

test_that("epi_curve produces a static chart", {

  # Create params list
  params <- list(
    df = lab_data,
    date_var = "specimen_date",
    date_start = "2020-01-01",
    date_end = "2023-12-31",
    time_period = "year_month",
    fill_colours = "#007C91",
    rolling_average_line = TRUE,
    rolling_average_line_lookback = 3,
    rolling_average_line_legend_label = "3-month rolling average",
    chart_title = "Laboratory Detections per Month",
    x_axis_title = "Year - Month",
    y_axis_title = "Number of detections",
    x_axis_label_angle = -90
  )

  # Create static epi curve
  result <- epi_curve(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("epi_curve produces a dynamic chart", {

  # Create params list
  params <- list(
    df = lab_data,
    date_var = "specimen_date",
    date_start = "2020-01-01",
    date_end = "2023-12-31",
    time_period = "year_month",
    fill_colours = "#007C91",
    rolling_average_line = TRUE,
    rolling_average_line_lookback = 3,
    rolling_average_line_legend_label = "3-month rolling average",
    chart_title = "Laboratory Detections per Month",
    x_axis_title = "Year - Month",
    y_axis_title = "Number of detections",
    x_axis_label_angle = -90
  )

  # Create dynamic epi curve
  result <- epi_curve(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("epi_curve produces a static chart with grouped data", {

  # Define list of date breaks for x-axis; use every other ISO week in date range
  week_seq <- seq(as.Date("2021-01-01"),as.Date("2022-05-31"), by = '2 week')
  week_breaks <- paste0(lubridate::isoyear(week_seq),'-W',lubridate::isoweek(week_seq))

  # Create parameter list
  params <- list(
    df = lab_data,
    date_var = "specimen_date",
    date_start = "2021-01-01",
    date_end = "2022-05-31",
    time_period = "iso_year_week",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    rolling_average_line = TRUE,
    rolling_average_line_legend_label = "7-week rolling average",
    chart_title = "Laboratory detections by species \n 2021-01 - 2022-05",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Year - ISO Week",
    y_axis_title = "Number of detections",
    x_axis_label_angle = -90,
    x_axis_break_labels = week_breaks,
    y_axis_break_labels = seq(0, 250, 20),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static epi curve
  result <- epi_curve(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("epi_curve produces a dynamic chart with grouped data", {

  # Define list of date breaks for x-axis; use every other ISO week in date range
  week_seq <- seq(as.Date("2021-01-01"),as.Date("2022-05-31"), by = '2 week')
  week_breaks <- paste0(lubridate::isoyear(week_seq),'-W',lubridate::isoweek(week_seq))

  # Create parameter list
  params <- list(
    df = lab_data,
    date_var = "specimen_date",
    date_start = "2021-01-01",
    date_end = "2022-05-31",
    time_period = "iso_year_week",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    rolling_average_line = TRUE,
    rolling_average_line_legend_label = "7-week rolling average",
    chart_title = "Laboratory detections by species \n 2021-01 - 2022-05",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Year - ISO Week",
    y_axis_title = "Number of detections",
    x_axis_label_angle = -90,
    x_axis_break_labels = week_breaks,
    y_axis_break_labels = seq(0, 250, 20),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create dynamic epi curve
  result <- epi_curve(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("epi_curve produces a static chart with grouped data,
          chart lines, and case boxes", {

  # Create parameter list
  params <- list(
              df = lab_data,
              date_var = "specimen_date",
              date_start = "2021-06-01",
              date_end = "2021-07-31",
              time_period = "day",
              group_var = "organism_species_name",
              group_var_barmode = "stack",
              fill_colours = c("#007C91","#8A1B61","#FF7F32"),
              case_boxes = TRUE,
              rolling_average_line = TRUE,
              rolling_average_line_legend_label = "7-day rolling average",
              cumulative_sum_line = TRUE,
              chart_title = "Laboratory detections by species \n June - July 2021",
              chart_title_colour = "#007C91",
              hline = c(35),
              hline_label = "Threshold",
              hline_width = 0.5,
              hline_colour = "orange",
              hline_label_colour = "orange",
              hline_type = "dotdash",
              legend_title = "Detected organisms",
              legend_pos = "right",
              y_limit_max = 40,
              x_axis_break_labels = as.character(seq(as.Date("2021-06-01"),
                                                     as.Date("2021-07-31"),
                                                     by = '2 days')),
              y_axis_break_labels = seq(0, 40, 5),
              x_axis_title = "Date",
              y_axis_title = "Number of detections",
              x_axis_label_angle = -90,
              y_axis_label_angle = 90
            )

  # Create static epi curve
  result <- epi_curve(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("epi_curve produces a dynamic chart with grouped data,
          chart lines, and case boxes", {

  # Create parameter list
  params <- list(
              df = lab_data,
              date_var = "specimen_date",
              date_start = "2021-06-01",
              date_end = "2021-07-31",
              time_period = "day",
              group_var = "organism_species_name",
              group_var_barmode = "stack",
              fill_colours = c("#007C91","#8A1B61","#FF7F32"),
              case_boxes = TRUE,
              rolling_average_line = TRUE,
              rolling_average_line_legend_label = "7-day rolling average",
              cumulative_sum_line = TRUE,
              chart_title = "Laboratory detections by species \n June - July 2021",
              chart_title_colour = "#007C91",
              hline = c(35),
              hline_label = "Threshold",
              hline_width = 0.5,
              hline_colour = "orange",
              hline_label_colour = "orange",
              hline_type = "dotdash",
              legend_title = "Detected organisms",
              legend_pos = "right",
              y_limit_max = 40,
              x_axis_break_labels = as.character(seq(as.Date("2021-06-01"),
                                                     as.Date("2021-07-31"),
                                                     by = '2 days')),
              y_axis_break_labels = seq(0, 40, 5),
              x_axis_title = "Date",
              y_axis_title = "Number of detections",
              x_axis_label_angle = -90,
              y_axis_label_angle = 90
            )

  # Create du=ynamic epi curve
  result <- epi_curve(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})




test_that("epi_curve produces a static chart with pre-aggregated un-grouped data,
          with chart lines", {

            # Define a test dataframe containing the number of detections by date
            pre_agg_data <- lab_data |>
              group_by(specimen_date) |>
              summarise(detections = n()) |>
              ungroup()

            # Create parameter list
            params <- list(
              df = pre_agg_data,
              y = "detections",
              date_var = "specimen_date",
              date_start = "2021-10-01",
              date_end = "2022-03-31",
              time_period = "iso_year_week",
              rolling_average_line = TRUE,
              rolling_average_line_lookback = 3,
              rolling_average_line_legend_label = "3-week rolling average",
              rolling_average_line_colour = "#007C91",
              rolling_average_line_width = 1.5,
              cumulative_sum_line = TRUE,
              cumulative_sum_line_colour = "orange",
              chart_title = "Laboratory Detections by Region \nWinter 2021-22",
              chart_title_colour = "#007C91",
              #legend_title = "Region",
              legend_pos = "right",
              y_axis_break_labels = seq(0, 300, 50),
              x_axis_title = "ISO Week",
              y_axis_title = "Number of detections",
              x_axis_label_angle = -90,
              hover_labels = "<b>Week:</b> %{x}<br><b>Count:</b> %{y}"
            )

            # Create static epi curve
            result <- epi_curve(params = params, dynamic = FALSE)

            # check that the output is a ggplot object
            expect_true(inherits(result, "ggplot"))

})




test_that("epi_curve produces a dynamic chart with pre-aggregated un-grouped data,
          with chart lines", {

            # Define a test dataframe containing the number of detections by date
            pre_agg_data <- lab_data |>
              group_by(specimen_date) |>
              summarise(detections = n()) |>
              ungroup()

            # Create parameter list
            params <- list(
              df = pre_agg_data,
              y = "detections",
              date_var = "specimen_date",
              date_start = "2021-10-01",
              date_end = "2022-03-31",
              time_period = "iso_year_week",
              rolling_average_line = TRUE,
              rolling_average_line_lookback = 3,
              rolling_average_line_legend_label = "3-week rolling average",
              rolling_average_line_colour = "#007C91",
              rolling_average_line_width = 1.5,
              cumulative_sum_line = TRUE,
              cumulative_sum_line_colour = "orange",
              chart_title = "Laboratory Detections by Region \nWinter 2021-22",
              chart_title_colour = "#007C91",
              #legend_title = "Region",
              legend_pos = "right",
              y_axis_break_labels = seq(0, 300, 50),
              x_axis_title = "ISO Week",
              y_axis_title = "Number of detections",
              x_axis_label_angle = -90,
              hover_labels = "<b>Week:</b> %{x}<br><b>Count:</b> %{y}"
            )

            # Create static epi curve
            result <- epi_curve(params = params, dynamic = TRUE)

            # check that the output is a ggplot object
            expect_true(inherits(result, "plotly"))

})





test_that("epi_curve produces a static chart with pre-aggregated grouped data,
          with chart lines", {

            # Define a test dataframe containing the number of detections by region
            pre_agg_data <- lab_data |>
              group_by(specimen_date, region) |>
              summarise(detections = n()) |>
              ungroup()

            # Create parameter list
            params <- list(
              df = pre_agg_data,
              y = "detections",
              date_var = "specimen_date",
              date_start = "2021-10-01",
              date_end = "2022-03-31",
              time_period = "iso_year_week",
              group_var = "region",
              group_var_barmode = "stack",
              rolling_average_line = TRUE,
              rolling_average_line_lookback = 3,
              rolling_average_line_legend_label = "3-week rolling average",
              rolling_average_line_colour = "#007C91",
              rolling_average_line_width = 1.5,
              cumulative_sum_line = TRUE,
              cumulative_sum_line_colour = "orange",
              chart_title = "Laboratory Detections by Region \nWinter 2021-22",
              chart_title_colour = "#007C91",
              legend_title = "Region",
              legend_pos = "right",
              y_axis_break_labels = seq(0, 300, 50),
              x_axis_title = "ISO Week",
              y_axis_title = "Number of detections",
              x_axis_label_angle = -90,
              hover_labels = "<b>Week:</b> %{x}<br><b>Count:</b> %{y}"
            )

            # Create static epi curve
            result <- epi_curve(params = params, dynamic = FALSE)

            # check that the output is a ggplot object
            expect_true(inherits(result, "ggplot"))

})



test_that("epi_curve produces a dynamic chart with pre-aggregated grouped data,
          with chart lines", {

            # Define a test dataframe containing the number of detections by region
            pre_agg_data <- lab_data |>
              group_by(specimen_date, region) |>
              summarise(detections = n()) |>
              ungroup()

            # Create parameter list
            params <- list(
              df = pre_agg_data,
              y = "detections",
              date_var = "specimen_date",
              date_start = "2021-10-01",
              date_end = "2022-03-31",
              time_period = "iso_year_week",
              group_var = "region",
              group_var_barmode = "stack",
              rolling_average_line = TRUE,
              rolling_average_line_lookback = 3,
              rolling_average_line_legend_label = "3-week rolling average",
              rolling_average_line_colour = "#007C91",
              rolling_average_line_width = 1.5,
              cumulative_sum_line = TRUE,
              cumulative_sum_line_colour = "orange",
              chart_title = "Laboratory Detections by Region \nWinter 2021-22",
              chart_title_colour = "#007C91",
              legend_title = "Region",
              legend_pos = "right",
              y_axis_break_labels = seq(0, 300, 50),
              x_axis_title = "ISO Week",
              y_axis_title = "Number of detections",
              x_axis_label_angle = -90,
              hover_labels = "<b>Week:</b> %{x}<br><b>Count:</b> %{y}"
            )

            # Create static epi curve
            result <- epi_curve(params = params, dynamic = TRUE)

            # check that the output is a ggplot object
            expect_true(inherits(result, "plotly"))

})

