

test_that("col_chart produces a static chart", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    x_axis_label_angle = -45
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("col_chart produces a dynamic chart", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    x_axis_label_angle = -45
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("col_chart produces a static chart with grouped data", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    show_gridlines = FALSE
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with grouped data", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    show_gridlines = FALSE
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with grouped data and multiple hlines", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    hline = c(50,100),
    hline_colour = 'orange',
    hline_label = c('test1','test2'),
    hline_label_colour = 'orange'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with grouped data and multiple hlines", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    hline = c(50,100),
    hline_colour = 'orange',
    hline_label = c('test1','test2'),
    hline_label_colour = 'orange'
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with grouped data and case boxes", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-01-07")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    case_boxes = TRUE
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with grouped data and case boxes", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-01-07")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    case_boxes = TRUE
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with bar labels", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    x_axis_label_angle = -45,
    bar_labels = 'detections',
    bar_labels_pos = 'bar_base',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with bar labels", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    x_axis_label_angle = -45,
    bar_labels = 'detections',
    bar_labels_pos = 'bar_base',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})




test_that("col_chart produces a static chart with stacked bars and bar labels", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with stacked bars and bar labels", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with dodged bars and bar labels", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with dodged bars and bar labels", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})




# bar labels - dodged data
# stacked errorbars
# dodged errorbars
# axis_flip


#### FIX DODGED DATE LABELS
#### TEST FOR DATE AXES





