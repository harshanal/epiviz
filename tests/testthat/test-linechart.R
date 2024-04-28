# test if the function returns a gpglot object using the mandatory fields

test_that("line chart fn ggplot output works", {

  library(dplyr)
  library(epiviz)


  data <- epiviz::lab_data
  data_processed <- data %>%
    group_by(specimen_date, organism_species_name) %>%
    summarize(count = n()) %>%
    ungroup()

  data_processed_monthly <- data_processed %>%
    filter(lubridate::year(specimen_date) == 2023) |>
    mutate(month = format(specimen_date, "%Y-%m")) %>%
    group_by(month, organism_species_name) %>%
    summarise(total_count = sum(count)) |>
    ungroup()


  result <- linechart(
              dynamic = FALSE,
              df = data_processed_monthly,
              x = "month",
              y = "total_count",
              group_var = "organism_species_name",
              line_colour = c("#ff5733", "#7b68ee", "#3cb371"),
              width = 1,
              st_theme = theme_minimal(),
              title = "Organism trends (2023)",
              x_label = "Time",
              y_label = "Organism count",
              y_percent = FALSE,
              show_gridlines = FALSE,
              show_axislines = TRUE,
              hline = 150,
              hline_colour = "purple",
              hline_label = "Threshold=150",
              add_points = TRUE,
              x_label_angle = 45,
              y_label_angle = 0,
              legend_title = "Organism",
              legend_position = "bottom"
            )

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})


# test if the function returns a gpglot object using the mandatory fields

test_that("line chart fn ggplot output works", {

  library(dplyr)
  library(epiviz)


  data <- epiviz::lab_data
  data_processed <- data %>%
    group_by(specimen_date, organism_species_name) %>%
    summarize(count = n()) %>%
    ungroup()

  data_processed_monthly <- data_processed %>%
    filter(lubridate::year(specimen_date) == 2023) |>
    mutate(month = format(specimen_date, "%Y-%m")) %>%
    group_by(month, organism_species_name) %>%
    summarise(total_count = sum(count)) |>
    ungroup()


  result <- linechart(
    dynamic = TRUE,
    df = data_processed_monthly,
    x = "month",
    y = "total_count",
    group_var = "organism_species_name",
    line_colour = c("#ff5733", "#7b68ee", "#3cb371"),
    width = 1,
    st_theme = theme_minimal(),
    title = "Organism trends (2023)",
    x_label = "Time",
    y_label = "Organism count",
    y_percent = FALSE,
    show_gridlines = FALSE,
    show_axislines = TRUE,
    hline = 150,
    hline_colour = "purple",
    hline_label = "Threshold=150",
    add_points = TRUE,
    x_label_angle = 45,
    y_label_angle = 0,
    legend_title = "Organism",
    legend_position = "bottom"
  )

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})
