test_that("col_chart basic functionality works", {
  library(epiviz)

  # Create summarized test data from lab_data
  test_data <- epiviz::lab_data |>
    dplyr::group_by(organism_species_name) |>
    dplyr::summarise(count = dplyr::n())

  # Basic column chart
  expect_true(inherits(
    col_chart(
      df = test_data,
      x = "organism_species_name",
      y = "count"
    ), "ggplot"))

  # With custom fill color
  expect_true(inherits(
    col_chart(
      df = test_data,
      x = "organism_species_name",
      y = "count",
      fill = "red"
    ), "ggplot"))

  # With x-axis label angle
  expect_true(inherits(
    col_chart(
      df = test_data,
      x = "organism_species_name",
      y = "count",
      x_label_angle = 45
    ), "ggplot"))
})

test_that("col_chart grouping works", {
  library(epiviz)

  # Create grouped test data from lab_data
  test_data <- epiviz::lab_data |>
    dplyr::group_by(organism_species_name, region) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  # Test with grouping
  expect_true(inherits(
    col_chart(
      df = test_data,
      x = "organism_species_name",
      y = "count",
      group_var = "region",
      position = "dodge"
    ), "ggplot"))


})

test_that("col_chart error handling works", {
  # Test missing data frame
  expect_error(col_chart(x = "test", y = "count"))

  # Test missing x variable
  expect_error(col_chart(df = data.frame(count = 1:5), y = "count"))

  # Test missing y variable
  expect_error(col_chart(df = data.frame(x = 1:5), x = "x"))

  # Test empty data frame
  expect_error(col_chart(df = data.frame(), x = "x", y = "y"))
})

test_that("col_chart axis customization works", {
  library(epiviz)

  test_data <- epiviz::lab_data |>
    dplyr::group_by(organism_species_name) |>
    dplyr::summarise(count = dplyr::n())

  # Test with custom labels
  p <- col_chart(
    df = test_data,
    x = "organism_species_name",
    y = "count",
    x_label = "Species",
    y_label = "Count"
  )
  expect_true(inherits(p, "ggplot"))

  # Test with percentage formatting
  p <- col_chart(
    df = test_data,
    x = "organism_species_name",
    y = "count",
    percent = TRUE
  )
  expect_true(inherits(p, "ggplot"))
})
