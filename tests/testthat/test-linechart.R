# test to chceck if the function returns a plotly object using the mandatory fields

test_that("linechart static works", {
  library(dplyr)
  library(epiviz)

  # Import df lab_data from epiviz and do some manipulation before passing for the test
  test_df <- epiviz::lab_data

  # Manipulating date within df
  test_df$specimen_date <- as.Date(test_df$specimen_date)

  # Setting start date and end date for aggregation
  start_date <- as.Date("2023-01-01")
  end_date <- as.Date("2023-12-31")

  # Summarization
  summarised_df <- test_df |>
    group_by(organism_species_name, specimen_date) |>
    summarize(count = n(), .groups = 'drop') |>
    ungroup() |>
    filter(specimen_date >= start_date & specimen_date <= end_date)

  # Ensure that summarised_df is a data frame
  summarised_df <- as.data.frame(summarised_df)

  # Create params list
  params <- list(
    dfr = summarised_df,  # Ensure this is correctly referencing the data frame
    x = "specimen_date", # Ensure this matches the column name exactly
    y = "count",         # Ensure this matches the column name exactly
    group_var = "organism_species_name",  # Ensure this matches the column name exactly
    line_colour = c("blue","green","orange")
  )

   # Generate the line chart
  result <- epiviz::linechart(params = params, dynamic = FALSE)


  # check that the output is a plotly object
  expect_true(inherits(result, "ggplot"))

})


# test to chceck if the function returns a plotly object using the mandatory fields

test_that("linechart dynamic works", {
  library(dplyr)
  library(epiviz)

  # Import df lab_data from epiviz and do some manipulation before passing for the test
  test_df <- epiviz::lab_data

  # Manipulating date within df
  test_df$specimen_date <- as.Date(test_df$specimen_date)

  # Setting start date and end date for aggregation
  start_date <- as.Date("2023-01-01")
  end_date <- as.Date("2023-12-31")

  # Summarization
  summarised_df <- test_df |>
    group_by(organism_species_name, specimen_date) |>
    summarize(count = n(), .groups = 'drop') |>
    ungroup() |>
    filter(specimen_date >= start_date & specimen_date <= end_date)

  # Ensure that summarised_df is a data frame
  summarised_df <- as.data.frame(summarised_df)

  # Create params list
  params <- list(
    dfr = summarised_df,  # Ensure this is correctly referencing the data frame
    x = "specimen_date", # Ensure this matches the column name exactly
    y = "count",         # Ensure this matches the column name exactly
    group_var = "organism_species_name",  # Ensure this matches the column name exactly
    line_colour = c("blue","green","orange")
  )

  # Generate the line chart
  result <- epiviz::linechart(params = params, dynamic = TRUE)


  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})

