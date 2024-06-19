test_that("point_chart works", {
  # Create sample data
  plot_df <- data.frame(
    age = c(20, 25, 30, 35),
    value = c(10, 15, 20, 25),
    original_label = c(1, 2, 3, 4)  # Use numeric values for original_label
  )

  # This example will plot the points with labels showing the plotted values
  # that are vertically adjusted from the plotted points
  # check that the output is a plotly object
  expect_true(inherits(
    point_chart(
      df = plot_df,
      x = "age",
      y = "value",
      labels = "original_label",
      vjust = -0.5
    ),
    "ggplot"
  ))
})
