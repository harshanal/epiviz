# test to check if the function returns a ggplot object

test_that("agesex_pyramid_linelist works", {

  library(epiviz)

  # check that the output is a ggplot object
  expect_true(inherits(agesex_pyramid_linelist(epiviz::lab_data), "ggplot"))
  expect_true(inherits(agesex_pyramid_linelist(epiviz::lab_data, x_breaks=10), "ggplot"))
  expect_true(inherits(agesex_pyramid_linelist(epiviz::lab_data, x_breaks=10, conf_limits = TRUE), "ggplot"))

})
