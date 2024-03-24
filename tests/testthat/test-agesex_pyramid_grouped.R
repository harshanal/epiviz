# test to check if the function returns a ggplot object from age sex pyramid grouped

test_that("agesex_pyramid_grouped works", {
  library(epiviz)

  data <- data.frame(
    age_group = rep(c("0-4", "5-18", "19-64", "65+"), each = 2),
    sex = rep(c("Female", "Male"), times = 4),
    value = c(110, 90, 80, 70, 60, 50, 40, 30)
  )

  expect_true(inherits(agesex_pyramid_grouped(data, x_breaks = 10), "ggplot"))

  data2 <- data.frame(
    age_group = rep(c("0-4", "5-18", "19-64", "65+"), each = 2),
    sex = rep(c("Female", "Male"), times = 4),
    value = c(110, 90, 80, 70, 60, 50, 40, 30),
    lowercl = c(5, 5, 5, 5, 5, 5, 5, 5),
    uppercl = c(5, 5, 5, 5, 5, 5, 5, 5)
  )
  expect_true(inherits(
    agesex_pyramid_grouped(data2, x_breaks=10,conf_limits = TRUE),
    "ggplot"
  ))

})
