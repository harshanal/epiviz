# test to check if the function returns a ggplot object

test_that("age_sex_pyramid works", {

  library(epiviz)



  # check that the output is a ggplot object
  expect_true(inherits(
    age_sex_pyramid(dynamic=FALSE,
                    params = list(
                      df=epiviz::lab_data,
                      var_map = list(
                        date_of_birth = 'date_of_birth',
                        sex = 'sex'
                      ),
                      grouped = FALSE,
                      colours = c("pink", "blue"))
    )
    , "ggplot"))

  expect_true(inherits(
    age_sex_pyramid(dynamic=FALSE,
                    params = list(
                      df=epiviz::lab_data,
                      var_map = list(
                        date_of_birth = 'date_of_birth',
                        sex = 'sex'
                      ),
                      grouped = FALSE,
                      colours = c("pink", "blue"),
                      x_breaks = 5)
    )
    , "ggplot"))

  expect_true(inherits(
    age_sex_pyramid(dynamic=FALSE,
                    params = list(
                      df=epiviz::lab_data,
                      var_map = list(
                        date_of_birth = 'date_of_birth',
                        sex = 'sex'
                      ),
                      grouped = FALSE,
                      colours = c("pink", "blue"),
                      x_breaks = 5,
                      conf_limits = TRUE)
    )
    , "ggplot"))
  expect_true(inherits(agesex_pyramid_linelist(epiviz::lab_data, x_breaks=10), "ggplot"))
  expect_true(inherits(agesex_pyramid_linelist(epiviz::lab_data, x_breaks=10, conf_limits = TRUE), "ggplot"))



})


