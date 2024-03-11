# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(epiviz)
library(dplyr)

# test to chceck if the function returns a plotly object using the mandatory fields

test_that("line_chart_plotly functionality and error handling ", {

  #import df lab_data from epiviz and do some manipulation before passing for the test
  #save the dataframe within the epiviz package as a new variable
  test_df <- epiviz::lab_data

  #manipulating date within df
  test_df$Specimen_Date <- as.Date(test_df$Specimen_Date)

  ## setting start date and end date for aggregation
  start_date <- as.Date("2023-01-01")
  end_date <- as.Date("2023-12-31")

  #summarisation
  summarised_df <- test_df  %>% group_by(Organism_Species_Name, Specimen_Date) %>%
    summarize(count = n()) %>% ungroup()%>%
    filter(Specimen_Date >= start_date & Specimen_Date<= end_date)

  #This matches with all the mandatory variable needed to produce a basic
  #line graph
  result <- line_chart_plotly(df = summarised_df, x= "Specimen_Date", y = "count",
                              grouping_col = "Organism_Species_name")

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))


 #Test for incorrect dataframe input
  expect_error(line_chart_plotly(df = na, x= "Specimen_Date", y = "count",
                                 grouping_col = "Organism_Species_name"),
               "Expect an error when df is incorrect")



  #Test for incorrect variable input
  expect_error(line_chart_plotly(df = lab_data, x= "date", y = "summ",
                                 grouping_col = "Organism_Species_name"),
               "Expect an error when x and y variables  is incorrect")


  #Test for using x variable as grouping col when it is absent
  expect_warning(line_chart_plotly(df = summarised_df, "Specimen_Date", y = "count"),
               "Expect a warning that grouping_col is missing")

})


test_that("line_chart_plotly functionality and error handling ", {

  #import df lab_data from epiviz and do some manipulation before passing for the test
  #save the dataframe within the epiviz package as a new variable
  test_df <- epiviz::lab_data

  #manipulating date within df
  test_df$Specimen_Date <- as.Date(test_df$Specimen_Date)

  ## setting start date and end date for aggregation
  start_date <- as.Date("2023-01-01")
  end_date <- as.Date("2023-12-31")

  #summarisation
  summarised_df <- test_df  %>% group_by(Organism_Species_Name, Specimen_Date) %>%
    summarize(count = n()) %>% ungroup()%>%
    filter(Specimen_Date >= start_date & Specimen_Date<= end_date)


  # Test different plot_mode
  result_mode <- line_chart_plotly(df = summarised_df, x= "Specimen_Date", y = "count",
                                   grouping_col = "Organism_Species_name",
                                   plot_mode = "lines+markers")

  expect_true(inherits(result_mode, "Plotly"), "The result should be a Plotly
              object with lines+markers mode.")

  # Test different theme change
  result_color <- line_chart_plotly(df = summarised_df, x= "Specimen_Date", y = "count",
                                   grouping_col = "Organism_Species_name",
                                   plot_mode = "lines", color_theme = c("lightgreen", "lightblue",
                                                                        "orange"))
  expect_true(inherits(result_color, "Plotly"), "The result should be a Plotly
              object with a custom color theme.")


  #Test error bars- assuming error bars are represented by some dummy columns lower_limit
  # and upper_limit

  grouped_df <- summarised_df |> group_by(Specimen_Date) |>
    summarise (mean_count = round(mean(count),2),
               sd_count = round(sd(count),2)) |> ungroup() |>
    mutate(upper_error = mean_count + sd_count) |>
    mutate(lower_error = mean_count - sd_count)


  result_error_bars <- line_chart_plotly(df = grouped_df, x= "Specimen_Date", y = "mean_count",
                                    grouping_col = "Organism_Species_name",
                                    lower_limit = "lower_error",
                                    upper_limit = "upper_error")
  expect_true(inherits(result_error_bars, "Plotly"), "The result should be a Plotly
              object with error bars.")

})


#Test for handling invalid inputs for optional parameters

test_that("line_chart_plotly handles invalid inputs for optional parameters",{
  #import df lab_data from epiviz and do some manipulation before passing for the test
  #save the dataframe within the epiviz package as a new variable
  test_df <- epiviz::lab_data

  #manipulating date within df
  test_df$Specimen_Date <- as.Date(test_df$Specimen_Date)

  ## setting start date and end date for aggregation
  start_date <- as.Date("2023-01-01")
  end_date <- as.Date("2023-12-31")

  #summarisation
  summarised_df <- test_df  %>% group_by(Organism_Species_Name, Specimen_Date) %>%
    summarize(count = n()) %>% ungroup()%>%
    filter(Specimen_Date >= start_date & Specimen_Date<= end_date)

  #Test invlaid color theme
  expect_error(line_chart_plotly(df = summarised_df, x= "Specimen_Date",
                                 y = "count", color_theme = "no color theme"),
               "Expect an error with invalid color_theme input")

  #Test invlaid plot_mode
  expect_error(line_chart_plotly(df = summarised_df, x= "Specimen_Date",
                                 y = "count", plot_mode =  "no plot mode"),
               "Expect an error with invalid plot_mode input")

})
