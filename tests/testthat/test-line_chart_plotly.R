# test to chceck if the function returns a plotly object using the mandatory fields

test_that("line_chart_plotly works", {

  library(dplyr)
  library(epiviz)

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
    summarize(count = n(), .groups = 'drop') %>% ungroup()%>%
    filter(Specimen_Date >= start_date & Specimen_Date<= end_date)

  #This matches with all the mandatory variable needed to produce a basic
  #line graph
  result <- line_chart_plotly(df = summarised_df, x= "Specimen_Date", y = "count",
                              grouping_col = "Organism_Species_name")

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})

