#' Age-Sex Pyramid using Line List Data
#'
#' This function creates an age-sex pyramid using line list data.
#'
#' @param df A data frame containing line list data.
#' @param age_breakpoints A numeric vector specifying the age breakpoints for defining age groups. Default is c(0, 4, 18, 64, Inf).
#' @param colours A character vector specifying the colors for the genders in HEX format, in the order Female, Male. Default is c("#440154", "#fde725").
#' @param x_breaks Number of ticks on the X-axis. Default is 20.
#' @param y_title Title for the Y-axis. Default is "Proportion of People Percentage (%)".
#' @param text_size Text size for the plot. Default is 12.
#' @param conf_limits Logical, indicating whether to include confidence limits (error bars) in the plot. Default is FALSE.
#' @param age_calc_refdate Reference date for calculating age from date of birth if 'age' column is not available in the dataset.
#'                         Default is Sys.Date() (today's date).
#'
#' @return Returns an age-sex pyramid as a ggplot object.
#'
#' @import ggplot2
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#'   # Sample line list data
#'   data <- data.frame(
#'     date_of_birth = as.Date(c("1990-05-15", "1985-08-22", "1972-03-10", "2002-11-30")),
#'     sex = c("Male", "Female", "Male", "Male")
#'   )
#'
#'   # Generate age-sex pyramid
#'   agesex_pyramid_linelist(data)
#' }
#'
agesex_pyramid_linelist <- function(df,
                                    age_breakpoints = c(0, 5, 19, 65, Inf),
                                    colours = c("#440154", "#fde725"),
                                    x_breaks = 10,
                                    y_title = "Proportion of People Percentage (%)",
                                    text_size = 12,
                                    conf_limits = FALSE,
                                    age_calc_refdate = Sys.Date()) {



  # Check if 'age' column is available in the dataset
  if (!"age" %in% names(df)) {
    # Check if 'date_of_birth' is available
    if ("date_of_birth" %in% names(df)) {
      # Calculate age from 'date_of_birth' using provided or default reference date
      df$age <- round(as.numeric(difftime(Sys.Date(), df$date_of_birth, units = "days")) / 365.25)
    } else {
      stop("No 'age' or 'date_of_birth' column found in the dataset.")
    }
  }


  # Filter out rows where 'sex' column is not "Male" or "Female"
  df <- df |>
    filter(sex %in% c("Male", "Female"))

  # Convert numeric age breakpoints to corresponding age group labels
  age_labels <- c()
  for (i in 1:(length(age_breakpoints)-1)) {
    age_labels[i] <- paste0(age_breakpoints[i], "-", age_breakpoints[i+1]-1)
  }
  age_labels[length(age_labels)] <- paste0(age_breakpoints[length(age_breakpoints)-1], "+")

  # Create age groups
  df$age_group <- cut(df$age,
                      breaks = age_breakpoints,
                      labels = age_labels)



  # Aggregate data by age group and sex
  df <- df |>
    group_by(age_group, sex) |>
    summarise(value = n(),
              lowercl = if (conf_limits) -1.96 * sqrt(value),
              uppercl = if (conf_limits) 1.96 * sqrt(value)) |>
    na.omit()

  # Call agesex_pyramid_grouped function
  plot <- agesex_pyramid_grouped(df,
                                 colours = colours,
                                 x_breaks = x_breaks,
                                 y_title = y_title,
                                 text_size = text_size,
                                 conf_limits = conf_limits)

  return(plot)
}

