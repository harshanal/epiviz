#' Process Line List for Age-Sex Pyramid
#'
#' This function processes a line list data frame, calculating the age from either the provided age column or date of birth.
#' It filters the data to include only males and females, groups the data into age groups, and summarizes the counts for each age group and sex.
#'
#' @param df A data frame containing the line list data.
#' @param var_map A list mapping variable names in the data frame to the expected names used in the function.
#' The list should contain elements:
#' \describe{
#'   \item{age}{Name of the column containing age values (default is `'age'`).}
#'   \item{date_of_birth}{Name of the column containing date of birth values (default is `'date_of_birth'`).}
#'   \item{sex}{Name of the column containing sex values (default is `'sex'`).
#'   Permitted values for sex include M, F, Male, Female (not case sensitive)}
#' }
#' @param age_breakpoints A numeric vector specifying the breakpoints for age groups. The default is `c(0, 5, 19, 65, Inf)`.
#' @param age_calc_refdate A Date object specifying the reference date for calculating age from date of birth.
#' The default is `Sys.Date()`.
#'
#' @return A data frame that is aggregated by age group and sex, with columns for age group, sex, value (count),
#' lower confidence limit, and upper confidence limit.
#'
#' @examples
#' \dontrun{
#'   df <- epiviz::lab_data
#'   processed_df <- process_line_list_for_age_sex_pyramid(df)
#' }
process_line_list_for_age_sex_pyramid <- function(df,
                                                  var_map = list(age = 'age',
                                                                 date_of_birth = 'date_of_birth',
                                                                 sex = 'sex'),
                                                  age_breakpoints = c(0, 5, 19, 65, Inf),
                                                  age_calc_refdate = Sys.Date()) {
  # Ensure df is a data frame
  if (!is.data.frame(df)) {
    stop("The input df must be a data frame.")
  }

  # Ensure that either 'age' or 'date_of_birth' is provided
  if (is.null(var_map$age) & is.null(var_map$date_of_birth)) {
    stop("Either 'age' or 'date_of_birth' must be provided in the var_map list.")
  }

  if (is.null(var_map$sex)) {
    stop("'sex' must be provided in the var_map list.")
  }


  # Handle age calculation if 'age' is not provided
  if (!is.null(var_map$age) && var_map$age %in% names(df)) {
    df$age <- df[[var_map$age]]
  } else if (!is.null(var_map$date_of_birth) &&
             var_map$date_of_birth %in% names(df)) {
    df$age <- round(as.numeric(difftime(age_calc_refdate, df[[var_map$date_of_birth]], units = "days")) / 365.25)
  } else {
    stop("The specified 'age' or 'date_of_birth' variable was not found in the data frame.")
  }

  # # Standardise the 'sex' variable
  # df <- df %>%
  #   mutate(!!var_map$sex := case_when(
  #     grepl("^(M|Male)$", .data[[var_map$sex]], ignore.case = TRUE) ~ "Male",
  #     grepl("^(F|Female)$", .data[[var_map$sex]], ignore.case = TRUE) ~ "Female",
  #     TRUE ~ NA_character_  # This will handle cases where the sex is not recognized
  #   ))

  # Filter out rows where 'sex' column is not "Male" or "Female"
  df <- df[df[[var_map$sex]] %in% c("Male", "Female"), ]

  # Convert numeric age breakpoints to corresponding age group labels
  age_labels <- c()
  for (i in 1:(length(age_breakpoints) - 2)) {
    age_labels[i] <- paste0(age_breakpoints[i], "-", age_breakpoints[i + 1] - 1)
  }
  # Create the last age group label based on the last numeric breakpoint
  age_labels[length(age_labels) + 1] <- paste0(age_breakpoints[length(age_breakpoints) - 1], "+")

  # Create age groups
  df$age_group <- cut(df$age,
                      breaks = age_breakpoints,
                      labels = age_labels,
                      right = FALSE) # right = FALSE ensures that age_group includes the lower bound and excludes the upper bound

  # Remove rows with NA in the age_group column
  df <- df %>% filter(!is.na(age_group))


  # Aggregate data by age group and sex
  df <- df %>%
    dplyr::group_by(age_group, sex = df[[var_map$sex]]) %>%
    dplyr::summarise(
      value = n(),
      lowercl = -1.96 * sqrt(value),
      uppercl =  1.96 * sqrt(value),
      .groups = "drop"
    )

  # Return the processed data frame
  return(df)
}
