#' Generate an Age-Sex Pyramid
#'
#' This function creates an age-sex pyramid visualization, either as a static ggplot or an interactive plotly chart.
#' The function can take either a line list (ungrouped data) or already grouped data as input.
#' When using a line list, the function processes the data, groups it by age and sex, and then generates the pyramid.
#' If grouped data is provided, it directly creates the pyramid.
#'
#' @param dynamic Logical. If `TRUE`, the function returns an interactive plotly chart. If `FALSE`, a static ggplot chart is returned.
#' @param base An optional base plot to add the pyramid to. Default is `NULL`.
#' @param params A list of parameters including:
#' \describe{
#'   \item{df}{Data frame containing the data to be used.}
#'   \item{var_map}{A list mapping variable names in the data frame to the expected names used in the function.
#'                  Expected elements include:
#'                  \itemize{
#'                    \item{age}{Name of the column containing age values. Default is `'age'`.}
#'                    \item{date_of_birth}{Name of the column containing date of birth values. Default is `'date_of_birth'`.}
#'                    \item{sex}{Name of the column containing sex values. Default is `'sex'`.}
#'                    \item{age_group}{Name of the column containing pre-grouped age groups (if `grouped = TRUE`).}
#'                    \item{value}{Name of the column containing the value counts (if `grouped = TRUE`).}
#'                    \item{lowercl}{Name of the column containing lower confidence limits (if `conf_limits = TRUE`).}
#'                    \item{uppercl}{Name of the column containing upper confidence limits (if `conf_limits = TRUE`).}
#'                  }}
#'   \item{colours}{A vector of colours to be used in the plot. Default is `c("#440154", "#fde725")`.}
#'   \item{x_breaks}{Number of breaks on the x-axis. Default is `10`.}
#'   \item{y_title}{Title of the y-axis. Default is `"Individual count"`.}
#'   \item{text_size}{Size of the text in the plot. Default is `12`.}
#'   \item{conf_limits}{Logical. If `TRUE`, confidence limits are displayed on the pyramid. Default is `FALSE`.}
#'   \item{age_breakpoints}{A numeric vector specifying the breakpoints for age groups. Default is `c(0, 5, 19, 65, Inf)`.}
#'   \item{age_calc_refdate}{Reference date for calculating age from date of birth. Default is `Sys.Date()`.}
#'   \item{grouped}{Logical. If `TRUE`, assumes the data is pre-grouped by age and sex. If `FALSE`, the function processes the line list data. Default is `FALSE`.}
#' }
#'
#' @return A ggplot or plotly object representing the age-sex pyramid, depending on the value of `dynamic`.
#'
#' @details
#' When `grouped = FALSE`, the function processes a line list by grouping the data by age and sex,
#' calculating age based on either the provided age column or date of birth, and then generating the age-sex pyramid.
#' When `grouped = TRUE`, it assumes the data is already grouped and uses the provided values directly to generate the pyramid.
#'
#' @examples
#' \dontrun{
#' # Example using a line list
#' df <- epiviz::lab_data
#' age_sex_pyramid(
#'   dynamic = FALSE,
#'   params = list(
#'     df = df,
#'     var_map = list(age = 'age', date_of_birth = 'date_of_birth', sex = 'sex'),
#'     grouped = FALSE
#'   )
#' )
#'
#' # Example using pre-grouped data
#' grouped_df <- data.frame(
#'   age_group = c("0-4", "5-18", "19-64", "65+"),
#'   sex = c("Male", "Female"),
#'   value = c(100, 120, 150, 80),
#'   lowercl = c(90, 110, 140, 70),
#'   uppercl = c(110, 130, 160, 90)
#' )
#' age_sex_pyramid(
#'   dynamic = FALSE,
#'   params = list(
#'     df = grouped_df,
#'     var_map = list(age_group = 'age_group', sex = 'sex', value = 'value'),
#'     grouped = TRUE
#'   )
#' )
#' }
#'
#' @import ggplot2
#' @importFrom dplyr %>% group_by summarise
#' @importFrom rlang .data
#' @export

age_sex_pyramid <- function(
    dynamic = FALSE,
    base = NULL,
    params = list(
      df,
      var_map = list(age = 'age',
                     date_of_birth = 'date_of_birth',
                     sex = 'sex',
                     age_group = 'age_group',
                     value = 'value',
                     lowercl = 'lowercl',
                     uppercl = 'uppercl'),
      colours = c("#440154", "#fde725"),
      x_breaks = 10,
      y_title = "Individual count",
      text_size = 12,
      conf_limits = FALSE,
      age_breakpoints = c(0, 5, 19, 65, Inf),
      age_calc_refdate = Sys.Date(),
      grouped = FALSE
    )
) {

  # Solve warnings regarding font family not found using utils/set_Arial() function
  set_Arial()

  # Where relevant, assign defaults to any parameters not specified by the user
  if(!exists('colours',where=params)) params$colours <- c("#440154", "#fde725")
  if(!exists('x_breaks',where=params)) params$x_breaks <- 10
  if(!exists('y_title',where=params)) params$y_title <- "Proportion of People Percentage (%)"
  if(!exists('text_size',where=params)) params$text_size <- 12
  if(!exists('conf_limits',where=params)) params$conf_limits <- FALSE
  if(!exists('age_breakpoints',where=params)) params$age_breakpoints <- c(0, 5, 19, 65, Inf)
  if(!exists('age_calc_refdate',where=params)) params$age_calc_refdate <- Sys.Date()
  if(!exists('grouped',where=params)) params$grouped <- FALSE


  ##### Checks and warnings

  # Check if df is is.null
  if (!exists('df',where=params)) stop("A data frame argument is required")

  # Check df is a df class
  if(!is.data.frame(params$df)) stop("df is not a data frame object")

  # Check df is empty
  if(!not_empty(params$df)) stop("df is empty")


  .grp_df <- NULL

  if(dynamic == FALSE){
    # ggplot implementation of static age-sex-pyramid

    # Correctly reference var_map from params
    var_map <- params$var_map

    if(params$grouped == FALSE){
      # Check if age or date_of_birth is provided and exists in the data frame
      if (!is.null(var_map$age) && var_map$age %in% names(params$df)) {
        .grp_df <- process_line_list_for_age_sex_pyramid(
          df = params$df,
          var_map = var_map,
          age_breakpoints = params$age_breakpoints,
          age_calc_refdate = params$age_calc_refdate
        )
      } else if (!is.null(var_map$date_of_birth) && var_map$date_of_birth %in% names(params$df)) {
        .grp_df <- process_line_list_for_age_sex_pyramid(
          df = params$df,
          var_map = var_map,
          age_breakpoints = params$age_breakpoints,
          age_calc_refdate = params$age_calc_refdate
        )
      }
    }else{
      # have been passed grouped data to function

    }

    # TODO: validate conf limits variables exists in df

    result <- agesex_pyramid_grouped(.grp_df,
                           colours = params$colours,
                           x_breaks = params$x_breaks,
                           y_title = params$y_title,
                           text_size = params$text_size,
                           conf_limits = params$conf_limits)


    return(result)
  }else{
    # plotly implementation of dynamic age-sex-pyramid
  }
}



