#' epi_map
#'
#' The map function has 2 mandatory arguments to plot, df (data frame containing
#' your data) and value_col (value variable name),with additional arguments as
#' described further below:
#'
#' @param df name of data frame for plotting
#' @param value_col name of value variable in data frame
#' @param inc_shp boolean parameter to indicate whether df includes shapefile
#' data or if this will be uploaded separately.
#' @param shp_filepath filepath for the shapefile required for spatial data in
#' function. Note this will not be uploaded if inc_shp = TRUE.
#' @param shp_areacode name of areacode variable in shapefile
#' (Mandatory if shp_filepath argument passed).
#' @param data_areacode name of areacode variable in dataframe
#' (Mandatory if shp_filepath argument passed).
#' @param lookup vector of categories to include in the legend.
#' @param break_intervals numeric vector of interval points for the legend
#' (Mandatory if lookup argument passed).
#' @param force_cat boolean parameter to determine whether all arguments passed
#' in the lookup argument are used, even if there are no values present in the data.
#' @param n_breaks number of break intervals. This argument is an alternative
#' to supplying defined breaks in lookup argument and will provide a number of
#' evenly distributed breaks as specified. Note if lookup argument is passed
#' this will be ignored.
#' @param legend_title string containing the legend title.
#' @param legend_pos string containing legend position.
#' @param border_shape_filepath filepath for the shapefile if an outer border is
#'  required for the map. This should be a higher geography e.g.
#' if creating an UTLA map- a regional shapefile can be used.
#' @param border_code_col name of areacode variable in the border shapefile.
#' Required if a specific area within the border shapefile is required.
#' @param border_areaname string containing the name of area required for border
#' shapefile (required if border_code_col argument passed).
#'
#' @import classInt
#' @import RColorBrewer
#' @import grDevices
#' @importFrom stats setNames
#' @export
#'
#' @examples
#'
#' # Examples are provided below for using this function that utilise the in-built
#' # DataVis package dummy data to produce various visualisations, using different
#' # arguments.
#'
#' # Example 1: Displays a map for rates per 100,000 across UTLA geographies in
#' # London with a border for the outer London region boundaries.
#' # The data frame contains shapefile data which isn't imported separately.
#' # The legend contains specified legend categories with all levels
#' # forcefully included, with a legend title stating "Rate per 100,000" and
#' # positioned on the left of the map.
#' \dontrun{
#' leg_cats <- c("0.0-0.9 (pre-elimination)", "1.0-4.9 (low incidence)",
#'               "5.0-9.9 (low incidence)", "10.0-14.9", "15.0-19.9", "20-29.9",
#'               "30.0-39.9", ">=40.0")
#'
#' break_vals <- c( 0.0, 1.0, 5.0, 10.0, 15.0, 20.0, 30.0, 40)
#'
#'
#' epi_map(df = map,
#'              inc_shp= TRUE,
#'              value_col = "value",
#'              shp_filepath = NULL,
#'              shp_areacode = NULL,
#'              data_areacode = NULL,
#'              lookup = leg_cats,
#'              break_intervals = break_vals,
#'              legend_title = "Rate per 100,000",
#'              force_cat = TRUE,
#'              n_breaks = NULL,
#'              border_shape_filepath = "filepath_to_shapefile",
#'              border_code_col = "RGN21NM",
#'              border_areaname = "London",
#'              legend_pos = "left")
#' }
#' # Example 2: Displays a map for rates per 100,000 across UTLA geographies in
#' # London without a border. The data frame contains shapefile data which isn't
#' # imported separately.  The legend includes 5 breaks which are #' not
#' # pre-defined, does not include a legend title and is positioned to the right
#' # of the map.
#' \dontrun{
#' epi_map(df = map,
#'              inc_shp= TRUE,
#'              value_col = "value",
#'              shp_filepath = NULL,
#'              shp_areacode = NULL,
#'              data_areacode = NULL,
#'              lookup = NULL,
#'              break_intervals = NULL,
#'              legend_title = NULL,
#'              force_cat = FALSE,
#'              n_breaks = 5,
#'              border_shape_filepath = NULL,
#'              border_code_col = NULL,
#'              border_areaname = NULL,
#'              legend_pos = "right")
#' }
epi_map <- function(df,
                       value_col = NULL,
                       inc_shp = TRUE,
                       shp_filepath = NULL,
                       shp_areacode = NULL,
                       data_areacode = NULL,
                       lookup = NULL,
                       break_intervals = NULL,
                       force_cat = TRUE,
                       n_breaks = NULL,
                       legend_title = NULL,
                       legend_pos = "left",
                       border_shape_filepath = NULL,
                       border_code_col = NULL,
                       border_areaname = NULL){


  #solves warnings regarding font family not found
  windowsFonts("Arial" = windowsFont("Arial"))


  # check for any missing mandatory arguments
  if (missing(df))
    stop("A data frame argument is required")
  if (missing(value_col))
    stop("Please inlcude argument data frame variable for value, ie value = variable_name")

  # Check that lookup and break_intervals are of the same length if not using TB categories
  if (!is.null(lookup)) {
    if (length(lookup) != length(break_intervals)) {
      stop("The lookup and break_intervals have different length vectors.")

    }


  }

  # Read in data based on whether the df and shapefiles are already merged
  if (inc_shp == TRUE) {
    # Assign the value column to the dataframe
    df$Value <- df[, value_col]

    # Read in shapefile
    data_sf_merged <- sf::st_as_sf(df)



  } else {
    # Assign the value column to the dataframe
    df$Value <- df[, value_col]

    # Read in shapefile
    area_sf <- sf::st_read(shp_filepath)

    # Merge data and shapefile
    data_sf_merged <-
      merge(area_sf, df, by.x = shp_areacode, by.y = data_areacode)

  }


  # Set breaks and break labels depending on whether they are predefined
  if (is.null(lookup)) {
    # Set default of n_breaks to 5 if this is missing
    if (is.null(n_breaks)) {
      n_breaks <- 5

    }

    # get quantile breaks. Add .00001 offset to catch the lowest value
    breaks_qt <-
      classIntervals(c(min(unlist(
        data_sf_merged$Value
      )) - .00001, unlist(data_sf_merged$Value)), n = n_breaks
      , style = "quantile")
    #Re-format labels- remove ( and [  brackets and change , to -
    data_sf <- data_sf_merged %>%
      mutate(value_cat = cut(unlist(Value), breaks_qt$brks)) %>%
      mutate(value_cat = gsub("\\(|\\]", "", unlist(value_cat))) %>%
      mutate(value_cat = gsub("\\,", "-", unlist(value_cat)))
    #,
    #interval = as.factor(interval))

    # Force force_cat  to be  FALSE
    force_cat <- FALSE

  } else {
    # count number of interval groups
    interval_grp <- c(1:length(lookup))

    # Create lookup of break interbals and labels
    breaks_qt <-
      data.frame(interval = interval_grp , value_cat = lookup)

    # Assign values to intervals- then join labels
    data_sf <- data_sf_merged %>%
      mutate(interval = findInterval(unlist(Value), break_intervals)) %>%
      left_join(breaks_qt, by = "interval")

    # Create order based on which categories are included
    order <-
      data.frame(interval = data_sf$interval,
                 value_cat = data_sf$value_cat) %>%
      arrange(interval) %>%
      distinct()

    # Re-order factor to correct order
    data_sf <- data_sf %>%
      mutate(
        value_cat = factor(value_cat, order$value_cat),
        interval = as.factor(interval)
      )


  }

  if (force_cat == TRUE) {
    # Count number of intervals for palette
    n_pal <- as.numeric(length(lookup))

    # Designate RColorBrewer blues for map
    pal <- RColorBrewer::brewer.pal(n = n_pal, name = "Blues")

    # Add lookup names to colours for legend
    pal <- setNames(pal, lookup)

    # To use in drop category argument for map
    drop_cat <- FALSE




  } else {
    # Count number of intervals for palette
    n_pal <- as.numeric(length(unique(data_sf$value_cat)))

    # Designate RColorBrewer blues for map
    pal <- RColorBrewer::brewer.pal(n = n_pal, name = "Blues")

    # Dont have any names to pass in map- use categories already there
    pal <- setNames(pal, NULL)

    # To use in drop category argument for map
    drop_cat <- TRUE

  }

  # If legend title is missing set to blank
  if (is.null(legend_title)) {
    legend_title <- ""

  }

  # Create map
  base <- ggplot() +
    geom_sf(data = data_sf, aes(fill = value_cat)) +
    # Set colours
    scale_fill_manual(
      values = pal,
      limits = names(pal),
      # Drop or keep all category levels for legend based on selection
      drop = drop_cat ,
      # Add legend title
      name = legend_title
    )



  if (!is.null(border_shape_filepath)) {
    # Read in shapefile
    border_sf <- st_read(border_shape_filepath)

    # Filter for specific area for border in shapefile
    if (!is.null(border_code_col)) {
      # Check if e have area to filter to- if not print message
      if (is.null(border_areaname)) {
        print("border_areaname is missing so all shapefile data will be included")

      }

      # sf dataframes act different so pull index to filter selected column
      index <-
        as.numeric(grep(border_code_col, colnames(border_sf)))

      # Filter area
      border_sf <- border_sf %>%
        filter(.[[index]] == border_areaname)

    }

    # Add border overlay
    base <- base +
      geom_sf(
        data = border_sf,
        fill = NA,
        size = 0.2,
        colour = "black"
      )


  }

  # Add formatting
  base <- base +
    # # # Remove axis text
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      # Remove gridlines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      # Set legend text formattting
      legend.text = element_text(size = 10, family = "Arial"),
      legend.title = element_text(
        face = "bold",
        size = 12,
        family = "Arial"
      ),
      # Change legend position
      legend.position = legend_pos
    )

  return(base)
}
