#' epi_map_leaflet
#'
#' @description A function to produce interactive leaflet rate maps
#'
#' @param df data frame used for plotting including SF geometry
#' @param value_col the name of the variable in the data frame to split the chart by and fill in the areas on the map
#' @param data_areacode name of areacode variable in dataframe
#' (Mandatory if shp_filepath argument passed).
#' @param inc_shp boolean parameter to indicate whether df includes shapefile
#' data or if this will be uploaded separately.
#' @param shp_filepath filepath for the shapefile required for spatial data in
#' function. Note this will not be uploaded if inc_shp = TRUE.
#' @param shp_areacode name of areacode variable in shapefile
#' (Mandatory if shp_filepath argument passed).
#' @param map_title string to supply the chart title
#' @param map_footer string to supply the chart subtitle
#' @param area_labels boolean parameter to add static area labels to the map areas
#' @param area_labels_topn display static area labels for only map areas with the top n values
#' @param legend_title string to supply the legend title
#' @param labels list to supply label which appears in the hover over labels. Labels can include HTML
#' @param zoom_loc a data frame that supplies values for latitude, longitude and zoom which will set the view of the chart. Variable names should be LAT, LONG, zoom.

#'
#' @import classInt
#' @import RColorBrewer
#' @import grDevices
#' @import sf
#' @import leaflet
#' @import htmltools
#' @export

epi_map_leaflet <- function (df,
                             value_col,
                             data_areacode = NULL,
                             inc_shp = TRUE,
                             shp_filepath = NULL,
                             shp_areacode = NULL,
                             map_title = "",
                             map_footer = "",
                             area_labels = FALSE,
                             area_labels_topn = NULL,
                             legend_title = "",
                             labels = NULL,
                             zoom_loc = NULL) {

  # Check for any missing mandatory arguments

  # Check df is a df class
  if(!is.data.frame(df)== TRUE) stop("A data frame argument is required")

  # Check if df is missing
  if (missing(df)) stop("A data frame argument is required")

  # Check if value_col argument is missing
  if ((is.null(value_col)) | missing(value_col)) stop("Please inlcude argument data frame variable for value_col, ie value_col = variable_name")

  # Check if data_areacode argument is missing
  if ((is.null(data_areacode)) | missing(data_areacode)) stop("Please inlcude argument data frame variable for data_areacode, ie data_areacode = variable_name")

  # Check if value_col column name is in df
  if ( !value_col %in% colnames(df)) stop("Please inlcude correct name for argument data frame variable for value_col, ie value_col = variable_name")

  # Check if data_areacode column name is in df
  if ( !data_areacode %in% colnames(df)) stop("Please inlcude correct name for argument data frame variable for data_areacode, ie area_col = variable_name")




  # Read in data based on whether the df and shapefiles are already merged
  if(inc_shp == TRUE) {

    # Read in shapefile
    data_sf_merged <- st_as_sf(df)

  } else {

    # Read in shapefile
    area_sf <- st_read(shp_filepath)

    # Merge data and shapefile
    data_sf_merged <- merge(df, area_sf, by.x = data_areacode, by.y = shp_areacode)

  }


  # Assign the value column to the dataframe
  df <- data_sf_merged %>%
            mutate(Value = get({{value_col}}),
                   Area = get({{data_areacode}}))


  # DEPRECIATED IN FAVOUR OF CENTROID COORDS BELOW
  # # lat and long column naming conventions can differ between shp files, force rename
  # df <- df %>%
  #   rename_with(
  #     ~ case_when(
  #       . == "lat" ~ "LAT",
  #       . == "latitude" ~ "LAT",
  #       . == "long" ~ "LONG",
  #       . == "longitude" ~ "LONG",
  #       TRUE ~ .))


  # Define static area labels
  if(area_labels == TRUE) {

    # define geometry centroid long & lat as position of each label
    # Note:- Existing long & lat fields in shp files often denote regional capitals rather
    #        than the true centre of an area, so force definition here.
    df <- df %>%
      mutate(labels_static_long = sf::st_coordinates(sf::st_centroid(df$geometry))[,1],
             labels_static_lat = sf::st_coordinates(sf::st_centroid(df$geometry))[,2])

    if (is.null(area_labels_topn)) {

      df <- df %>% mutate(labels_static = Area)

    } else {

      # Rank data to only show top n labels
      df <- df %>%
        mutate(ranks = rank(desc(Value)),
               labels_static = case_when(ranks <= area_labels_topn ~ Area,
                                         TRUE ~ ""))
    }

  }



  # Add title controls

  tag.map.title <- htmltools::tags$style(htmltools::HTML("
  .leaflet-control.map-title {
    transform: translate(11%,-15%);
    position: absolute !important;
    width: max-content;
    text-align: left;
    padding-left: 10px;
    padding-right: 10px;
    opacity: 1;
    font-weight: bold;
    font-size: 13px;
    font-family: Helvetica;
    color:rgba(0,124,145,1.00);
  }
  "))
  ##d had to add 'htmltools::' before 'tags' and 'HTML' to get it to work
  ##d had to change 'position: fixed' to 'position: absolute' else the title would remain fixed in place whilst scrolling
  ##d changed to 'text-align: left;' and added 'width: max-content;' so that title wasn't squashed into narrow div box
  #note: add 'border: 1px solid black;' to identify box


  # Add footer controls

  tag.map.footer <- htmltools::tags$style(htmltools::HTML("
  .leaflet-control.map-footer {
    font-family: Helvetica;
    color:rgba(0,124,145,1.00);
  }
  "))
  ##d previous footer appears beneath html widget in Shiny, but is invisible in
  #   markdown and directly outputted widgets, amended the above CSS to print
  #   footer within map box.


  # Title text

  title <- htmltools::tags$div(
    tag.map.title, htmltools::HTML(gsub("/n","<br>",map_title))  # sub R linebreak for html linebreak
  )
  ##d had to add 'htmltools::' before 'tags' and 'HTML' to get it to work


  # Footer text

  footer <- htmltools::tags$div(
    tag.map.footer, htmltools::HTML(gsub("/n","<br>",map_footer))   # sub R linebreak for html linebreak
  )
  ##d had to add 'htmltools::' before 'tags' and 'HTML' to get it to work



  # Select colour palette

  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = df$Value
  )
  ##d add option to select colour palette?




  # Add labels if this isn't supplied

  if(is.null(labels)) {

    labels <- df %>%
      mutate(labs = paste0(Area, ": ",  Value))

    labels <- as.list(labels$labs)
  }




  # Create map

  map <- leaflet::leaflet(st_as_sf(df)) %>%


    ## add polygons

    addPolygons(stroke = TRUE,
                color = "white",
                weight="1",
                smoothFactor = 0.3,
                fillOpacity = 0.7,
                fillColor = ~pal(Value),
                label = lapply(labels, htmltools::HTML)
    ) %>%


    # Add title

    addControl(title, position = "topleft", className="map-title") %>%

    # Add footer

    addControl(footer, position = "bottomleft", className="map-footer") %>%

    ## add legend

    addLegend_decreasing(pal = pal,
                         values = ~df$Value,
                         opacity = 0.5,
                         title = gsub("/n","<br>",legend_title),  # sub R linebreak for html linebreak that will work in leaflet
                         position = "topright",
                         #group = "rate_per_100000",
                         #className = "info legend Count",
                         decreasing = TRUE)
    ##d optional legend?
    ##d 'group' and 'class' don't seem to have any effect?
    ##d replaced fixed title with dynamic title


  ## add static labels for top n Areas

  if(area_labels == TRUE) {

    map <- map %>%
      addLabelOnlyMarkers(lng = ~labels_static_long, #~LONG,
                          lat = ~labels_static_lat,  #~LAT,
                          label = ~labels_static,
                          labelOptions = labelOptions(noHide = TRUE,
                                                      direction = 'center',
                                                      textOnly = TRUE,
                                                      style = list("font-size" = "12px",
                                                                   "font-style" = "italic",
                                                                   #"color" = "blue",
                                                                   "font-family" = "sans-serif")
                                                      )
                            )
  }


  ## set default position and zoom

  if(!is.null(zoom_loc)){

    map <- map %>%
      setView(lat = zoom_loc$LAT,
              lng = zoom_loc$LONG,
              zoom = zoom_loc$zoom)
  }

} #epi_map_leaflet end



# credit: https://github.com/rstudio/leaflet/issues/256#issuecomment-440290201
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors,
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(),
                                  title = NULL, className = "info legend", layerId = NULL,
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {

  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors))
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula"))
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] ==
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins))
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1)
        pretty(values, bins)
      else bins
      if (length(bins) > 2)
        if (!all(abs(diff(bins, differences = 2)) <=
                 sqrt(.Machine$double.eps)))
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values)))
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels))
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)),
                 na_color = na.color, na_label = na.label, opacity = opacity,
                 position = position, type = type, title = title, extra = extra,
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)

}


