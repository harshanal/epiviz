#' epi_map_leaflet
#'
#' @description A function to produce either static (ggplot) or dynamic (leaflet)
#' choropleth maps.
#'
#' @param dynamic Logical indicating whether to produce a dynamic (leaflet) output.
#' Default is \code{FALSE}, which will return a static ggplot output.
#' @param df Data frame containing values used to fill areas on the output map.
#' Can include pre-merged shapefile data if inc_shp = TRUE.
#' @param value_col Name of the variable in df used to fill map areas.
#' @param data_areacode Name of the variable in df containing the name or code of
#' the map areas to be plotted. (Mandatory if shp_filepath argument passed).
#' @param inc_shp boolean parameter to indicate whether df already includes shapefile
#' data.
#' @param shp_filepath filepath for the shapefile containing the spatial information
#' for the resultant map output. This will not be used if inc_shp = TRUE.
#' @param shp_areacode Name of the variable in shp_filepath containing the name
#' or code of the map areas to be plotted. (Mandatory if shp_filepath argument passed).
#' @param fill_palette name of the RColorBrewer palette to use in map fill (default = "Blues")
#' @param fill_opacity numeric value between 0 and 1 to determine map fill-color opacity.
#' @param break_intervals numeric vector of interval points for legend
#' (Mandatory if break_labels argument is passed, break_intervals and break_labels must be
#' of equal length).
#' @param break_labels vector of labels to include in the legend.
#' (Mandatory if break_labels argument is passed, break_intervals and break_labels must be
#' of equal length).
#' @param force_cat boolean parameter to determine whether all arguments passed
#' in break_labels are used in the legend, even if there are no values present in the data.
#' @param n_breaks Number of break intervals. This argument is an alternative
#' to supplying defined breaks via break_labels, and will provide a number of
#' evenly distributed breaks as specified (default = 5). If break_labels argument is passed,
#' n_breaks will be ignored.
#' @param labels name of string variable in df containing labels for each map area. If
#' dynamic = FALSE, these labels will be positioned in the centre of each map area.
#' If dynamic = TRUE, then these labels will appear as hover-over labels. If dynamic =
#' TRUE, labels can include HTML.
#' @param map_title string to determine map title.
#' @param map_title_size font size of map title.
#' @param map_title_colour string to determine map title colour.
#' @param map_footer string to determine map footer.
#' @param map_footer_size font size of map footer.
#' @param map_footer_colour string to determine map title colour.
#' @param area_labels boolean parameter to add data_areacode as static area labels to the
#' map areas. If dynamic = FALSE and a labels parameter has alredy been supplied, then area_labels
#' will be ignored.
#' @param area_labels_topn numeric value to display only area_labels for areas with
#' the top n values of value_col (e.g. if area_labels_topn = 5, only area_labels for map
#' areas with the top 5 values of value_col will be displayed).
#' @param legend_title string to determine legend title.
#' @param legend_pos string to determine legend position. When dynamic = TRUE, both
#' ggplot and leaflet permissable legend positions can be provided. When dynamic = FALSE,
#' only leaflet permissable legend positions can be provided (i.e."topright", "bottomright",
#'  "bottomleft", or "topleft").
#' @param map_zoom A single row data frame with variables of 'LAT', 'LONG', and 'zoom'
#' that allows the map to be zoomed in on a specific region (e.g. data.frame(LONG =
#' -2.547855, LAT = 53.00366, zoom = 6)). LAT = numerical latitude coordinate for
#' the centre point of the zoom, LONG = numerical longitude coordinate for the
#' centre point of the zoom, zoom = numerical value to represent the depth of zoom.
#' @param border_shape_filepath Optional filepath for a shapefile containing additional
#' borders to include in the output map. This should be a higher geography than the base
#' map (e.g. if creating a map displaying UTLAs, a shapefile containing regional boundaries
#' or higher should be used). Only boundaries contained within border_shape_filepath will
#' be used, areas will be unfilled.
#' @param border_code_col Variable name of the area code / name within border_shape_filepath.
#' Required if a specific area within the border shapefile is required.
#' @param border_areaname String containing the name of a specific area within
#' border_code_col to be plotted. If supplied, only the boundaries of border_areaname will
#' be plotted. If not supplied, the boundaries of all areas within border_shape_filepath
#' will be plotted.
#'
#'
#' @return A ggplot or leaflet object.
#'
#' @import classInt
#' @import RColorBrewer
#' @import grDevices
#' @import sf
#' @import leaflet
#' @import htmltools
#' @export
#'
#' @examples
#'
epi_map_leaflet <- function (dynamic = FALSE,
                             df,
                             value_col,
                             data_areacode = NULL,
                             inc_shp = TRUE,
                             shp_filepath = NULL,
                             shp_areacode = NULL,
                             fill_palette = "Blues",
                             fill_opacity = 1.0,
                             break_intervals = NULL,
                             break_labels = NULL,
                             force_cat = TRUE,
                             n_breaks = NULL,
                             labels = NULL,
                             map_title = "",
                             map_title_size = 13,
                             map_title_colour = "#007C91",
                             map_footer = "",
                             map_footer_size = 12,
                             map_footer_colour = "#007C91",
                             area_labels = FALSE,
                             area_labels_topn = NULL,
                             legend_title = "",
                             legend_pos = "topright",
                             map_zoom = NULL,
                             border_shape_filepath = NULL,
                             border_code_col = NULL,
                             border_areaname = NULL) {


  #solves warnings regarding font family not found
  windowsFonts("Arial" = windowsFont("Arial"))
  windowsFonts(Arial = windowsFont("Arial"))


  # Checks and warnings

  # Check df is a df class
  if(!is.data.frame(df) == TRUE) stop("Argument df is not a data frame object")

  # Check if df is missing
  if (missing(df)) stop("A data frame argument is required")

  # Check if value_col argument is missing
  if ((is.null(value_col)) | missing(value_col))
    stop("Please include a data frame variable for value_col, i.e. value_col = \"variable_name\"")

  # Check if value_col argument is missing
  if ((is.null(value_col)) | missing(value_col))
    stop("Please include a data frame variable for value_col, i.e. data_areacode = \"variable_name\"")

  # Check if data_areacode argument is missing
  if ((is.null(data_areacode)) | missing(data_areacode))
    stop("Please include a data frame variable for data_areacode, i.e. data_areacode = \"variable_name\"")

  # Check if value_col column name is in df
  if (!value_col %in% colnames(df))
    stop("value_col not found within df. Please include a data frame variable for value_col, i.e. data_areacode = \"variable_name\"")

  # Check if data_areacode column name is in df
  if (!data_areacode %in% colnames(df))
    stop("data_areacode not found within df. Please include a data frame variable for data_areacode, i.e. data_areacode = \"variable_name\"")

  # Check if inc_shp = FALSE but no shp_filepath provided
  if (inc_shp == FALSE & is.null(shp_filepath))
    stop("A shp_filepath arguement must be provided when inc_shp = FALSE")

  # Check that if break_intervals has been passed then break_labels has also been passed
  if (!is.null(break_intervals) & is.null(break_labels))
    stop("If break_intervals is provided then break_labels must also be provided")
  if (is.null(break_intervals) & !is.null(break_labels))
    stop("If break_labels is provided then break_intervals must also be provided")

  # Check that break_labels and break_intervals are the same length
  if (!is.null(break_intervals)) {
    if (length(break_labels) != length(break_intervals)) {
      stop("break_labels and break_intervals must be vectors of equal length")
    }
  }

  # Check if dynamic = TRUE but legend_pos not a leaflet permissable value
  if (dynamic == TRUE & !(legend_pos %in% c("topright", "bottomright", "bottomleft", "topleft")))
    stop("When dynamic = TRUE, legend_pos must equal \"topright\", \"bottomright\", \"bottomleft\", or \"topleft\"")

  # Check that map_zoom df contains the 'LAT', 'LONG', and 'zoom' variables
  if (!is.null(map_zoom) & !all(c("LONG","LAT","zoom") %in% names(map_zoom)))
    stop("map_zoom must be a single-row df containing variables of 'LAT', 'LONG', and 'zoom',
         e.g. data.frame(LONG = -2.547855, LAT = 53.00366, zoom = 6)")

  # Check that map_zoom df contains only a single row
  if (!is.null(map_zoom)) {
    if(nrow(map_zoom) != 1)
      stop("map_zoom must be a single-row df containing variables of 'LAT', 'LONG', and 'zoom',
           e.g. data.frame(LONG = -2.547855, LAT = 53.00366, zoom = 6)")
  }

  # Check that if border_shape_filepath has been provided that border_code_col has also been provided
  if (!is.null(border_shape_filepath) & is.null(border_code_col))
    stop("border_code_col must be provided when border_shape_filepath is provided")

  # Warn that n_breaks will be ignored if break_intervals is set
  if (!is.null(break_intervals) & !is.null(n_breaks))
    warning("n_breaks will be ignored if break_intervals is set")

  # Warn that area_labels will not be used for static map if labels are provided
  if ((dynamic == FALSE) & (area_labels == TRUE) & !is.null(labels))
    warning("area_labels will not be used if labels are provided when dynamic = FALSE")

  # Warn
  # shp_filepath included but inc_shape == TRUE


  # Read in data based on whether the df and shapefile are already merged
  if(inc_shp == TRUE) {

    # Read in shapefile, assign 'Area' variable
    df <- st_as_sf(df) |>
      mutate(Area = get({{data_areacode}}))

  } else {

    # Read in shapefile
    area_sf <- st_read(shp_filepath)

    # Merge data and shapefile, assign 'Area' variable
    df <- merge(area_sf, df, by.x = shp_areacode, by.y = data_areacode) |>
      mutate(Area = get({{shp_areacode}}))

  }


  # Assign 'Value' column to dataframe
  df <- df |>
    mutate(Value = get({{value_col}}))


  # define area centroid long & lat for label positions
  df <- df |>
    mutate(centroid_long = sf::st_coordinates(sf::st_centroid(df$geometry))[,1],
           centroid_lat = sf::st_coordinates(sf::st_centroid(df$geometry))[,2])


  # Define static area labels
  if(area_labels == TRUE) {

    # Rank data to only show top n labels
    if (!is.null(area_labels_topn)) {

      df <- df |>
        mutate(ranks = rank(desc(Value)),
               labels_static = case_when(ranks <= area_labels_topn ~ Area,
                                         TRUE ~ ""))

    } else {

      df <- df |> mutate(labels_static = Area)

    }

  }


  # Define colour palette and legend

      # pal <- colorNumeric(
      #   palette = "YlOrRd",
      #   domain = df$Value
      # )

  # Set breaks and break labels depending on whether they are predefined
  if (is.null(break_intervals)) {
    # Set default of n_breaks to 5 if this is missing
    if (is.null(n_breaks)) {
      n_breaks <- 5

    }

    # # get quantile breaks. Add .00001 offset to catch the lowest value
    # breaks_qt <-
    #   classIntervals(c(min(unlist(df$Value)),# - .00001,
    #                    unlist(df$Value)),
    #                  n = n_breaks, style = "quantile")

    # Re-format labels; remove ( and [  brackets and change ',' to ' - '
    data_sf <- df |>
      # mutate(value_cat = cut(unlist(Value), breaks_qt$brks, dig.lab=10)) |>  # dig.lab=10 to eliminate scientific notation in legend
      mutate(value_cat = cut(unlist(Value), n_breaks, dig.lab=10)) |>
      mutate(value_cat = gsub("\\(|\\]", "", unlist(value_cat))) |>
      mutate(value_cat = gsub("\\,", " - ", unlist(value_cat)))

    # Force force_cat to be FALSE
    force_cat <- FALSE

  } else {
    # count number of interval groups
    interval_grp <- c(1:length(break_intervals))

    # Create breaks of break intervals and labels
    breaks_qt <-
      data.frame(interval = interval_grp , value_cat = break_labels)

    # Assign values to intervals- then join labels
    data_sf <- df |>
      mutate(interval = findInterval(unlist(Value), break_intervals)) |>
      left_join(breaks_qt, by = "interval")

    # Create order based on which categories are included
    order <-
      data.frame(interval = data_sf$interval,
                 value_cat = data_sf$value_cat) |>
      arrange(interval) |>
      distinct()

    # Re-order factor to correct order
    data_sf <- data_sf |>
      mutate(
        value_cat = factor(value_cat, order$value_cat),
        interval = as.factor(interval)
      )


  }

  if (force_cat == TRUE) {
    # Count number of intervals for palette
    n_pal <- as.numeric(length(break_intervals))

    # Designate RColorBrewer palatte for map
    pal <- RColorBrewer::brewer.pal(n = n_pal, name = fill_palette)

    # Create df of colours + categories for legend
    pal <- data.frame(value_cat = break_labels,
                      fill_colour = pal)

    # To use in drop category argument for map
    drop_cat <- FALSE


  } else {

    # Count number of intervals for palette
    if(!is.null(n_breaks)) {
      n_pal <- n_breaks
    } else {
      n_pal <- as.numeric(length(unique(data_sf$value_cat)))
    }

    # Designate RColorBrewer palette for map
    pal <- RColorBrewer::brewer.pal(n = n_pal, name = fill_palette)

    # Create df of colours + categories for legend

    # legend_order <- data.frame(Value = data_sf$Value,
    #                            value_cat = data_sf$value_cat) |>
    #   arrange(Value) |> select(-Value) |> distinct()

    # pal <- data.frame(value_cat = legend_order$value_cat,
    #                   fill_colour = pal)

    pal <- data.frame(value_cat = levels(cut(unlist(df$Value), n_breaks, dig.lab=10)),
                      fill_colour = pal) |>
            mutate(value_cat = gsub("\\(|\\]", "", unlist(value_cat))) |>
            mutate(value_cat = gsub("\\,", " - ", unlist(value_cat)))

    # To use in drop category argument for map
    drop_cat <- TRUE

  }

  # merge fill-colors back into df for use in leaflet
  df <- merge(data_sf, pal, by = "value_cat")




  # Define border if provided

  if (!is.null(border_shape_filepath)) {
    # Read in shapefile
    border_sf <- st_read(border_shape_filepath)

    # Filter for specific area for border in shapefile
    if (!is.null(border_code_col)) {
      # Check if we have area to filter by, if not print message
      if (is.null(border_areaname)) {
        message("border_areaname is missing so all border shapefile areas will be included")

      } else {

      # Filter area
      border_sf <- border_sf |>
        filter(get({{border_code_col}}) == border_areaname)

      }

    }

  }



  ### produce plot

  if (!dynamic) {
    # produce ggplot object if 'dynamic' is set to FALSE

    ### GGPLOT START

    # If legend title is missing set to blank
    if (is.null(legend_title)) {
      legend_title <- ""

    }

    # Create map
    map <- ggplot() +
      geom_sf(data = data_sf,
              alpha = fill_opacity,
              key_glyph = "rect", # remove border around legend keys
              aes(fill = value_cat),
              show.legend = TRUE) +  # ensure unused legend keys are coloured in: https://github.com/tidyverse/ggplot2/issues/5728
      # Set colours
      scale_fill_manual(
        values = alpha(pal$fill_colour, fill_opacity),
        limits = pal$value_cat,
        # Drop or keep all category levels for legend based on selection
        drop = drop_cat,
        # Add legend title
        name = legend_title
      ) +
      # Set title
      ggtitle(map_title) +
      # Set footer
      labs(caption = map_footer)


    # Add user defined labels
    if(!is.null(labels)) {

      map <- map +
        geom_text(
          data = data_sf,
          size = 8/.pt,
          #family = 'Ariel',
          aes(x = centroid_long,
              y = centroid_lat,
              family = 'Ariel',
              label = get({{labels}}))
              #label = stringr::str_wrap(get({{labels}}),12))
        )
    }


    # Add area name labels; overidden if other area labels are defined
    if(area_labels == TRUE & is.null(labels)) {

      map <- map +
        geom_text(
          data = data_sf,
          size = 8/.pt,
          #family = 'Ariel',
          aes(x = centroid_long,
              y = centroid_lat,
              label = labels_static,
              family = 'Ariel',
              fontface = 'italic')
        )
    }


    # Add map border if provided

    if (!is.null(border_shape_filepath)) {

      # Add border overlay
      map <- map +
        geom_sf(
          data = border_sf,
          fill = NA,
          size = 0.2,
          colour = "black"
        )

    }


    # Zoom in if map_zoom is specified

    if (!is.null(map_zoom)) {

      # Define new co-ordinate bounding box from zoom parameters
      # credit: https://www.r-bloggers.com/2019/04/zooming-in-on-maps-with-sf-and-ggplot2/
      lon_span <- 360 / 2^map_zoom$zoom
      lat_span <- 180 / 2^map_zoom$zoom

      lon_bounds <- c(map_zoom$LONG - lon_span / 2, map_zoom$LONG + lon_span / 2)
      lat_bounds <- c(map_zoom$LAT - lat_span / 2, map_zoom$LAT + lat_span / 2)

      # Apply zoomed co-rdinate limits
      map <- map +
        coord_sf(xlim = lon_bounds, ylim = lat_bounds)

    }


    # Add formatting
    map <- map +
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
          size = 10,
          family = "Arial"
        ),
        # Change legend position
        #legend.position = legend_pos,
        # Set title formatting
        plot.title = element_text(
          face = "bold",
          size = map_title_size,
          family = "Ariel",
          colour = map_title_colour),
        # Set footer formatting
        plot.caption = element_text(
          hjust = 0,
          size = map_footer_size,
          colour = map_footer_colour)
      )


    # Position legend if legend_pos matches leaflet-only inputs
    if (legend_pos == "bottomright") {
      map <- map +
        theme(
          legend.position='right',
          legend.justification='bottom'
        )
    } else if (legend_pos == "topright") {
      map <- map +
        theme(
          legend.position='right',
          legend.justification='top'
        )
    } else if (legend_pos == "bottomleft") {
      map <- map +
        theme(
          legend.position='left',
          legend.justification='bottom'
        )
    } else if (legend_pos == "topleft") {
      map <- map +
        theme(
          legend.position='left',
          legend.justification='top'
        )
    } else {
      map <- map +
        theme(legend.position = legend_pos)
    }

    return(map)

    ### GGPLOT END


  } else {
    # produce leaflet object if 'dynamic' is set to TRUE

  ### LEAFLET START

  # Add title controls

  tag.map.title <- htmltools::tags$style(htmltools::HTML(
    paste0(
    "
    .leaflet-control.map-title {
      transform: translate(30px,-0px);
      position: absolute !important;
      width: max-content;
      text-align: left;
      padding-left: 10px;
      padding-right: 10px;
      opacity: 1;
      font-weight: bold;
      font-size:", map_title_size, "px;
      font-family: Helvetica;
      color:", map_title_colour,";
    }
    ")))
    # border: 1px solid black;
    # transform: translate(14%,-15%);


  # Add footer controls

  tag.map.footer <- htmltools::tags$style(htmltools::HTML(
  paste0(
  "
  .leaflet-control.map-footer {
    font-family: Helvetica;
    font-size:", map_footer_size, "px;
    color:", map_footer_colour,";
  }
  ")))


  # Title text

  title <- htmltools::tags$div(
    tag.map.title, htmltools::HTML(gsub("\n","<br>",map_title))  # sub R linebreak for html linebreak
  )


  # Footer text

  footer <- htmltools::tags$div(
    tag.map.footer, htmltools::HTML(gsub("\n","<br>",map_footer))   # sub R linebreak for html linebreak
  )



  # Define list of labels for leaflet hover, add if not supplied

  if(is.null(labels)) {

    labels <- df |>
      mutate(labs = paste0(Area, ": ",  Value))

    labels <- as.list(labels$labs)

  } else {

    labels <- df |>
      mutate(labs = gsub("\\\\n","<br>",get({{labels}})))   # sub R linebreak for html linebreak

    labels <- as.list(labels$labs)

  }



  # Create map

  map <- leaflet::leaflet(st_as_sf(df)) |>


    ## add polygons

    addPolygons(stroke = TRUE,
                color = "black",
                weight = "0.5",
                smoothFactor = 0.3,
                #fillOpacity = 0.7,
                #fillColor = ~pal(Value),
                fillOpacity = fill_opacity,
                fillColor = ~fill_colour,
                label = lapply(labels, htmltools::HTML),
                highlightOptions = highlightOptions(color = "white",
                                                    weight = 2,
                                                    bringToFront = TRUE)
    ) |>


    # Add title

    addControl(title, position = "topleft", className="map-title") |>

    # Add footer

    addControl(footer, position = "bottomleft", className="map-footer") |>

    # add legend

    addLegend(colors = pal$fill_colour,
              labels = pal$value_cat,
              values = ~df$Value,
              opacity = fill_opacity,
              title = gsub("\n","<br>",legend_title),  # sub R linebreak for html linebreak
              position = legend_pos)
  ##d optional legend?


  ## add static labels for top n Areas

  if(area_labels == TRUE) {

    map <- map |>
      addLabelOnlyMarkers(lng = ~centroid_long,
                          lat = ~centroid_lat,
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

  if(!is.null(map_zoom)){

    map <- map |>
      setView(lat = map_zoom$LAT,
              lng = map_zoom$LONG,
              zoom = map_zoom$zoom)
  }


  # Add border if provided

  if (!is.null(border_shape_filepath)) {

    # Add border overlay
    map <- map |>
      addPolylines(data = border_sf,
                  color = "black",
                  weight = 0.3,
      )

  }

  return(map)

}  ### LEAFLET END

} ### epi_map_leaflet end







