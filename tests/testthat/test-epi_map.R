
# test that function returns a ggplot object when provided with an un-merged shapefile
test_that("epi_map returns a ggplot object when provided with un-merged shapefile", {

  library(dplyr)
  library(epiviz)


  data <- epiviz::lab_data
  data_processed <- data |>
    filter(region == "London",
           organism_species_name == "STAPHYLOCOCCUS AUREUS") |>
    group_by(local_authority_name) |>
    summarise(detections = n())


  shape <- epiviz::London_LA_boundaries_2023


  result <- epi_map(
              dynamic = FALSE,
              params = list(
                df = data_processed,
                value_col = "detections",
                data_areacode = "local_authority_name",
                inc_shp = FALSE,
                shp_name = shape,
                shp_areacode = "LAD23NM",
                fill_palette = "Blues",
                fill_opacity = 1.0,
                break_intervals = NULL,
                break_labels = NULL,
                force_cat = TRUE,
                n_breaks = NULL,
                labels = NULL,
                map_title = "Staphylococcus Aureus detections in London Local Authority Districts",
                map_title_size = 13,
                map_title_colour = "black",
                map_footer = "",
                map_footer_size = 12,
                map_footer_colour = "black",
                area_labels = TRUE,
                area_labels_topn = NULL,
                legend_title = "Number of \nDetections",
                legend_pos = "right",
                map_zoom = data.frame(LONG = c(-0.12776), LAT = c(51.50735), zoom = c(8.7)),
                border_shape_name = NULL,
                border_code_col = NULL,
                border_areaname = NULL
              )
            )

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



# test that function returns a ggplot object when provided with a pre-merged shapefile
test_that("epi_map returns a ggplot object when provided with pre-merged shapefile", {

  library(dplyr)
  library(epiviz)


  data <- epiviz::lab_data
  data_processed <- data |>
    filter(organism_species_name == "KLEBSIELLA PNEUMONIAE") |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(map_labels = paste0(region,": \n",detections))


  shape <- epiviz::PHEC_boundaries_2016

  merged <- left_join(x = shape, y = data_processed,
                      by = c("phec16nm" = "region"))


  params <- list(df = merged,
                 value_col = "detections",
                 data_areacode = "phec16nm",
                 inc_shp = TRUE,
                 shp_name = NULL,
                 shp_areacode = NULL,
                 fill_palette = "YlOrRd",
                 fill_opacity = 0.7,
                 break_intervals = c(0,500,1000,1500,2000,2500),
                 break_labels = c("0-499","500-999","1000-1499","1500-1999","2000-2499","2500+"),
                 force_cat = TRUE,
                 n_breaks = NULL,
                 labels = "map_labels",
                 map_title = "Number of Klebsiella Pneumoniae detections \nin UK public health regions",
                 map_title_size = 12,
                 map_title_colour = "orangered",
                 map_footer = "Map represents simulated test data only.",
                 map_footer_size = 10,
                 map_footer_colour = "black",
                 area_labels = FALSE,
                 area_labels_topn = NULL,
                 legend_title = "Number of \nDetections",
                 legend_pos = "topright",
                 map_zoom = NULL,
                 border_shape_name = NULL,
                 border_code_col = NULL,
                 border_areaname = NULL)

  result <- epi_map(dynamic = FALSE, params = params)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



# test that function returns a ggplot object when provided with a border shapefile
test_that("epi_map returns a ggplot object when provided with a border shapefile", {

  library(dplyr)
  library(epiviz)


  data <- epiviz::lab_data
  data_processed <- data |>
    filter(organism_species_name == "KLEBSIELLA PNEUMONIAE") |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(map_labels = paste0(region,": \n",detections))


  shape <- epiviz::PHEC_boundaries_2016
  border_shape <- epiviz::UK_boundaries_2023

  params <- list(df = data_processed,
                 value_col = "detections",
                 data_areacode = "region",
                 inc_shp = FALSE,
                 shp_name = shape,
                 shp_areacode = "phec16nm",
                 fill_palette = "YlOrRd",
                 fill_opacity = 0.7,
                 break_intervals = c(0,500,1000,1500,2000,2500),
                 break_labels = c("0-499","500-999","1000-1499","1500-1999","2000-2499","2500+"),
                 force_cat = TRUE,
                 n_breaks = NULL,
                 labels = "map_labels",
                 map_title = "Number of Klebsiella Pneumoniae detections \nin UK public health regions",
                 map_title_size = 12,
                 map_title_colour = "orangered",
                 map_footer = "Map represents simulated test data only.",
                 map_footer_size = 10,
                 map_footer_colour = "black",
                 area_labels = FALSE,
                 area_labels_topn = NULL,
                 legend_title = "Number of \nDetections",
                 legend_pos = "topright",
                 map_zoom = NULL,
                 border_shape_name = border_shape,
                 border_code_col = "CTRY23NM",
                 border_areaname = c("Wales", "Northern Ireland", "Scotland")
                 )


  result <- epi_map(dynamic = FALSE, params = params)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



# test that function returns a leaflet object when provided with an un-merged shapefile
test_that("epi_map returns a leaflet object when provided with un-merged shapefile", {

  library(dplyr)
  library(epiviz)


  data <- epiviz::lab_data
  data_processed <- data |>
    filter(region == "London",
           organism_species_name == "STAPHYLOCOCCUS AUREUS") |>
    group_by(local_authority_name) |>
    summarise(detections = n())


  shape <- epiviz::London_LA_boundaries_2023


  result <- epi_map(
    dynamic = TRUE,
    params = list(
      df = data_processed,
      value_col = "detections",
      data_areacode = "local_authority_name",
      inc_shp = FALSE,
      shp_name = shape,
      shp_areacode = "LAD23NM",
      fill_palette = "Blues",
      fill_opacity = 1.0,
      break_intervals = NULL,
      break_labels = NULL,
      force_cat = TRUE,
      n_breaks = NULL,
      labels = NULL,
      map_title = "Staphylococcus Aureus detections in London Local Authority Districts",
      map_title_size = 13,
      map_title_colour = "black",
      map_footer = "",
      map_footer_size = 12,
      map_footer_colour = "black",
      area_labels = TRUE,
      area_labels_topn = NULL,
      legend_title = "Number of \nDetections",
      legend_pos = "topright",
      map_zoom = data.frame(LONG = c(-0.12776), LAT = c(51.50735), zoom = c(8.7)),
      border_shape_name = NULL,
      border_code_col = NULL,
      border_areaname = NULL
    )
  )

  # check that the output is a leaflet object
  expect_true(inherits(result, "leaflet"))

})



# test that function returns a leaflet object when provided with a pre-merged shapefile
test_that("epi_map returns a leaflet object when provided with pre-merged shapefile", {

  library(dplyr)
  library(epiviz)


  data <- epiviz::lab_data
  data_processed <- data |>
    filter(organism_species_name == "KLEBSIELLA PNEUMONIAE") |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(map_labels = paste0(region,": \n",detections))


  shape <- epiviz::PHEC_boundaries_2016

  merged <- left_join(x = shape, y = data_processed,
                      by = c("phec16nm" = "region"))


  params <- list(df = merged,
                 value_col = "detections",
                 data_areacode = "phec16nm",
                 inc_shp = TRUE,
                 shp_name = NULL,
                 shp_areacode = NULL,
                 fill_palette = "YlOrRd",
                 fill_opacity = 0.7,
                 break_intervals = c(0,500,1000,1500,2000,2500),
                 break_labels = c("0-499","500-999","1000-1499","1500-1999","2000-2499","2500+"),
                 force_cat = TRUE,
                 n_breaks = NULL,
                 labels = "map_labels",
                 map_title = "Number of Klebsiella Pneumoniae detections \nin UK public health regions",
                 map_title_size = 12,
                 map_title_colour = "orangered",
                 map_footer = "Map represents simulated test data only.",
                 map_footer_size = 10,
                 map_footer_colour = "black",
                 area_labels = FALSE,
                 area_labels_topn = NULL,
                 legend_title = "Number of \nDetections",
                 legend_pos = "topright",
                 map_zoom = NULL,
                 border_shape_name = NULL,
                 border_code_col = NULL,
                 border_areaname = NULL
                 )


  result <- epi_map(dynamic = TRUE, params = params)

  # check that the output is a leaflet object
  expect_true(inherits(result, "leaflet"))

})



# test that function returns a leaflet object when provided with a border shapefile
test_that("epi_map returns a leaflet object when provided with a border shapefile", {

  library(dplyr)
  library(epiviz)


  data <- epiviz::lab_data
  data_processed <- data |>
    filter(organism_species_name == "KLEBSIELLA PNEUMONIAE") |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(map_labels = paste0(region,": \n",detections))


  shape <- epiviz::PHEC_boundaries_2016
  border_shape <- epiviz::UK_boundaries_2023


  params <- list(df = data_processed,
                 value_col = "detections",
                 data_areacode = "region",
                 inc_shp = FALSE,
                 shp_name = shape,
                 shp_areacode = "phec16nm",
                 fill_palette = "YlOrRd",
                 fill_opacity = 0.7,
                 break_intervals = c(0,500,1000,1500,2000,2500),
                 break_labels = c("0-499","500-999","1000-1499","1500-1999","2000-2499","2500+"),
                 force_cat = TRUE,
                 n_breaks = NULL,
                 labels = "map_labels",
                 map_title = "Number of Klebsiella Pneumoniae detections \nin UK public health regions",
                 map_title_size = 12,
                 map_title_colour = "orangered",
                 map_footer = "Map represents simulated test data only.",
                 map_footer_size = 10,
                 map_footer_colour = "black",
                 area_labels = TRUE,
                 area_labels_topn = 5,
                 legend_title = "Number of \nDetections",
                 legend_pos = "topright",
                 map_zoom = data.frame(LONG = c(-2.89479), LAT = c(54.793409), zoom = c(5)),
                 border_shape_name = border_shape,
                 border_code_col = "CTRY23NM",
                 border_areaname = c("Wales", "Northern Ireland", "Scotland")
                 )


  result <- epi_map(dynamic = TRUE, params = params)

  # check that the output is a leaflet object
  expect_true(inherits(result, "leaflet"))

})

