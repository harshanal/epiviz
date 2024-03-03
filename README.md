---
title: "epiviz: Easy Data Visualization for R"
output:
  html_document:
    theme: cosmo
---

# epiviz <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`epiviz` provides easy-to-use data visualization functions for R data science products. The package includes functions to create a range of static and interactive visualizations. Additionally, the package also provides helper functions that will allow converting line lists to aggregated data frames that are in the form ready to be consumed by the visualization functions.

## Installation

```r
devtools::install_github('harshanal/epiviz')
``` 

## Features

Following functions are available in the current release of epiviz:

- `line_chart()`: Line chart with options to add dual axes, threshold lines, confidence limits as ribbons/error bars.
- `line_chart_plotly()`: Dynamic line chart.
- `col_charts()`: Column chart with options to add dual axes, threshold lines, confidence limits as ribbons/error bars.
- `epi_map()`: Epidemiological map with customizable maps adaptable for multiple administrative levels.
- `agesex_pyramid()`: Age-sex pyramid with customizable age bands with the option to add error bars.

## Package data

epiviz includes a dataset consisting of laboratory data.
