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

`epiviz` provides easy-to-use data visualisation functions for R data science products. The package includes functions to create a range of static and interactive visualisations. Additionally, the package also provides helper functions that will allow converting line lists to aggregated data frames that are in the form ready to be consumed by the visualisation functions.

## Installation

epivis can be installed from GitHub used the following code.
```r
remotes::install_github('harshanal/epiviz')

# To install the development version
remotes::install_github("harshanal/epiviz@dev")
``` 

## Features

Following functions are available in the current release of epiviz:

- `line_chart()`: Line chart with options to add dual axes, threshold lines, confidence limits as ribbons/error bars.
- `line_chart_plotly()`: Dynamic line chart.
- `col_charts()`: Column chart with options to add dual axes, threshold lines, confidence limits as ribbons/error bars.
- `epi_map()`: Epidemiological map with customisable maps adaptable for multiple administrative levels.
- `agesex_pyramid()`: Age-sex pyramid with customisable age bands with the option to add error bars.

## Package data

epiviz includes an anonymised sample dataset from the SGSS laboratory database: `lab_data()`

The Second Generation Surveillance System (SGSS) is a data asset held by the [UK Health Security Agency](https://www.gov.uk/government/publications/securing-our-health-the-uk-health-security-agency/securing-our-health-the-uk-health-security-agency) (formerly Public Health England) which routinely and automatically collects laboratory data from across England.

Laboratories return data on organisms isolated from samples such as the organism species, specimen type, sampling date and antimicrobial susceptibility testing results. These data are routinely used for public health surveillance and epidemiology in England.
