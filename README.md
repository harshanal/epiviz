# epiviz <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->
`epiviz` provides easy to use data visualisation functions for R data science products. The package includes functions to create a range of static and interactive visualisations. Additionally, the package also provide helper functions that will allow converting line lists to aggregated data frames that are in the form ready to be consumed by the visualisation functions. 

## Installation

```bash
devtools::install('harshanal/epiviz')
```
## Features
Following functions are available in the current release of epiviz:
* *line_chart()*: line chart with options to add dual axis, threshold lines, confidence limits as ribbon/error bars
* *line_chart_plotly*: dynamic line chart
* *col_charts()*: column chart with options to add dual axis, threshold lines, confidence limits as ribbon/error bars
* *epi_map()*: epidemiological map with customisable maps adaptable for multiple administrative levels
* *agesex_pyramid()*: age sex pyramid with customisable age bands with the option to add error bars

## Package data
epivis includes a dataset consisting of laboratory data. 
