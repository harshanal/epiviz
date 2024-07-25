#' Function to scale/shift secondary y axis labels
#'
#' @param x the value to be scaled/shifted
#' @param scale the value to scale the axis by
#' @param shift the value to shift the axis by
#'
#' @return scaled/shifted y axis values
#'
#' @examples
#' \dontrun{
#' scale_function(2,4,5)
#' }
scale_function <- function(x, scale, shift){
  return ((x)*scale - shift)
}




#' Function to scale/shift secondary y axis data
#'
#' @param x the value to be scaled/shifted
#' @param scale the value to scale the data by
#' @param shift the value to shift the data by
#'
#' @return scaled/shifted data to plot on secondary y axis
#'
#' @examples
#' \dontrun{
#' inv_scale_function(2,4,5)
#' }
inv_scale_function <- function(x, scale, shift){
  return ((x + shift)/scale)
}



#' Function for determining user's operating system
#' credit: https://www.r-bloggers.com/2015/06/identifying-the-os-from-r/
#'
#' @return Name of user's operating system
#'
#' @examples
#' \dontrun{
#' get_os()
#' }
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}




#' Function to set chart_font variable to Arial in order to resolve warnings
#'
#' @return chart_font string
#'
#' @examples
#' \dontrun{
#' set_Arial()
#' }
set_Arial <- function() {
  if(get_os()[[1]] == "windows") {
    windowsFonts("Arial" = windowsFont("Arial"))
    chart_font <- "Arial"
  } else if(get_os()[[1]] == "osx") {
    chart_font <- "Arial"
  } else {
    # Arial not included with linux as standard, so default to sans
    chart_font <- "sans"
  }
  assign("chart_font", chart_font, envir = parent.frame())
}





#' Function to assign list elements to variables within the parent
#' environment. Compares input list against a reference vector of expected
#' list elements, and assigns any that it cannot find a value of 'NULL'.
#'
#' @param params List of input parameters.
#' @param reference Character vector of expected parameters.
#'
#' @return Assigns parameter values in the parent environment
#'
#' @examples
#' \dontrun{
#' param_assign(params, c("df","x","y","legend","title"))
#' }
param_assign <- function(params, reference) {

  # Define parameters from params list
  for(i in 1:length(params)) {
    assign(names(params)[i], params[[i]], envir = parent.frame())
  }

  # Set any unused parameter values to NULL
  unused <- setdiff(reference, names(params))

  if (length(unused) > 0) {
    for(i in 1:length(unused)) {
      assign(unused[i], NULL, envir = parent.frame())
    }
  }

}






#' Function to apply html bold tags to input text strings for use in
#' plotly font definitions.
#'
#' @param x A text string
#'
#' @return A text string wrapped in <b> </b> for html bold font
#'
#' @examples
#' \dontrun{
#' html_bold("Chart Title")
#' }
html_bold <- function(x) {
  x <- paste0("<b>",x,"</b>")
}




#' Function to translate ggplot linetypes into plotly linetypes
#'
#' @param x A text string ggplot line type ("solid", "dotted", "dashed",
#' "longdash", "dotdash")
#'
#' @return A text string plotly line type
#'
#' @examples
#' \dontrun{
#' plotly_line_style("dotdash")
#' }
plotly_line_style <- function(x) {

  # Define valid line types
  valid_line_types <-
      c("solid", "dotted", "dashed", "longdash", "dotdash")

  # resolve line style
  if (!(x %in% valid_line_types)) {

    stop(
      "Invalid line type. Please provide one of the following line types:
             solid, dotted, dashed, longdash, dotdash"
    )

  } else {

    plotly_line_style <- switch(
      x,
      "solid" = "solid",
      "dotted" = "dot",
      "dashed" = "dash",
      "longdash" = "longdash",
      "dotdash" = "dashdot",
      "solid"
    )
  }

  return(plotly_line_style)

}





#' Function to convert ggplot scale_date(date_breaks) values to d3 date
#' format for use in plotly dtick variable for date axis break setting.
#'
#' @param x A string giving the distance between breaks like "2 weeks",
#' or "10 years". Valid specifications are 'sec', 'min', 'hour', 'day',
#' 'week', 'month' or 'year', optionally followed by 's'.
#' See https://ggplot2.tidyverse.org/reference/scale_date.html
#'
#' @return A string in a format appropriate for plotly dtick date break formatting.
#'
#'
#' @examples
#' \dontrun{
#' datebreak_to_d3("2 months")
#'
#' datebreak_to_d3("1 week")
#' }
datebreak_to_d3 <- function(x) {

  # Gather string elements
  number <- stringr::word(x , 1)
  interval <- stringr::word(x , 2)

  # If 'day'/'days' selected; convert to milliseconds
  if(interval %in% c('day', 'days')) {

    return(as.numeric(number)*86400000.0)

  # If 'week'/'weeks' selected; convert to multiples of 7-days in milliseconds
  } else if (interval %in% c('week', 'weeks')) {

    return(as.numeric(number)*7*86400000.0)

  # If 'month'/'months' selected; convert to 'Mxxx' format
  } else if (interval %in% c('month', 'months')) {

    return(paste0("M",number))

  # If 'year'/'years' selected; convert to 'Mxxx' format times 12
  } else if (interval %in% c('year', 'years')) {

    return(paste0("M",as.numeric(number)*12))

  # If 'hour'/'hours' selected; convert to milliseconds
  } else if (interval %in% c('hour', 'hours')) {

    return(as.numeric(number)*60*60*1000)

  # If 'minute'/'minutes' selected; convert to milliseconds
  } else if (interval %in% c('minute', 'minutes')) {

    return(as.numeric(number)*60*1000)

  # If 'second'/'seconds' selected; convert to milliseconds
  } else if (interval %in% c('second', 'seconds')) {

    return(as.numeric(number)*1000)

  }
}
