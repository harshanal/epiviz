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
  assign(chart_font, chart_font, envir = parent.frame())
}

