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
