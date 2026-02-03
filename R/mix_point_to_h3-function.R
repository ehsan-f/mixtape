#' Convert geographic points to H3 hexagon indices
#'
#' @description
#' Converts geographic points (longitude/latitude) to H3 hexagon indices at a specified resolution.
#'
#' @param df Data frame containing geographic coordinates
#' @param long Column name for longitude values
#' @param lat Column name for latitude values
#' @param h3_res H3 resolution level
#'
#' @export
mix_point_to_h3 <- function(df, long, lat, h3_res) {
  library(sf)
  library(h3jsr)

  #-- Create points
  xy_point <- st_multipoint(x = as.matrix(df[,c(long, lat)]))

  #-- H3 hex
  h3_hex <- point_to_cell(input = as.matrix(xy_point), res = h3_res)

  #-- Output
  return(h3_hex)
}
