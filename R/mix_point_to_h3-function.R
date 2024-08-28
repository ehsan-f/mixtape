#' @export
mix_point_to_h3 <- function(df, long, lat, h3_res) {
  #-- Packages
  library(sf)
  library(h3jsr)

  #-- Create points
  xy_point <- st_multipoint(x = as.matrix(df[,c(long, lat)]))

  #-- H3 hex
  h3_hex <- point_to_cell(input = as.matrix(xy_point), res = h3_res)

  #-- Output
  return(h3_hex)
}
