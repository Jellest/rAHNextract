#'@inheritParams ahn_area
#'@noRd
create_spatialpoint <- function(X, Y, LONLAT = FALSE) {
  point.df <- data.frame(X = X, Y = Y, stringsAsFactors = FALSE)
  if (LONLAT == FALSE) {
    projcrs <- epsg_rd
  } else {
    projcrs <- epsg_wgs
  }
  point.sf <- sf::st_as_sf(x = point.df, coords = c("X", "Y"), crs = projcrs)

  if (LONLAT == TRUE) {
    point.sf <- sf::st_transform(point.sf, epsg_rd)
  }
  return(point.sf)
}