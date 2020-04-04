#'Create a sf spatial point from X/Y or LAT/LON coordinates
#'
#'@title spatial point
#'@description create a sf spatial point in RD New coordinates
#'@param X X coordinate in RD New or WGS84 (LON)
#'@param Y Y coordinate in RD New or WGS84 (LAT)
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will be in RD New format.
#'@author Jelle Stuurman
#'create_spatialpoint(X = , Y = , LONLAT = TRUE)
#'@return sf spatial point in RD New coordinates format
create_spatialpoint <- function(X, Y, LONLAT = FALSE){
  point.df <- data.frame(X = X, Y = Y, stringsAsFactors=FALSE)
  if(LONLAT == FALSE){
    projcrs <- epsg_rd
  } else {
    projcrs <- epsg_wgs
  }
  point.sf <- sf::st_as_sf(x = point.df, coords = c('X', 'Y'), crs = projcrs)

  if(LONLAT == TRUE){
    point.sf <- sf::st_transform(point.sf, epsg_rd)
  }
  return (point.sf)
}
