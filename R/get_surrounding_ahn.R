#'Get buffer surrounding AHN
#'
#'@title Get buffer surrounding AHN
#'@description Get buffer surrounding AHN
#'@param X X coordidnate in RD New or WGS84 (LON)
#'@param Y Y coordidnate in RD New or WGS84 (LAT)
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will be in RD New format.
#'@param radius Optional. Set radius in meters of area around a point to create a buffer area.
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5, 25, and 100 m.
#'@author Jelle Stuurman
#'ahn_area(name, X, Y, radius, bbox, geom, LONLAT = FALSE, AHN = "AHN3", dem = "dsm", resolution, interpolate = TRUE, decimals = 2, sheets = FALSE, delete.sheets = FALSE, redownload = FALSE)
#'@return BBOX of buffer area
get_surrounding_ahn <- function(X, Y, LONLAT = FALSE, AHN = "AHN3", dem = dem, resolution = resolution, radius = 50){

  #create point
  spatialpoint <- create_spatialpoint(X = X, Y = Y, LONLAT = LONLAT)

  surroundingBuffer <- sf::st_buffer(spatialpoint,dist=radius)

  #get BBOX extent of buffer area
  surroundingExtent <- raster::extent(surroundingBuffer)
  my_bbox <- paste(toString(surroundingExtent@my_xmin), toString(surroundingExtent@my_ymin), toString(surroundingExtent@my_xmax), toString(surroundingExtent@my_ymax), sep=",")

  return(my_bbox)
}
