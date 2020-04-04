#'Get AHN of a certain area
#'
#'@title AHN area
#'@description Get AHN of a certain area
#'ahn_area(name, X, Y, radius, bbox, geom, LONLAT = FALSE, AHN = "AHN3", dem = "DSM", resolution, interpolate = TRUE, decimals = 2, sheets = FALSE, delete.sheets = FALSE, redownload = FALSE)
#'@param ras Required. Raster
#'@param point Required. spatial point
#'@author Jelle Stuurman
#'@return .tif file of AHN area
intersect_raster <- function(ras, point){
  print("Intersecting raster. Getting elevation...")
  my_elevation <- raster::extract(ras, point, method = "bilinear")
  return (my_elevation)
}
