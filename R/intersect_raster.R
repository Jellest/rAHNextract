#'intersect raster
#'
#'@title intersect raster
#'@description intersect raster at certain point
#'@param ras Required. Raster
#'@param point Required. spatial point
#'@param method 'bilinear' (default) or 'simple'
#'@author Jelle Stuurman
#'@return GeoTIFF file of AHN area
intersect_raster <- function(ras, point, method = "bilinear"){
  print("Intersecting raster. Getting elevation...")
  my_elevation <- raster::extract(ras, point, method = method)
  return (my_elevation)
}
