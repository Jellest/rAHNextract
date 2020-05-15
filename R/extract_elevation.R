#'extract elevation raster
#'
#'@title extract elevation
#'@description extract elevation at certain point
#'@param ras Required. Raster
#'@param point Required. spatial point
#'@param extract.method Default 'bilinear'. Choose 'bilinear or 'simple'. Intersection is done using [\code{extract()}](https://www.rdocumentation.org/packages/raster/versions/3.1-5/topics/extract) function from the \code{raster} package.
#'@author Jelle Stuurman
#'@return GeoTIFF file of AHN area
extract_elevation <- function(ras, point, extract.method = "bilinear"){
  print("Intersecting raster. Getting elevation...")
  my_elevation <- raster::extract(ras, point, method = extract.method)
  return (my_elevation)
}
