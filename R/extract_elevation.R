#'@title extract AHN elevation (m) at specified point location.
#'@description Extract the elevation value (in meters) from the AHN.
#'@param ras Spatial raster on which the elevation will be extracted
#'@param point Spatial point from where the elevation will be extracted
#'@inheritParams ahn_point
#'@noRd
extract_elevation <- function(ras, point, extract.method = "simple") {
  print("Intersecting raster. Getting elevation...")
  my_elevation <- terra::extract(x = ras, y = point, method = extract.method, ID = FALSE)
  return(my_elevation)
}
