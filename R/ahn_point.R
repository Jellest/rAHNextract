#'Get AHN elvation of a certain point location
#'
#'@title Get AHN elvation of a certain point location
#'@description Get elevation of specific point location.
#'#'@param X X coordidnate in RD New or WGS84 (LON)
#'@param Y Y coordidnate in RD New or WGS84 (LAT)
#'@param name Optional. Give a name of the specified area.
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will be in RD New format.
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5, 25, and 100 m.
#'@param decimals Default 2. Decide number of decimal places of output elevations.
#'@param interpolate Default TRUE. Ony applicable for AHN2 DTM. It decides if you want the interpolated version of the AHN2 or not.
#'@param delete.ahn Deault TRUE. Set to FALSE if you want to keep the downloaded AHN area of the point (9 pixels).
#'@author Jelle Stuurman
#'ahn_point(name, X, Y, LONLAT = FALSE, AHN = "AHN3", dem = "dsm", resolution = 0.5, delete.ahn = TRUE, decimals = 2, interpolate = TRUE)
#'@return elevation in meters.
#'@export

ahn_point <- function(X, Y, name, LONLAT = FALSE, AHN = "AHN3", dem = "dsm", resolution = 0.5, delete.ahn = TRUE, decimals = 2, interpolate = TRUE){
  name_trim <- trim_name(name)
  #selected AHN layer
  ahn_lower <- tolower(AHN)
  if(ahn_lower != "ahn1" && ahn_lower != "ahn2" && ahn_lower != "ahn3"){
    stop("No correct AHN is provided. Please select 'AHN3' or 'AHN2'")
  } else {
    my_ahn <- toupper(AHN)
  }

  my_point <- generate_ahn_point(name = name_trim, X = X, Y = Y, LONLAT = LONLAT, resolution = resolution)
  my_url <- create_wcs_url(bbox = my_point$bbox, type = "point", AHN = my_ahn, dem = dem, resolution = resolution, interpolate = interpolate)
  my_raster <- download_wcs_raster(wcsUrl = my_url, name = name_trim)

  my_elevation <- intersect_raster(my_raster$raster, my_point$point)

  if(delete.ahn == TRUE){
    file.remove(my_raster$file)
  }
  my_elevation <- format(round(my_elevation, decimals), nsmall = decimals)
  print(paste("Elevation of", name , ":", my_elevation, "m.", sep=" "))
  my_elevation <- as.numeric(my_elevation)
  return (my_elevation)
}
