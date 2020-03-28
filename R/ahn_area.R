#'Get AHN of a certain area
#'
#'@title AHN area
#'@description Get AHN of a certain area
#'@param X Optional. X Coordidnate in RD New or WGS84 (LON)
#'@param Y Optional. Y Coordidnate in RD New or WGS84 (LAT)
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will always be in RD New format
#'@param radius Optional. Set radius in meters of area around a point to create a buffer area (circle).
#'@param bbox Optional. Set bbox of area. c(XMIN, YMIN, XMAX, YMAX) in RD New or WGS84 (LONLAT)
#'@param polygon Optional. Use spatial object as your area. .shp, .gpkg. Output will in the shape of this polygon
#'@param name Optional. Give a name of the specified area. Name will used in the output folders and files
#'@param type Default 'raster'. Select 'raster' to get raster data
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters for AHN2/AHN3, 5 meters fpor AHN1. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5, 25, and 100 m.
#'@param interpolate Default TRUE. Only applicable for AHN2 DTM. It decides if you want the interpolated version of the AHN2 or not.
#'@param filtered Default FALSE. Only applicable for AHN1 or AHN2 point cloud data. It decides if you want to download the 'gefiltered' point cloud data set or the 'uitgefiltered' data set.
#'@param decimals Default 2. Decide number of decimal places of output elevations.
#'@param keep.ahn Deault TRUE. Set to FALSE if you want to delete the downloaded AHN area of the point (9 pixels).
#'@param sheets Default FALSE. Set to TRUE if you want to download AHN areas through the sheets (kaartbladen) instead through the WCS method (geotiff 32bit float
#'@param delete.sheets Default TRUE. Only applicable if sheets is set to TRUE. Set to FALSE if you want to keep the downloaded sheets (kaartbladen).
#'@param redownload Default FALSE. Only applicable if sheets is set to TRUE. Set to TRUE if you want to redownload the sheets (kaartbladen)
#'@author Jelle Stuurman
#'ahn_area(name, X, Y, radius, bbox, polygon, LONLAT = FALSE, AHN = "AHN3", dem = "dsm", resolution, interpolate = TRUE, decimals = 2, sheets = FALSE, delete.sheets = FALSE, redownload = FALSE)
#'@return .tif file of AHN area
#'@export

ahn_area <- function(X, Y, radius, bbox, polygon, name, LONLAT = FALSE, type = "raster", AHN = "AHN3", dem = "dsm", resolution, interpolate = TRUE, filtered = FALSE, decimals = 2, keep.ahn = FALSE, sheets = FALSE, delete.sheets = FALSE, redownload = FALSE){
  name_trim <- trim_name(name)
  #selected AHN layer
  ahn_lower <- tolower(AHN)
  if(ahn_lower != "ahn1" && ahn_lower != "ahn2" && ahn_lower != "ahn3"){
    stop("No correct AHN is provided. Please select 'AHN1', 'AHN2' or 'AHN3'.")
  } else {
    my_ahn <- toupper(AHN)
  }

  ahn_area <- create_area(X = X, Y = Y, radius = radius, bbox = bbox, polygon = polygon, LONLAT = LONLAT, sheets = sheets)
  if(sheets == TRUE || type == "pc"){
    #download AHN sheets and get data (slow)
    ahn_data <- get_ahn_sheets(name = name_trim, area = ahn_area, type = type, AHN = my_ahn, dem = dem, resolution = resolution, filtered = filtered, delete.sheets = delete.sheets, redownload = redownload)
  } else {
    #retrieve data through WCS (fast)
    wcs_url <- create_wcs_url(bbox = ahn_area$bbox, type = "area", AHN = my_ahn, resolution = resolution, dem = dem, interpolate = interpolate)
    raster_data <- download_wcs_raster(wcsUrl = wcs_url, name = name_trim, AHN = AHN, dem = tolower(dem), resolution = resolution, interpolate = interpolate, keep.ahn = keep.ahn)
    raster_mask <- raster::mask(x = raster_data$raster, mask = ahn_area$area, filename = raster_data$file, overwrite = TRUE)
    ahn_data <- raster_mask
    if(keep.ahn == FALSE){
      unlink(raster_data$file)
    }
  }
  return (ahn_data)
}
