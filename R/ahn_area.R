#'Get AHN of a certain area
#'
#'@title AHN area
#'@description Get AHN of a certain area
#'@param name Optional. Give a name of the specified area.
#'@param X X coordidnate in RD New or WGS84 (LON)
#'@param Y Y coordidnate in RD New or WGS84 (LAT)
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will be in RD New format.
#'@param radius Optional. Set radius in meters of area around a point to create a buffer area.
#'@param bbox Optional. Set bbox of area. c(XMIN, YMIN, XMAX, YMAX)
#'@param geom Optional. Use geometric object as your area. .shp, .gpkg. Output will be BBox of this object.
#'@param type Select ; 'raster' or 'pc'
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5, 25, and 100 m.
#'@param interpolate Default TRUE. Olny applicable for AHN2 DTM. It decides if you want the interpolated version of the AHN2 or not.
#'@param decimals Default 2. Decide number of decimal places of output elevations.
#'@param sheets Default FALSE. Set to TRUE if you want to download AHN areas through the sheets (kaartbladen) instead through the WCS method (geotiff 32bit float
#'@param delete.sheets Deault TRUE. Only applicable if sheets is set to TRUE. Set to FALSE if you want to keep the downloaded sheets (kaartbladen).
#'@param redownload Deafult FALSE. nly applicable if sheets is set to TRUE. Set to TRUE if you want to redownload the sheets (kaartbladen)
#'@author Jelle Stuurman
#'ahn_area(name, X, Y, radius, bbox, geom, LONLAT = FALSE, AHN = "AHN3", dem = "dsm", resolution, interpolate = TRUE, decimals = 2, sheets = FALSE, delete.sheets = FALSE, redownload = FALSE)
#'@return .tif file of AHN area
#'@export

ahn_area <- function(name, X, Y, radius, bbox, geom, LONLAT = FALSE, type, AHN = "AHN3", dem = "dsm", resolution, interpolate = TRUE, decimals = 2, sheets = FALSE, delete.sheets = FALSE, redownload = FALSE){
  name_trim <- trim_name(name)
  #selected AHN layer
  ahn_lower <- tolower(AHN)
  if(ahn_lower != "ahn1" && ahn_lower != "ahn2" && ahn_lower != "ahn3"){
    stop("No correct AHN is provided. Please select 'AHN1', 'AHN2' or 'AHN3'.")
  } else {
    my_ahn <- toupper(AHN)
  }

  ahn_area <- create_area(X = X, Y = Y, radius = radius, bbox = bbox, geom = geom, LONLAT = LONLAT, sheets = sheets)
  if(sheets == TRUE || type == "pc"){
    #download AHN sheets and get data (slow)
    ahn_data <- get_ahn_sheets(name = name_trim, area = ahn_area, type = type, AHN = my_ahn, dem = dem, resolution = resolution, interpolate = interpolate, delete.sheets = delete.sheets, redownload = redownload)
  } else {
    #retrieve data through WCS (fast)
    wcs_url <- create_wcs_url(bbox = ahn_area$bbox, type = "area", AHN = my_ahn, resolution = resolution, dem = dem, interpolate = interpolate)
    raster_data <- download_wcs_raster(wcsUrl = wcs_url, name = name_trim)
    raster_mask <- raster::mask(x = raster_data$raster, mask = ahn_area$area, filename = raster_data$file, overwrite = TRUE)
    ahn_data <- raster_mask
  }

  return (ahn_data)
}
