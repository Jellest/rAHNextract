#'Get AHN of a certain area
#'
#'@title AHN area
#'@description Get AHN of a certain area
#'@param X Optional. X coordinate in RD New or WGS84 (LON)
#'@param Y Optional. Y coordinate in RD New or WGS84 (LAT)
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will always be in RD New format
#'@param radius Optional. Set radius in meters of area around a point to create a buffer area (circle).
#'@param bbox Optional. Set bbox of area. c(XMIN, YMIN, XMAX, YMAX) in RD New or WGS84 (LONLAT)
#'@param polygon Optional. Use spatial object as your area. .shp, .gpkg. Output will in the shape of this polygon
#'@param name Optional. Give a name of the specified area. Name will used in the gefilterd folders and files
#'@param type Default 'raster'. Select 'raster' to get raster data
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'
#'@param dem Required for raster datasets. Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters for AHN2/AHN3, 5 meters for AHN1. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5, and 100 m.
#'@param interpolate Default TRUE. Only applicable for AHN2 DTM. It decides if you want the interpolated version of the AHN2 or not.
#'@param gefilterd Default FALSE. Only applicable for AHN1 or AHN2 point cloud data. It decides if you want to download the 'gefilterd' point cloud data set or the 'uitgefilterd' data set.
#'@param decimals Default 2. Decide number of decimal places of gefilterd elevations.
#'@param destfile Default "structured". When set to "structured", all the gefilterd files will be saved in an organized way in the folder 'AHN_gefilterd' created in your current working directory. This included the AHN sheets. Set to 'Structured' to save  Set to any other gefilterd path to save all the gefilterd files there. The AHN sheets will be downloaded in the gefilterd folder 'AHN_sheets" in your working directory. Set to "", to have all gefilterd files be temporary files except for the AHN sheets which are downloaded in the 'AHN_sheets' folder.#'@param method.sheets Default FALSE. Set to TRUE if you want to download AHN areas through the sheets (kaartbladen) instead through the WCS method (geotif 32bit float
#'@param method.sheets Only applicable for raster data. Default FALSE. FALSE downloads AHN area through the faster WCS method. Output is 32float geotif file. TRUE downloads AHN area through the available .tiff AHN sheets (kaartbladen) available on PDOK.
#'@param keep.sheets Default TRUE. Only applicable if method.sheets is set to TRUE. Set to FALSE if you want to delete the downloaded sheets (kaartbladen).
#'@param redownload Default FALSE. Only applicable if sheets is set to TRUE. Set to TRUE if you want to redownload the sheets (kaartbladen)
#'@author Jelle Stuurman
#'@return .tif file of AHN area
#'@export

ahn_area <- function(X, Y, radius, bbox, polygon, name, LONLAT = FALSE, AHN = "AHN3", dem = "DSM", type =  "raster", resolution, interpolate = TRUE, gefilterd = FALSE, decimals = 2, destfile = "structured", method.sheets = FALSE, keep.sheets = TRUE, redownload = FALSE){
  if(destfile != "structured" && destfile != ""){
    print(destfile)
    if(!dir.exists(destfile)){
      dir.create(destfile)
      warning("Directory did not exist and was created in your working directory.")
    }
  }
  if(missing(X) == TRUE && missing(Y) == TRUE && (missing(polygon) == TRUE || missing(bbox) == TRUE) && missing(radius) == TRUE){
    #creating BBOX or shape
    radius <- ""
  }
  # if(missing(dem) == TRUE){
  #   type = "pc"
  # } else {
  #   type = "raster"
  # }
  name_trim <- trim_name(name)
  #selected AHN layer
  ahn_lower <- tolower(AHN)
  if(ahn_lower != "ahn1" && ahn_lower != "ahn2" && ahn_lower != "ahn3"){
    stop("No correct AHN is provided. Please select 'AHN1', 'AHN2' or 'AHN3'.")
  } else {
    my_ahn <- toupper(AHN)
  }

  ahn_area <- create_area(X = X, Y = Y, radius = radius, bbox = bbox, polygon = polygon, LONLAT = LONLAT)
  if(method.sheets == TRUE || type == "pc"){
    #download AHN sheets and get data (slow)
    data <- get_ahn_sheets(name = name_trim, area = ahn_area, type = type, AHN = my_ahn, dem = dem, resolution = resolution, radius = radius, interpolate = interpolate, gefilterd = gefilterd, destfile = destfile, keep.sheets = keep.sheets, redownload = redownload)
    if(destfile == ""){
      unlink(data$fileDir, recursive = TRUE)
    }
    ahn_data <- data$data
  } else {
    #retrieve data through WCS (fast)
    wcs_url <- create_wcs_url(bbox = ahn_area$bbox, type = "area", AHN = my_ahn, resolution = resolution, dem = dem, interpolate = interpolate)
    raster_data <- download_wcs_raster(wcsUrl = wcs_url, name = name_trim, AHN = AHN, dem = tolower(dem), radius = radius, resolution = resolution, interpolate = interpolate, destfile = destfile)
    raster_mask <- raster::mask(x = raster_data$data, mask = ahn_area$area, filename = raster_data$fileName, overwrite = TRUE)
    ahn_data <- raster_mask
    if(destfile == ""){
      unlink(raster_data$fileDir)
    }
  }
  return (ahn_data)
}
