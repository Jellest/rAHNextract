#'@title AHN raster area
#'@description Get AHN raster area of a specified area.
#'
#'AHN Area is retrieved through a circle (x, y and radius), BBOX (x, y and radius, OR bbox), or own geometry (polygon).
#'
#'AHN data is obtained from the AHN3 (default), AHN2 or AHN 1 from the available resolutions.
#'
#'Default resolution is always the highest resolution (smallest number). AHN3 and AHN2 DEM (Digital Elevation Model) are available for the Digital Surface Model (DSM) and Digital Terrain Model (DTM). AHN1 only has the DTM. THE DTM of the AHN2 has an interpolated and a non-interpolate version.
#'
#'You can download the AHN data using the WCS method (default, \code{sheets.method = FALSE}) returning a GeoTIFF float32 format. The sheets method (\code{sheets.method = TRUE}) returns a regular raster GeoTIFF output file that is extracted from the PDOK sheets. The WCS method is recommended if only a few AHN elevation points need to be extracted. The sheets method always requires more data to be downloaded to the client but may be more efficient if many elevatiomns need to be retrieved from a small area. Choosing your method depends on speed and your desired output format. See documentation for all available parameters.
#'@param name Optional. Give a name of the specified area. This name will be used in the output file names.
#'@param output.dir Optional but recommended. Set location of output directory. Leaving blank (default) will make all output point files be temporary files. This output directory excludes the location of the AHN sheets which is depicted with the \code{sheets.location} parameter.
#'@param X Required for circle or BBOX when no BBOX coordinates are provided. X coordinate in RD New or WGS84 (LON)
#'@param Y Required for circle or BBOX when no BBOX coordinates are provided. Y coordinate in RD New or WGS84 (LAT)
#'@param radius Required for circle or BBOX when no BBOX coordinates are provided. Set radius in meters of area around a point to create a buffer area (circle).
#'@param bbox Required when using a BBOX. Create BBOX of an area. Use c(XMIN, YMIN, XMAX, YMAX) OR set to TRUE when a radius is provided. Use RD New or WGS84 (LONLAT) coordinates.
#'@param polygon Required when create an area of custom geometry. Use spatial object as your area. .shp, .gpkg. Output will in the shape of this polygon
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'
#'@param dem Required for raster datasets. Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters for AHN2/AHN3, 5 meters for AHN1. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5, and 100 m.
#'@param interpolate Default TRUE. Only applicable for AHN2 DTM. If true, it gets  the interpolated version of the AHN2.
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will always be in RD New format
#'@param decimals Default 2. Decide number of decimal places of output elevations.
#'@param sheets.method Default FALSE. FALSE downloads AHN area through the faster WCS method. Output is 32float GeoTIFF file.TRUE downloads AHN area through the available GeoTIFF AHN sheets available on [PDOK](http://esrinl-content.maps.arcgis.com/apps/Embed/index.html?appid=a3dfa5a818174aa787392e461c80f781).
#'@param sheets.location Optional. Default is the 'AHN_sheets' directory in the working directory. Set directory where all the AHN sheets are loaded when pre-existing sheets will be used or when new sheets will be stored. When loading existing files, always use the correct directory structure and capitalization within the selected directory. Example directory structure when this parameter is set to e.g. 'myFolder': 'myFolder/AHN_sheets/AHN3/DSM' or 'myFolder/AHN_sheets/AHN2/DTM'. Only use extracted files in their original name after download.
#'@param sheets.keep Default TRUE. Only applicable if \code{sheets.method} is set to TRUE and sheets were downloaded. Set to FALSE if you want to delete the downloaded sheet. It is recommended to keep the sheets if ahn elevation extarction will be followed.
#'@author Jelle Stuurman
#'@return GeoTIFF file of AHN area
#'@export
ahn_area <- function(name = "AHNarea", output.dir, X, Y, radius, bbox, polygon, AHN = "AHN3", dem = "DSM", resolution, interpolate = TRUE, LONLAT = FALSE, decimals = 2, sheets.method = FALSE, sheets.location, sheets.keep = TRUE, sheets.redownload = FALSE){
  #set tmp folder if applicable or create output and directory
  loadNamespace("raster")
  name_trim <- trim_name(name)

  #set tmp folder if applicable or create output directory
  if(missing(output.dir) == TRUE){
    output.dir <- tempdir()
  } else {
    if(!dir.exists(output.dir) == TRUE){
      dir.create(output.dir)
      print(paste0("'",output.dir, "' directory was not found and was created."))
    }
    #print(output.dir)
  }

  #selected AHN
  if(tolower(AHN) != "ahn1" && tolower(AHN) != "ahn2" && tolower(AHN) != "ahn3"){
    stop("No correct AHN is provided. Please select 'AHN1', 'AHN2' or 'AHN3'.")
  } else {
    AHN <- toupper(AHN)
  }

  #complete geometry parameters
  if(missing(X) == TRUE && missing(Y) == TRUE && (missing(polygon) == TRUE || missing(bbox) == TRUE) && missing(radius) == TRUE){
    #creating BBOX or shape
    radius <- ""
  }

  #set resoluition if missing
  if(missing(resolution) == TRUE){
    resolution = ""
  }

  #get and create area
  ahn_area <- create_area(X = X, Y = Y, radius = radius, bbox = bbox, polygon = polygon, LONLAT = LONLAT, type = "raster")

  #get AHN data
  if(sheets.method == TRUE){
    #download AHN sheets and get data (slow)

    #set AHN sheets location
    if(missing(sheets.location) == TRUE){
      sheets.location <- getwd()
      print(paste0("The AHN sheets are loaded from or downloaded in: ", sheets.location, "/", default.sheets.dir, "/", AHN, "/", dem))
    } else {
      ahn_sheet_directory <- paste(sheets.location, AHN, dem, sep="/")
      print(sprintf("The AHN sheets are loaded from or downloaded in: %s. If no AHN sheet in the correct directory or if no correct name of AHN sheet is found, sheet will be downloaded. For first use it is recommended to use the default output directory.", ahn_sheet_directory))
    }

    #get AHN data
    raster_data <- get_ahn_sheets(name = name_trim, area = ahn_area, type = "area", AHN = AHN, dem = dem, resolution = resolution, radius = radius, interpolate = interpolate, output.dir = output.dir, sheets.location = sheets.location, sheets.keep = sheets.keep, sheets.redownload = sheets.redownload)
    ahn_data <- raster_data$data
  } else {
    my_resolution <- get_resolution(AHN = AHN, resolution = resolution)

    wcs_url <- create_wcs_url(bbox = ahn_area$bbox, type = "area", AHN = AHN, resolution = my_resolution, dem = dem, interpolate = interpolate)
    raster_data <- download_wcs_raster(wcsUrl = wcs_url, name = name_trim, AHN = AHN, dem = tolower(dem), radius = radius, resolution = my_resolution, interpolate = interpolate, output.dir = output.dir, type = "raster")
    ahn_data <- raster::mask(x = raster_data$data, mask = ahn_area$area, filename = raster_data$fileName, overwrite = TRUE)
  }
  if(LONLAT == TRUE){
    warning("The input geometry was provided using Longitude and Latitude coordinates. The output, however, is a raster using the the RD New cordinate system.")
  }
  # if(output.dir == tempdir()){
  #   unlink(raster_data$fileName)
  # }
  return (ahn_data)
}
