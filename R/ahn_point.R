#'@title AHN elevation point
#'@description Get elevation of specific point location.
#'
#'Requires the X and Y coordinates as input. AHN data is obtained from the AHN3 (default), AHN2 or AHN 1 from the available resolutions. Default resolution is always the highest resolution (smallest number).
#'
#'AHN3 and AHN2 DEM (Digital Elevation Models) are available for the Digital Surface Model (DSM) and Digital Terrain Model (DTM). AHN1 only has the DTM. THE DTM of the AHN2 has an interpolated and a non-interpolate version.
#'
#'You can download the AHN data using the WCS method (default, sheets.method = FALSE) returning a GeoTIFF float32 format. The sheets method (sheets.method = TRUE) returns a regular raster GeoTIFF output file that is extracted from the PDOK sheets. The WCS method is recommended if only a few AHN elevation points need to be extracted. The sheets method always requires more data to be downloaded to the client but may be more efficient if many elevatiomns need to be retrieved from a small area. Choosing your method depends on speed and your desired output format. See documentation for all available parameters.
#'@param name Optional. Give a name of the specified area. This name will be used in the output file names.
#'@param X Required. X coordinate in RD New or WGS84 (LON).
#'@param Y Required. Y coordinate in RD New or WGS84 (LAT).
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters for AHN2/AHN3, 5 meters for AHN1. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5 and 100 m.
#'@param interpolate Default TRUE. Only applicable for AHN2 DTM. If true, it gets  the interpolated version of the AHN2.
#'@param output.dir Optional but unnecessary. Set location of output raster files. Leaving blank (default) will make all output point files be temporary files. This output directory excludes the location of the AHN sheets which is depicted with the \code{sheets.location} parameter.
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will be in RD New format.
#'@param extract.method Default 'bilinear'. Choose 'bilinear or 'simple'. Intersection is done using [\code{extract()}](https://www.rdocumentation.org/packages/raster/versions/3.1-5/topics/extract) function from the \code{raster} package.
#'@param decimals Default 2. Decide number of decimal places of output elevations.
#'@param sheets.method Default FALSE. FALSE downloads AHN area through the faster WCS method. Output is 32float GeoTIFF file.TRUE downloads AHN area through the available GeoTIFF AHN sheets available on [PDOK](http://esrinl-content.maps.arcgis.com/apps/Embed/index.html?appid=a3dfa5a818174aa787392e461c80f781).
#'@param sheets.location Default is the 'AHN_sheets' directory in the working directory. Set directory where all the AHN sheets are loaded when pre-existing sheets will be used or when new sheets will be stored. When loading existing files, always use the correct directory structure and capitalization within the selected directory. Example directory structure when this parameter is set to e.g. 'myFolder': 'myFolder/AHN_sheets/AHN3/DSM' or 'myFolder/AHN_sheets/AHN2/DTM'. Only use extracted files in their original name after download.
#'@param sheets.keep Default TRUE. Only applicable if \code{sheets.method} is set to TRUE and sheets were downloaded. Set to FALSE if you want to delete the downloaded sheet. It is recommended to keep the sheets if ahn elevation extarction will be followed.
#'@author Jelle Stuurman
#'@return AHN elevation in meters.
#'@export
ahn_point <- function(name = "AHNelevation", X, Y, AHN = "AHN3", dem = "DSM", resolution = 0.5, interpolate = TRUE, output.dir, LONLAT = FALSE, extract.method = "bilinear", decimals = 2, sheets.method = FALSE, sheets.location, sheets.keep = TRUE){
  loadNamespace("raster")
  name_trim <- trim_name(name)

  #set tmp folder if applicable or create output and directory
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
    stop("No correct AHN is provided. Please select 'AHN3' or 'AHN2'")
  } else {
    AHN <- toupper(AHN)
  }

  #set missing resolution
  if(missing(resolution) == TRUE){
    resolution = ""
  }

  #get and create a point
  my_point <- generate_ahn_point(name = name_trim, X = X, Y = Y, LONLAT = LONLAT, resolution = resolution)

  #get AHN elevation
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

    #create area
    ahn_area <- create_area(bbox = my_point$bbox, LONLAT = LONLAT, type = "point")

    #get AHN area
    raster_data <- get_ahn_sheets(name = name_trim, area = ahn_area, type = "point", AHN = AHN, dem = dem, resolution = resolution, radius = "", interpolate = interpolate, output.dir = output.dir, sheets.keep = sheets.keep, sheets.location = sheets.location)

    #get elevation
    my_elevation <- extract_elevation(raster_data$data, my_point$point, extract.method = extract.method)
  } else {
    #retrieve data through WCS (fast)
    my_resolution <- get_resolution(AHN = AHN, resolution = resolution)

    my_url <- create_wcs_url(type = "point", bbox = my_point$bbox, AHN = AHN, dem = dem, resolution = my_resolution, interpolate = interpolate)
    raster_data <- download_wcs_raster(wcsUrl = my_url, name = name_trim, AHN = AHN, dem = tolower(dem), resolution = my_resolution, output.dir = output.dir, interpolate = interpolate, type = "point")
    my_elevation <- extract_elevation(raster_data$data, my_point$point, extract.method = extract.method)
  }

  my_elevation <- format(round(my_elevation, decimals), nsmall = decimals)
  print(paste("Elevation of ", name , ": ", my_elevation, " m.", sep=""))
  my_elevation <- as.numeric(my_elevation)
  # if(output.dir == tempdir()){
  #   unlink(raster_data$fileName)
  # }
  return (my_elevation)
}
