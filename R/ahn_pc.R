#'@title AHN point clouds area
#'@description Get AHN point clouds of a certain area.
#'
#'AHN Point clouds is retrieved through a circle (X, Y and radius), BBOX (X, Y and radius, OR bbox), or own geometry (polygon). AHN data is obtained from the AHN3 (default), AHN2 or AHN 1 from the available point clouds datasets. AHN3 (default) only has one available datasets. AHN1 and AHN2 include 'gefilterd' or 'uitgefilterd' datasets. See documentation for all available parameters.
#'@param name Optional. Give a name of the specified area. This name will be used in the output file names.
#'@param output.dir Set location of output directory. Default 'AHN_output'. 'AHN_output' saves all the output files in the 'AHN_output' directory created in the working directory. Set to any other output path to save all the output files there. Set to "", to have all output files be temporary files. This output directory excludes the location of the AHN sheets which is depicted with the 'sheets.location' parameter.
#'@param X Required for circle or bbox when no BBOX coordinates are provided. X coordinate in RD New or WGS84 (LON)
#'@param Y Required for circle or bbox when no BBOX coordinates are provided. Y coordinate in RD New or WGS84 (LAT)
#'@param radius Required for circle or bbox when no BBOX coordinates are provided. Set radius in meters of area around a point to create a buffer area (circle).
#'@param bbox Required when using a BBOX. Create BBOX of an area. Use c(XMIN, YMIN, XMAX, YMAX) OR set to TRUE when a radius is provided. Use RD New or WGS84 (LONLAT) coordinates.
#'@param polygon Required when create an area of custom geometry. Use spatial object as your area. .shp, .gpkg. Output will in the shape of this polygon
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'
#'@param gefilterd Default FALSE. Only applicable for AHN1 or AHN2 point cloud data. It depicts if you want to download the 'gefilterd' point cloud data set or the 'uitgefilterd' data set.
#'@param sheets.location Default is the 'AHN_sheets' directory in the working directory. Set directory where all the AHN sheets are loaded when pre-exisiting sheets will be used or when new sheets will be stored. When loading existing files, always use the correct directory structure and capitalization within the selected directory. Example directory structure when this parameter is set to e.g. 'myFolder': 'myFolder/AHN_sheets/AHN3/PC' or 'myFolder/AHN_sheets/AHN2/PC'. Only use extracted files in their original name after download.
#'@param sheets.keep Default TRUE. Set to FALSE if you want to delete the downloaded point clouds sheets (structure). It is recommended to keep the sheets if this function will be used more than once.
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will always be in RD New format
#'@author Jelle Stuurman
#'@return .laz data of area
#'@export
ahn_pc <- function(name = "AHNpointclouds", output.dir = "AHN_output", X, Y, radius, bbox, polygon, AHN = "AHN3", gefilterd = FALSE, LONLAT = FALSE, sheets.location, sheets.keep = TRUE){

  name_trim <- trim_name(name)

  #selected
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
  resolution = ""

  #locate/create output and sheet directories
  if(!dir.exists(output.dir) == TRUE){
    dir.create(output.dir)
    print(paste0("'",output.dir, "' directory was not found and was created."))
    #print(output.dir)
  }
  if(missing(sheets.location) == TRUE){
    sheets.location <- getwd()
    print(paste0("The AHN sheets are loaded from or downloaded in: ", sheets.location, "/", default.sheets.dir, "/", AHN, "/PC"))
  } else {
    #check ahn directory
    pc_sheet_directory <- paste(sheets.location, AHN, "PC", sep="/")
    print(sprintf("The AHN sheets are loaded from or downloaded in: %s. If no AHN sheet in the correct directory or if no correct name of AHN sheet is found, sheet will be downloaded. For first use it is recommended to use the default output directory.", pc_sheet_directory))
  }

  #get amd create area
  ahn_area <- create_area(X = X, Y = Y, radius = radius, bbox = bbox, polygon = polygon, LONLAT = LONLAT, type = "pc")

  #get AHN point clouds
  pc_data <- get_ahn_sheets(name = name_trim, area = ahn_area, type = "pc", AHN = AHN, radius = radius, gefilterd = gefilterd, output.dir = output.dir, sheets.location = sheets.location, sheets.keep = sheets.keep)
  return (pc_data)
}
