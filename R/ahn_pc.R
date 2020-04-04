#'Get AHN point clouds of a certain area
#'
#'@title AHN point clouds
#'@description Get AHN point clouds of a certain area
#'@param X Optional. X coordinate in RD New or WGS84 (LON)
#'@param Y Optional. Y coordinate in RD New or WGS84 (LAT)
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will always be in RD New format
#'@param radius Optional. Set radius in meters of area around a point to create a buffer area (circle).
#'@param bbox Optional. Set bbox of area. c(XMIN, YMIN, XMAX, YMAX) in RD New or WGS84 (LONLAT)
#'@param polygon Optional. Use spatial object as your area. .shp, .gpkg. Output will in the shape of this polygon
#'@param name Optional. Give a name of the specified area. Name will used in the folders and files
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'
#'@param gefilterd Default FALSE. Only applicable for AHN1 or AHN2 point cloud data. It decides if you want to download the 'gefilterd' point cloud data set or the 'uitgefilterd' data set.
#'@param destfile Default "structured". When set to "structured", all the output files will be saved in an organized way in the folder 'AHN_output' created in your current working directory. This included the AHN sheets. Set to 'Structured' to save  Set to any other output path to save all the output files there. The AHN sheets will be downloaded in the output folder 'AHN_sheets" in your working directory. Set to "", to have all output files be temporary files except for the AHN sheets which are downloaded in the 'AHN_sheets' folder.geotif 32bit float).
#'@param keep.sheets Default TRUE. Set to FALSE if you want to delete the downloaded point clouds sheets (kaartbladen).
#'@param redownload Default FALSE. Only applicable if point clouds sheets is set to TRUE. Set to TRUE if you want to redownload the sheets (kaartbladen)
#'@author Jelle Stuurman
#'@return .laz data of area
#'@export
ahn_pc <- function(X, Y, radius, bbox, polygon, name, LONLAT = FALSE, AHN = "AHN3", gefilterd = FALSE, destfile = "structured", keep.sheets = TRUE, redownload = FALSE){
  resolution = ""
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

  ahn_area <- create_area(X = X, Y = Y, radius = radius, bbox = bbox, polygon = polygon, LONLAT = LONLAT, type = "pc")
  data <- get_ahn_sheets(name = name_trim, area = ahn_area, type = "pc", AHN = my_ahn, radius = radius, gefilterd = gefilterd, destfile = destfile, keep.sheets = keep.sheets, redownload = redownload)
  if(destfile == ""){
    unlink(data$fileDir, recursive = TRUE)
  }
  ahn_data <- data$data
  return (ahn_data)
}
