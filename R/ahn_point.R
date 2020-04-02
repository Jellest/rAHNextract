#'Get AHN elevation of a certain point location
#'
#'@title Get AHN elevation of a certain point location
#'@description Get elevation of specific point location.
#'@param X X coordinate in RD New or WGS84 (LON)
#'@param Y Y coordinate in RD New or WGS84 (LAT)
#'@param name Optional. Give a name of the specified area.
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will be in RD New format.
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5, 25, and 100 m.
#'@param decimals Default 2. Decide number of decimal places of output elevations.
#'@param interpolate Default TRUE. Only applicable for AHN2 DTM. It decides if you want the interpolated version of the AHN2 or not.
#'@param destfile Default "". All output point files be temporary files.When set to "structured", all the output point files will be saved in an organized way in the folder 'AHN_output' created in your current working directory. This included the AHN sheets. Set to any other output path to save all the output files there. The AHN sheets will be downloaded in the output folder 'AHN_sheets" in your working directory.
#'@param method.sheets Default FALSE. FALSE downloads AHN area through the faster WCS method. Output is 32float geotif file. TRUE downloads AHN area through the available .tif AHN sheets (kaartbladen) available on PDOK.
#'@param keep.sheets Default TRUE. Only applicable if method.sheets is set to TRUE. Set to FALSE if you want to delete the downloaded sheets (kaartbladen).
#'@param redownload Default FALSE. Only applicable if sheets is set to TRUE. Set to TRUE if you want to redownload the sheets (kaartbladen)
#'@author Jelle Stuurman
#'@return elevation in meters.
#'@export

ahn_point <- function(X, Y, name, LONLAT = FALSE, AHN = "AHN3", dem = "DSM", resolution = 0.5, destfile = "", decimals = 2, interpolate = TRUE, method.sheets = FALSE, keep.sheets = TRUE, redownload = FALSE){
  if(destfile != "structured" && destfile != ""){
    print(destfile)
    if(!dir.exists(destfile)){
      dir.create(destfile)
      warning("Directory did not exist and was created in your working directory.")
    }
  }

  name_trim <- trim_name(name)
  #selected AHN layer
  ahn_lower <- tolower(AHN)
  if(ahn_lower != "ahn1" && ahn_lower != "ahn2" && ahn_lower != "ahn3"){
    stop("No correct AHN is provided. Please select 'AHN3' or 'AHN2'")
  } else {
    my_ahn <- toupper(AHN)
  }
  my_point <- generate_ahn_point(name = name_trim, X = X, Y = Y, LONLAT = LONLAT, resolution = resolution)
  if(method.sheets == TRUE){
    ahn_area <- create_area(bbox = my_point$bbox, LONLAT = LONLAT, type = "point")
    raster_data <- get_ahn_sheets(name = name_trim, area = ahn_area, type = "raster", AHN = my_ahn, dem = dem, resolution = resolution, radius = "", interpolate = interpolate, destfile = destfile, keep.sheets = keep.sheets, redownload = redownload)
    if(destfile == ""){
      unlink(raster_data$fileDir, recursive = TRUE)
    }
    my_elevation <- intersect_raster(raster_data$data, my_point$point)
  } else {
    my_url <- create_wcs_url(type = "point", bbox = my_point$bbox, AHN = my_ahn, dem = dem, resolution = resolution, interpolate = interpolate)
    raster_data <- download_wcs_raster(wcsUrl = my_url, name = name_trim, AHN = AHN, dem = tolower(dem), resolution = resolution, destfile = destfile, interpolate = interpolate, type = "point")
    my_elevation <- intersect_raster(raster_data$data, my_point$point)
  }

  my_elevation <- format(round(my_elevation, decimals), nsmall = decimals)
  print(paste("Elevation of ", name , ": ", my_elevation, " m.", sep=""))
  my_elevation <- as.numeric(my_elevation)
  if(destfile == ""){
    unlink(raster_data$fileName)
  }
  return (my_elevation)
}
