#'Get cropped AHN sheets
#'
#'@title Get cropped AHN sheets
#'@description Get cropped AHN sheets
#'@param name Optional. Give a name of the specified area.
#'@param type 'raster' or 'pc'.
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5, 25, and 100 m.
#'@param area Required area to be downloaded
#'@param interpolate Default TRUE. Olny applicable for AHN2 DTM. It decides if you want the interpolated version of the AHN2 or not.
#'@param delete.sheets Deault TRUE. Only applicable if sheets is set to TRUE. Set to FALSE if you want to keep the downloaded sheets (kaartbladen).
#'@param redownload Deafult FALSE. nly applicable if sheets is set to TRUE. Set to TRUE if you want to redownload the sheets (kaartbladen)
#'@author Jelle Stuurman
#'get_ahn_sheets(name, area, AHN = "AHN3", resolution = 0.5, dem = "dsm", interpolate = TRUE, redownload = FALSE, delete.sheets = TRUE)
#'@return .tif AHN kaartblad cropped to area

get_ahn_sheets <- function(name, area, type, AHN = "AHN3", resolution = 0.5, dem = "dsm", interpolate = TRUE, redownload = FALSE, delete.sheets = TRUE){
  ###BladIndex method ###
  #get AHN bladIndex
  outputDirectory <- paste("output")

  if (!dir.exists(outputDirectory)){
    dir.create(outputDirectory)
  }

  directory <- paste(outputDirectory, AHN, sep="/")
  if (!dir.exists(directory)){
    dir.create(directory)
  }

  dir.create(paste(directory, name, sep="/"), showWarnings = FALSE)

  #bladIndex.sf <- download_bladnrs(wd = directory, AHN = AHN)
  if(tolower(AHN) == "ahn1"){
    bladIndex.sf <- ahn1_bladIndex
  } else if(tolower(AHN) == "ahn2"){
    bladIndex.sf <- ahn2_bladIndex
  } else if(tolower(AHN) == "ahn3"){
    bladIndex.sf <- ahn3_bladIndex
  }
  shape_area <- area$area
  shape_area <- sf::st_transform(shape_area, sf::st_crs(bladIndex.sf))

  sf::st_agr(bladIndex.sf) <- "constant"
  sf::st_agr(shape_area) <- "constant"
  bladnrsIntersect.sf <- sf::st_intersection(bladIndex.sf, sf::st_buffer(shape_area, 0))

  bladnrs <- bladnrsIntersect.sf$bladnr

  working_directory <- paste(directory, name, sep="/")
  if(type == "raster"){
    if(tolower(dem) == "dtm"){
      elevations.tif <- download_dtm(name = name, wd = working_directory, AHN = AHN, dem = dem, resolution = resolution, bladnrs = bladnrs, area = shape_area, interpolate = interpolate, delete.sheets = delete.sheets, redownload = redownload)
    } else if(tolower(dem) == "dsm"){
      elevations.tif <- download_dsm(name = name, wd = working_directory, AHN = AHN, dem = dem, resolution = resolution, bladnrs = bladnrs, area = shape_area, interpolate = interpolate, delete.sheets = delete.sheets, redownload = redownload)
    } else {
      stop("No correct dem argument is provided. Please use 'DTM' or 'DSM'.")
    }
  } else {
    elevations.tif <- download_point_cloud(name = name, wd = working_directory, AHN = AHN, bladnrs = bladnrs, area = shape_area, interpolate = interpolate, delete.sheets = delete.sheets, redownload = redownload)
  }
  return (elevations.tif)
}
