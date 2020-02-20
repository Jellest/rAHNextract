#'Get cropped AHN sheets
#'
#'@title Get cropped AHN sheets
#'@description Get cropped AHN sheets
#'@param name Optional. Give a name of the specified area.
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5, 25, and 100 m.
#'@param badnrs Required. Blad numbers.
#'@param area Required area to be downloaded
#'@param interpolate Default TRUE. Olny applicable for AHN2 DTM. It decides if you want the interpolated version of the AHN2 or not.
#'@param delete.sheets Deault TRUE. Only applicable if sheets is set to TRUE. Set to FALSE if you want to keep the downloaded sheets (kaartbladen).
#'@param redownload Deafult FALSE. nly applicable if sheets is set to TRUE. Set to TRUE if you want to redownload the sheets (kaartbladen)
#'@author Jelle Stuurman
#'get_ahn_sheets(name, area, AHN = "AHN3", resolution = 0.5, dem = "dsm", interpolate = TRUE, redownload = FALSE, delete.sheets = TRUE)
#'@return .tif AHN kaartblad cropped to area

get_ahn_sheets <- function(name, area, AHN = "AHN3", resolution = 0.5, dem = "dsm", interpolate = TRUE, redownload = FALSE, delete.sheets = TRUE){
  ###BladIndex method ###
  #get AHN bladIndex
  dataDirectory <- paste("data")

  if (!dir.exists(dataDirectory)){
    dir.create(dataDirectory)
  }

  directory <- paste("data", AHN, sep="/")
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

  area <- st_transform(area, st_crs(bladIndex.sf))

  sf::st_agr(bladIndex.sf) <- "constant"
  sf::st_agr(area) <- "constant"
  # print("summary area")
  # print(summary(area))
  # print("summary bladindex")
  # print(summary(bladIndex.sf))
  bladnrsIntersect.sf <- sf::st_intersection(bladIndex.sf, st_buffer(area, 0))
  #print(summary(bladnrsIntersect.sf))
  #View(bladnrsIntersect.sf)
  #st_write(bladnrsIntersect.sf, "intersect.gpkg")

  bladnrs <- bladnrsIntersect.sf$bladnr

  working_directory <- paste(directory, name, sep="/")
  if(tolower(dem) == "dtm"){
    elevations.tif <- download_dtm(name = name, wd = working_directory, AHN = AHN, dem = dem, resolution = resolution, bladnrs = bladnrs, area = area, interpolate = interpolate, delete.sheets = delete.sheets, redownload = redownload)
  } else if(tolower(dem) == "dsm"){
    elevations.tif <- download_dsm(name = name, wd = working_directory, AHN = AHN, dem = dem, resolution = resolution, bladnrs = bladnrs, area = area, interpolate = interpolate, delete.sheets = delete.sheets, redownload = redownload)
  } else {
    stop("No correct dem argument is provided. Please use 'DTM' or 'DSM'.")
  }

  # if(redownload == TRUE){
  #   warning("AHN file(s) already existed and were redownloaded")
  #   download_ahn_area(name = name, area = area, dem = dem, AHN = AHN, resolution = resolution, redownload = redownload, delete.sheets = TRUE)
  # }
  # if(delete.sheets == TRUE){
  #   file.remove(bladIndex_shape_file)
  #   file.remove(paste( bladIndex_shape_filepath, "/", AHN, "_bladIndex", ".shx", sep=""))
  #   file.remove(paste( bladIndex_shape_filepath, "/", AHN, "_bladIndex", ".dbf", sep=""))
  #   file.remove(paste( bladIndex_shape_filepath, "/", AHN, "_bladIndex", ".prj", sep=""))
  # }
  return (elevations.tif)
}
