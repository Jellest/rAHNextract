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
#'@param radius Radius of circle or squared BBOX in meters.
#'@param destfile destination file.
#'@param interpolate Default TRUE. Only applicable for AHN2 DTM. It decides if you want the interpolated version of the AHN2 or not.
#'@param gefilterd Default FALSE. Only applicable for AHN1 or AHN2 point cloud data. It decides if you want to download the 'gefilterd' point cloud data set or the 'uitgefilterd' data set.
#'@param keep.sheets Default TRUE. Only applicable if method.sheets is set to TRUE. Set to FALSE if you want to delete the downloaded sheets (kaartbladen).
#'@param redownload Default FALSE. Only applicable if sheets is set to TRUE. Set to TRUE if you want to redownload the sheets (kaartbladen)
#'@author Jelle Stuurman
#'get_ahn_sheets(name, area, AHN = "AHN3", resolution = 0.5, dem = "DSM", interpolate = TRUE, redownload = FALSE, keep.sheets = TRUE)
#'@return .tif AHN kaartblad cropped to area

get_ahn_sheets <- function(name, area, type = "", AHN = "AHN3", resolution = 0.5, dem = "DSM", radius, interpolate = TRUE, gefilterd = FALSE, redownload = FALSE, destfile = "structured", keep.sheets = TRUE){
  if(destfile == "structured"){
      outputDirectory <- paste(structured_output_folder)
      if (!dir.exists(outputDirectory)){
        dir.create(outputDirectory)
      }
  } else if(destfile == ""){
    outputDirectory <- tempdir()
  } else {
    outputDirectory <- destfile
  }

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

  if(type == "pc"){
    #download point clouds sheets
    bladnrsIntersect.sf <- sf::st_crop(bladIndex.sf, sf::st_buffer(shape_area, 0))
    bladnrs <- bladnrsIntersect.sf$bladnr
    bboxes <- c()
    for(f in 1:length(bladnrs)){
      bladnr <- bladnrsIntersect.sf$bladnr == bladnrs[f]
      singlebladNr.sf <- bladnrsIntersect.sf[bladnr,]
      sf::st_agr(singlebladNr.sf) <- "constant"
      singlebladNr.sf <- sf::st_crop(singlebladNr.sf, sf::st_buffer(shape_area, 0))
      my_bbox <- sf::st_bbox(singlebladNr.sf)
      bboxes <- cbind(bboxes, my_bbox)
    }
    data <- download_pointCloud(name = name, wd = outputDirectory, AHN = AHN, bladnrs = bladnrs, area = shape_area, radius = radius, bboxes = bboxes, gefilterd = gefilterd, keep.sheets = keep.sheets, redownload = redownload)
  } else {
    #download raster sheets
    bladnrsIntersect.sf <- sf::st_intersection(bladIndex.sf, sf::st_buffer(shape_area, 0))
    bladnrs <- bladnrsIntersect.sf$bladnr
    bboxes <- c()
    if(tolower(dem) == "dtm"){
      data <- download_dtm(name = name, wd = outputDirectory, AHN = AHN, dem = dem, resolution = resolution, radius = radius, bladnrs = bladnrs, area = shape_area, interpolate = interpolate, keep.sheets = keep.sheets, redownload = redownload)
    } else if(tolower(dem) == "dsm"){
      data <- download_dsm(name = name, wd = outputDirectory, AHN = AHN, dem = dem, resolution = resolution, radius = radius, bladnrs = bladnrs, area = shape_area, interpolate = interpolate, keep.sheets = keep.sheets, redownload = redownload)
    } else {
      stop("No correct dem argument is provided. Please use 'DTM' or 'DSM'.")
    }
  }
  return (data)
}
