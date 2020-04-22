#'Get cropped AHN sheets
#'
#'@title Get cropped AHN sheets
#'@description Get cropped AHN sheets
#'get_ahn_sheets(name, area, AHN = "AHN3", resolution = 0.5, dem = "DSM", interpolate = TRUE, sheets.redownload = FALSE, sheets.keep = TRUE)
#'@param name Optional. Give a name of the specified area.
#'@param type 'raster' or 'pc'.
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5 and 100 m.
#'@param area Required area to be downloaded
#'@param radius Radius of circle or squared BBOX in meters.
#'@param output.dir destination file.
#'@param interpolate Default TRUE. Only applicable for AHN2 DTM. It decides if you want the interpolated version of the AHN2 or not.
#'@param gefilterd Default FALSE. Only applicable for AHN1 or AHN2 point cloud data. It decides if you want to download the 'gefilterd' point cloud data set or the 'uitgefilterd' data set.
#'@param sheets.keep Default TRUE. Only applicable if sheets.method is set to TRUE. Set to FALSE if you want to delete the downloaded sheets (structure).
#'@param sheets.location Optional. Default is the 'AHN_sheets' directory in working directory. Set directory where all the AHN sheets are stored or will be stored. Always use the correct directory structure and capitalization within the selected directory. Example directory structure: 'AHN3/DSM' or 'AHN2/DTM' Only use extracted files in their original name after download.
#'@param sheets.redownload Default FALSE. Only applicable if sheets is set to TRUE. Set to TRUE if you want to redownload the sheets (structure)
#'@author Jelle Stuurman
#'@return GeoTIFF AHN kaartblad cropped to area

get_ahn_sheets <- function(name, area, type = "", AHN = "AHN3", resolution = 0.5, dem = "DSM", radius, interpolate = TRUE, gefilterd = FALSE, sheets.redownload = FALSE, output.dir = "AHN_output", sheets.location, sheets.keep = TRUE){
  #bladIndex.sf <- download_bladnrs(output.dir = directory, AHN = AHN)
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
    ahn_data <- download_pointCloud(name = name, output.dir = output.dir, AHN = AHN, bladnrs = bladnrs, area = shape_area, radius = radius, bboxes = bboxes, gefilterd = gefilterd, sheets.location = sheets.location, sheets.keep = sheets.keep, sheets.redownload = sheets.redownload)
  } else if(type == "point" || type == "area"){
    #download raster sheets for area or point intersection
    bladnrsIntersect.sf <- sf::st_intersection(bladIndex.sf, sf::st_buffer(shape_area, 0))
    bladnrs <- bladnrsIntersect.sf$bladnr
    if(type == "point"){
      if(length(bladnrs) == 4){
        stop("The selected point is exactly on the intersect of 4 AHN sheets. Pease adjust the X and Y coordinates by at least 1 meter.")
      } else if(length(bladnrs) == 2){
        stop("The selected point is exactly on the intersect of 2 AHN sheets. Pease adjust the X OR Y coordinates by at least 1 meter. If changinig either coordinate doe not work, change both.")
      }
    } else if(type == "area"){
      geom_types <- sf::st_geometry_type(bladnrsIntersect.sf, by_geometry = TRUE)
      if(length(unique(geom_types)) > 1){
        polygon_bladnrsIntersect.sf <- sf::st_collection_extract(x = bladnrsIntersect.sf, type = "POLYGON", warn = FALSE)
        bladnrs <- polygon_bladnrsIntersect.sf$bladnr
      }
    }
    bboxes <- c()
    if(tolower(dem) == "dtm"){
      ahn_data <- download_dtm(name = name, output.dir = output.dir, AHN = AHN, dem = dem, resolution = resolution, radius = radius, bladnrs = bladnrs, area = shape_area, interpolate = interpolate, sheets.location = sheets.location, sheets.keep = sheets.keep, sheets.redownload = sheets.redownload)
    } else if(tolower(dem) == "dsm"){
      ahn_data <- download_dsm(name = name, output.dir = output.dir, AHN = AHN, dem = dem, resolution = resolution, radius = radius, bladnrs = bladnrs, area = shape_area, interpolate = interpolate, sheets.location = sheets.location, sheets.keep = sheets.keep, sheets.redownload = sheets.redownload)
    } else {
      stop("No correct dem argument is provided. Please use 'DTM' or 'DSM'.")
    }
  }
  return (ahn_data)
}
