#'Create WCS URL
#'
#'@title Create WCS URL
#'@description Create WCS URL
#'@param type required. Point or area.
#'@param bbox Optional. Set bbox of area. c(XMIN, YMIN, XMAX, YMAX)
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5, 25, and 100 m.
#'@param interpolate Default TRUE. Only applicable for AHN2 DTM. It decides if you want the interpolated version of the AHN2 or not.
#'@author Jelle Stuurman
#'create_wcs_url(type, bbox, AHN = "AHN3", resolution = 0.5, dem = "DSM", interpolate = TRUE)
#'@return WCS URL string

create_wcs_url <- function(type, bbox, AHN = "AHN3", resolution = 0.5, dem = "DSM", interpolate = TRUE){


  wcs_baseUrl = paste0(ngr,"/", tolower(AHN), "/wcs?SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage")

  #get resolution
  my_resolution <- get_resolution(AHN= AHN, resolution = resolution)

  #get dem type
  dem <- get_dem(AHN = AHN, resolution = my_resolution$res, dem = dem, interpolate = interpolate)

  #get BBOX extent of buffer area
  my_bbox <- paste(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax, sep=",")
  bbox_url <- paste0("BBOX=", my_bbox)

  #create image pixel dimensions
  if(type == "point"){
    my_width <- 3
    my_height <- 3
  } else if(type == "area") {
    my_width <- bbox$xmax - bbox$xmin
    my_height <- bbox$ymax - bbox$ymin
  }

  dimensions_url <- paste0("WIDTH=", toString(my_width), "&HEIGHT=", toString(my_height))
  #name of layer
  if(dem == ""){
    underscore <-  ""
  } else {
    underscore <- "_"
  }
  name_layer_url <- paste0("COVERAGE=", tolower(AHN), "_" , my_resolution$res_name , underscore, dem)

  #WCS image format
  imgFormat_url <- "FORMAT=GEOTIFF_FLOAT32"

  #coordinate system
  crs_url <- "CRS=EPSG:28992&RESPONSE_CRS=EPSG:28992"

  #generate URL
  wcsUrl <- paste(wcs_baseUrl, name_layer_url, bbox_url, crs_url, imgFormat_url, dimensions_url, sep="&")

  #print(wcsUrl)

  return (wcsUrl)
}
