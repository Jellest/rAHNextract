#'@inheritParams ahn_area
#'@noRd
create_wcs_url <- function(type, bbox, AHN = "AHN", resolution = list(res = 0.5, res_name = "05m"), dem = "DSM", interpolate = TRUE, wcs) {
  #get BBOX extent of buffer area
  my_bbox <- paste(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax, sep = ",")
  bbox_url <- paste0("BBOX=", my_bbox)

  #create image pixel dimensions
  if (type == "point") {
    my_width <- 3
    my_height <- 3
  } else if (type == "area") {
    my_width <- bbox$xmax - bbox$xmin
    my_height <- bbox$ymax - bbox$ymin
  }

  if (AHN %in% pdok_versions) {
    dimensions_url <- paste0("WIDTH=", toString(my_width), "&HEIGHT=", toString(my_height))
    #name of layer
    name_layer_url <- paste0("COVERAGE=", tolower(dem), "_", resolution$res_name, sep = "")

    #WCS image format
    imgFormat_url <- "FORMAT=GeoTiff"

    #coordinate system
    crs_url <- "CRS=EPSG:28992&RESPONSE_CRS=EPSG:28992"

  } else if (AHN %in% non_pdok_versions) {
    dimensions_url <- paste0("WIDTH=", toString(my_width), "&HEIGHT=", toString(my_height))
    #name of layer
    if (dem == "") {
      underscore <-  ""
    } else {
      underscore <- "_"
    }
    name_layer_url <- paste0("COVERAGE=", tolower(AHN), "_", resolution$res_name, underscore, dem)

    #WCS image format
    imgFormat_url <- "FORMAT=GEOTIFF_FLOAT32"

    #coordinate system
    crs_url <- "CRS=EPSG:28992&RESPONSE_CRS=EPSG:28992"
  }

  #generate URL
  wcsUrl <- paste(wcs, name_layer_url, bbox_url, crs_url, imgFormat_url, dimensions_url, sep = "&")

  return(wcsUrl)
}