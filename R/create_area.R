#'Get Create area
#'
#'@title Create area
#'@description Create area through buffer, BBOX or custom area
#'@param X X coordidnate in RD New or WGS84 (LON)
#'@param Y Y coordidnate in RD New or WGS84 (LAT)
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will be in RD New format.
#'@param radius Optional. Set radius in meters of area around a point to create a buffer area.
#'@param bbox Optional. Set bbox of area. c(XMIN, YMIN, XMAX, YMAX)
#'@param geom Optional. Use geometric object as your area. .shp, .gpkg. Output will be BBox of this object.
#'@param sheets Default FALSE. Set to TRUE if you want to download AHN areas through the sheets (kaartbladen) instead through the WCS method (geotiff 32bit float
#'@author Jelle Stuurman
#'create_area(X, Y, radius, bbox, geom, LONLAT = FALSE, sheets = FALSE)
#'@return "area": geometry of area, "bbox": BBOX coordinates.

create_area <- function(X, Y, radius, bbox, geom, LONLAT = FALSE, sheets = FALSE){
  my_bbox <-NULL
  if(missing(bbox) == TRUE && missing(geom) == TRUE){
    #create circle through buffer around a point
    if(missing(X) == TRUE || missing(Y) == TRUE){
      stop("X or Y input coordinates are missing.")
    }
    if(missing(radius)){
      stop("Radius input coordinate(s) is/are missing.")
    }
    my_point <- create_spatialpoint(X = X, Y = Y, LONLAT = LONLAT)
    my_area.sf <- sf::st_buffer(my_point, dist=radius)
    sf::st_crs(my_area.sf, epsg_rd)
    if(sheets == FALSE){
      my_bbox <- sf::st_bbox(my_area.sf, crs = epsg_rd)
      my_bbox <- data.frame("xmin" = floor(my_bbox$xmin), "ymin" = floor(my_bbox$ymin), "xmax" = ceiling(my_bbox$xmax), "ymax" = ceiling(my_bbox$ymax))
      my_area.sf <- create_bbox_polygon(my_bbox)
    }
  } else if(missing(X) == TRUE && missing(Y) == TRUE && missing(geom) == TRUE){
    #create bbox
    if(length(bbox) != 4){
      stop("4 coordinates are required: XMIN, YMIN, XMAX, YMAX.")
    }
    if(LONLAT == TRUE){
      my_min <- create_spatialpoint(X = bbox[1], Y = bbox[2], LONLAT = LONLAT)
      my_max <- create_spatialpoint(X = bbox[3], Y=bbox[4], LONLAT = LONLAT)

      min_coords <- sf::st_coordinates(my_min)
      max_coords <- sf::st_coordinates(my_max)

      my_bbox <- data.frame("xmin" = min_coords[1,"X"], "ymin" = min_coords[1,"Y"], "xmax" = max_coords[1,"X"], "ymax" = max_coords[1,"Y"])
    } else {
      my_bbox <- data.frame("xmin" = bbox[1], "ymin" = bbox[2], "xmax" = bbox[3], "ymax" = bbox[4])
    }
    if(sheets == FALSE){
      my_bbox <- data.frame("xmin" = floor(my_bbox$xmin), "ymin" = floor(my_bbox$ymin), "xmax" = ceiling(my_bbox$xmax), "ymax" = ceiling(my_bbox$ymax))
    }
    my_area.sf <- create_bbox_polygon(my_bbox)
  } else if(missing(X) == TRUE && missing(Y) == TRUE && missing(bbox) == TRUE){
    #load shape
    if(LONLAT == TRUE){
      my_area.sf <- sf::st_transform(geom, epsg_rd)
      #my_area.sf <- st_make_valid(my_area.sf)
    } else {
      my_area.sf <- geom
      #my_area.sf <- st_make_valid(my_area.sf)
    }
    if(sheets == FALSE){
      my_bbox <- sf::st_bbox(my_area.sf, crs = epsg_rd)
      my_bbox <- data.frame("xmin" = floor(my_bbox$xmin), "ymin" = floor(my_bbox$ymin), "xmax" = ceiling(my_bbox$xmax), "ymax" = ceiling(my_bbox$ymax))
      my_area.sf <- create_bbox_polygon(my_bbox)
    }
  }
  print("BBOX: ", my_bbox)
  return(list("area" = my_area.sf, "bbox" = my_bbox))
}
