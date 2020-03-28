#'Get Create area
#'
#'@title Create area
#'@description Create area through buffer, BBOX or custom area
#'@param X X coordidnate in RD New or WGS84 (LON)
#'@param Y Y coordidnate in RD New or WGS84 (LAT)
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will be in RD New format.
#'@param radius Optional. Set radius in meters of area around a point to create a buffer area.
#'@param bbox Optional. Set bbox of area. c(XMIN, YMIN, XMAX, YMAX)
#'@param polygon Optional. Use polygonetric object as your area. .shp, .gpkg. Output will be BBox of this object.
#'@author Jelle Stuurman
#'create_area(X, Y, radius, bbox, polygon, LONLAT = FALSE, sheets = FALSE)
#'@return "area": polygon of area, "bbox": BBOX coordinates.


create_area <- function(X, Y, radius, bbox, polygon, LONLAT = FALSE){
  if(missing(bbox) == TRUE && missing(polygon) == TRUE){
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
    my_bbox <- sf::st_bbox(my_area.sf, crs = epsg_rd)
    my_bbox <- data.frame("xmin" = floor(my_bbox$xmin), "ymin" = floor(my_bbox$ymin), "xmax" = ceiling(my_bbox$xmax), "ymax" = ceiling(my_bbox$ymax))
    my_bbox_area.sf <- create_bbox_polygon(my_bbox)
  } else if(missing(X) == TRUE && missing(Y) == TRUE && missing(polygon) == TRUE){
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
    my_bbox_area.sf <- create_bbox_polygon(my_bbox)
    my_area.sf <- my_bbox_area.sf
  } else if(missing(X) == TRUE && missing(Y) == TRUE && missing(bbox) == TRUE){
    #load shape through polygon
    if(LONLAT == TRUE){
      my_area.sf <- sf::st_transform(polygon, epsg_rd)
      #my_area.sf <- st_make_valid(my_area.sf)
    } else {
      my_area.sf <- polygon
      #my_area.sf <- st_make_valid(my_area.sf)
    }
    my_bbox <- sf::st_bbox(my_area.sf, crs = epsg_rd)
    my_bbox <- data.frame("xmin" = floor(my_bbox$xmin), "ymin" = floor(my_bbox$ymin), "xmax" = ceiling(my_bbox$xmax), "ymax" = ceiling(my_bbox$ymax))
    my_bbox_area.sf <- create_bbox_polygon(my_bbox)
  }
  return(list("area" = my_area.sf, "bbox_area" = my_bbox_area.sf, "bbox" = my_bbox))
}
