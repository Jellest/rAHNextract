#'Get Create area
#'
#'@title Create area
#'@description Create area through buffer, BBOX or custom area
#'create_area(X, Y, radius, bbox, polygon, LONLAT = FALSE, sheets = FALSE)
#'@param X X coordinate in RD New or WGS84 (LON)
#'@param Y Y coordinate in RD New or WGS84 (LAT)
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will be in RD New format.
#'@param radius Optional. Set radius in meters of area around a point to create a buffer area.
#'@param bbox Optional. Set to TRUE if you want to create a bbox with the radius as parameter. Or create own bbox of area with format: c(XMIN, YMIN, XMAX, YMAX)
#'@param polygon Optional. Use polygon object as your area. Formats includes .shp, .gpkg. Output will be BBOX of this object.
#'@param type 'point', 'raster' or 'pc'
#'@author Jelle Stuurman
#'@return "area": polygon of area, "bbox": BBOX coordinates.
create_area <- function(X, Y, radius, bbox, polygon, LONLAT = FALSE, type){
  if(type == "point"){
    my_bbox <- bbox
    my_bbox_area.sf <- create_bbox_polygon(my_bbox)
    my_area.sf <- my_bbox_area.sf
  } else if(type == "raster" || type == "pc"){
      if((missing(bbox) == TRUE || bbox == TRUE) && missing(polygon) == TRUE){
        #create circle through buffer around a point
        if(missing(bbox) == TRUE){
          print("Creating circle from radius input.")
        }
        if(missing(X) == TRUE || missing(Y) == TRUE){
          stop("X or Y input coordinates are missing.")
        }
        if(missing(radius)){
          stop("Radius input is missing. If you want to get AHN elevation of a certain point, please use the function 'ahn_point()'.")
      }

      my_point <- create_spatialpoint(X = X, Y = Y, LONLAT = LONLAT)
      my_area.sf <- sf::st_buffer(my_point, dist=radius)
      sf::st_crs(my_area.sf, epsg_rd)

      my_bbox <- sf::st_bbox(my_area.sf, crs = epsg_rd)
      my_bbox <- data.frame("xmin" = floor(my_bbox$xmin), "ymin" = floor(my_bbox$ymin), "xmax" = ceiling(my_bbox$xmax), "ymax" = ceiling(my_bbox$ymax))
      my_bbox_area.sf <- create_bbox_polygon(my_bbox)
      if(missing(bbox) == FALSE && bbox == TRUE){
        #create bbox based on only radius
        print("Creating bbox from point and radius input.")
        my_area.sf <- my_bbox_area.sf
      }
    } else if(missing(X) == TRUE && missing(Y) == TRUE && missing(polygon) == TRUE && radius == ""){
      #create BBOX using BBOX coordinates
      print("Creating BBOX from BBOX coordinates.")
      if(length(bbox) != 4){
        stop("4 coordinates are required: XMIN, YMIN, XMAX, YMAX.")
      }
      if(LONLAT == TRUE){
        my_min <- create_spatialpoint(X = bbox$xmin, Y = bbox$ymin, LONLAT = LONLAT)
        my_max <- create_spatialpoint(X = bbox$xmax, Y=bbox$ymax, LONLAT = LONLAT)

        min_coords <- sf::st_coordinates(my_min)
        max_coords <- sf::st_coordinates(my_max)

        my_bbox <- data.frame("xmin" = min_coords[1,"X"], "ymin" = min_coords[1,"Y"], "xmax" = max_coords[1,"X"], "ymax" = max_coords[1,"Y"])
      } else {
        my_bbox <- data.frame("xmin" = bbox[1], "ymin" = bbox[2], "xmax" = bbox[3], "ymax" = bbox[4])
      }
      my_bbox_area.sf <- create_bbox_polygon(my_bbox)
      my_area.sf <- my_bbox_area.sf
    } else if(missing(X) == TRUE && missing(Y) == TRUE && missing(bbox) == TRUE && radius == ""){
      my_area.sf <- sf::st_as_sf(polygon)
      geometry_point <- sf::st_is(my_area.sf, "POINT")
      geometry_polygon <- sf::st_is(my_area.sf, "POLYGON")
      #load shape through polygon to create area
      if(geometry_point == "POINT" || geometry_polygon == FALSE){
        stop("Geometry type is not a polygon. Please make sure the input given for the 'polygon' paramter is a polygon in the correct format.")
      }
      print("Creating area from custom geometry.")
      if(LONLAT == TRUE){
        my_area.sf <- sf::st_transform(polygon, epsg_rd)
      }
      if(nrow(my_area.sf) != 1){
        stop("The selected polygon has no or more than one feature. Add/reduce to one feature or use loop functionalities.")
      }
      my_bbox <- sf::st_bbox(my_area.sf, crs = epsg_rd)
      my_bbox <- data.frame("xmin" = floor(my_bbox$xmin), "ymin" = floor(my_bbox$ymin), "xmax" = ceiling(my_bbox$xmax), "ymax" = ceiling(my_bbox$ymax))
      my_bbox_area.sf <- create_bbox_polygon(my_bbox)
    }
    if(!exists("my_area.sf")){
      stop("Too many or little parameters have been defined. Please add or remove them.")
    } else {
      sf::st_crs(my_area.sf, epsg_rd)
      #sf::st_write(obj = my_area.sf, dsn = "C:/ROutput/Utrecht/My_area.shp",  append=FALSE)
    }
  }
  return(list("area" = my_area.sf, "bbox_area" = my_bbox_area.sf, "bbox" = my_bbox))
}
