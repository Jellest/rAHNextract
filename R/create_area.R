#'@inheritParams ahn_area
#'@noRd
create_area <- function(name, X, Y, radius, resolution, bbox, polygon, LONLAT = FALSE, type) {
  if (type == "point") {
    my_bbox <- bbox
    my_bbox_area.sf <- create_bbox_polygon(name = name, bbox = my_bbox, resolution = resolution$res)
    my_area.sf <- my_bbox_area.sf$rectified_area
  } else if (type == "raster" || type == "pc") {
    if (missing(X) == TRUE && missing(Y) == TRUE && missing(polygon) == TRUE && radius == "") {
      #create BBOX using BBOX coordinates
      print("Creating BBOX from BBOX coordinates.")
      if (length(bbox) != 4) {
        stop("4 coordinates are required: XMIN, YMIN, XMAX, YMAX.")
      }
      if (LONLAT == TRUE) {
        #create spatial points in RD
        my_min <- create_spatialpoint(X = bbox$xmin, Y = bbox$ymin, LONLAT = LONLAT)
        my_max <- create_spatialpoint(X = bbox$xmax, Y = bbox$ymax, LONLAT = LONLAT)

        min_coords <- sf::st_coordinates(my_min)
        max_coords <- sf::st_coordinates(my_max)

        my_bbox <- list("xmin" = min_coords[1, "X"], "ymin" = min_coords[1, "Y"], "xmax" = max_coords[1, "X"], "ymax" = max_coords[1, "Y"])
      } else {
        my_bbox <- list("xmin" = bbox[1], "ymin" = bbox[2], "xmax" = bbox[3], "ymax" = bbox[4])
      }
      my_bbox_area.sf <- create_bbox_polygon(name = name, bbox = my_bbox, resolution = resolution$res)
      my_area.sf <- my_bbox_area.sf$rectified_area
    } else if (radius != "") {
      #create circle through buffer around a X,Y point
      if (radius == 0) {
        stop("Radius input is set to 0. If you want to get AHN elevation of a certain point, please use the function 'ahn_point()'.")
      }
      if (bbox == FALSE) {
        print("Creating circle from single X,Y point and a radius input.")
      }
      if (missing(X) == TRUE || missing(Y) == TRUE) {
        stop("X or Y input coordinates are missing.")
      }

      my_point <- create_spatialpoint(X = X, Y = Y, LONLAT = LONLAT)
      my_area.sf <- sf::st_buffer(my_point, dist = radius)
      sf::st_crs(my_area.sf, epsg_rd)

      my_bbox <- sf::st_bbox(my_area.sf, crs = epsg_rd)
      my_bbox_area.sf <- create_bbox_polygon(name = name, bbox = my_bbox, resolution = resolution$res)

      # recreate circle based on rectified grid
      new_radius <- (my_bbox_area.sf$rectified_bbox$xmax - my_bbox_area.sf$rectified_bbox$xmin) / 2
      if (new_radius != radius) {
        warning(paste("The provided X, Y coordinates and radius do not return an area that matches the AHN rectified grid. The radius has therefore been adjusted with ", (new_radius - radius), "meters.", sep = " "))
      }
      my_area.sf <- sf::st_buffer(my_point, dist = radius)
      sf::st_crs(my_area.sf, epsg_rd)

      if (bbox == TRUE) {
        #create bbox based on only radius
        print("Creating bbox from X,Y point and radius input.")
        my_area.sf <- my_bbox_area.sf$rectified_area
      }
    } else if (missing(X) == TRUE && missing(Y) == TRUE && bbox == FALSE && radius == "") {
      #load shape through polygon to create area
      warning("Using a polygon as area will most likely not return a raster that aligns with the rectified grid. If this raster will be used for elevation analysis, wrong values may be calculated.")

      my_area.sf <- sf::st_as_sf(polygon)
      geometry_point <- sf::st_is(my_area.sf, "POINT")
      geometry_polygon <- sf::st_is(my_area.sf, "POLYGON")
      if (geometry_point == "POINT" || geometry_polygon == FALSE) {
        stop("Geometry type is not a polygon. Please make sure the input given for the 'polygon' parameter is a polygon in the correct format.")
      }
      print("Creating area from custom geometry.")
      if (LONLAT == TRUE) {
        my_area.sf <- sf::st_transform(polygon, epsg_rd)
      }
      if (nrow(my_area.sf) != 1) {
        stop("The selected polygon has no or more than one feature. Add/reduce to one feature or use loop functionalities.")
      }

      #get bbox of polygon area
      my_bbox <- sf::st_bbox(my_area.sf, crs = epsg_rd)
      my_bbox_area.sf <- create_bbox_polygon(name = name, bbox = my_bbox, resolution = resolution$res)
    }
    if (!exists("my_area.sf")) {
      stop("Too many or little parameters have been defined. Please add or remove them.")
    } else {
      sf::st_crs(my_area.sf, epsg_rd)
      #sf::st_write(obj = my_area.sf, dsn = "C:/ROutput/Utrecht/My_area.shp", append=FALSE)
    }
  }

  return(list("area" = my_area.sf, "rectified_bbox" = my_bbox_area.sf$rectified_bbox))
}
