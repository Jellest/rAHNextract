#'@title get rectified grid for point
#'@inheritParams ahn_area
#'@noRd
get_rectified_grid_for_point <- function(name = "", X, Y, LONLAT = FALSE, resolution) {
  #RD new coordinate system
  my_point <- create_spatialpoint(X = X, Y = Y, LONLAT = LONLAT)

  coords <- sf::st_coordinates(my_point)
  if (coords[1, "X"] <  RD_min$X || coords[1, "X"] > RD_max$X) {
    stop("X coordinate out of range.")
  }
  if (coords[1, "Y"] < RD_min$Y || coords[1, "Y"] > RD_max$Y) {
    stop("Y coordinate out of range.")
  }

  #round to nearest x,y pixel bbox value to make middle pixel that aligns with rectified grid
  xround <- get_rectified_coordinates(coords[1, "X"], resolution = resolution)
  yround <- get_rectified_coordinates(coords[1, "Y"], resolution = resolution)

  ## get 8 rectified pixels around middle rectified pixel
  if (resolution == 0.5) {
    #x coordinate
    if (coords[1, "X"] -  xround > 0) {
      #rounded down
      my_xmin <- xround - (1 * resolution)
      my_xmax <- xround + (2 * resolution)
    } else if (coords[1, "X"] - xround < 0) {
      #rounded up
      my_xmin <- xround - (2 * resolution)
      my_xmax <- xround + (1 * resolution)
    } else {
      #whole numbers
      my_xmin <- coords[1, "X"] - (1 * resolution)
      my_xmax <- coords[1, "X"] + (2 * resolution)
    }

    #y coordinate
    if (coords[1, "Y"] - yround > 0) {
      #rounded down
      my_ymin <- yround - (1 * resolution)
      my_ymax <- yround + (2 * resolution)
    } else if (coords[1, "Y"] - yround < 0) {
      #rounded up
      my_ymin <- yround - (2 * resolution)
      my_ymax <- yround + (1 * resolution)
    } else {
      #whole numbers
      my_ymin <- coords[1, "Y"] - (1 * resolution)
      my_ymax <- coords[1, "Y"] + (2 * resolution)
    }
  } else if (resolution == 5 || resolution == 100) {
    stop("No support at the moment to use this resolutions.")
  } else {
    stop("No correct WCS resolution is provided. Please try again.")
  }
  bbox <- list("xmin" = my_xmin[[1]], "xmax" = my_xmax[[1]], "ymin" = my_ymin[[1]], "ymax" = my_ymax[[1]])
  return(list("name" = name, "point" = my_point, "rectified_bbox" = bbox))
}
