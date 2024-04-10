#'@inheritParams ahn_area
#'@noRd
generate_ahn_point <- function(name = "", X, Y, LONLAT = FALSE, resolution) {
  #RD new coordinaten systeem
  my_point <- create_spatialpoint(X = X, Y = Y, LONLAT = LONLAT)

  coords <- sf::st_coordinates(my_point)
  if (coords[1, "X"] < 482.06 || coords[1, "X"] > 284182.97) {
    stop("X coordinate out of range.")
  }
  if (coords[1, "Y"] < 284182.97 || coords[1, "Y"] > 637049.52) {
    stop("Y coordinate out of range.")
  }

  ##create 9 pixels bbox coordinates

  #round number
  #always round 0.5 to 1
  rounding <- function(x, digits) {
    posneg <- sign(x)
    z <- abs(x) * 10^digits
    z <- z + 0.5
    z <- trunc(z)
    z <- z / 10^digits
    z * posneg
  }

  #round
  xround <- rounding(coords[1, "X"], digits = 0)
  yround <- rounding(coords[1, "Y"], digits = 0)

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
  } else {
    stop("No correct WCS resolution is provided. Please try again.")
  }
  bbox <- data.frame("xmin" = my_xmin, "xmax" = my_xmax, "ymin" = my_ymin, "ymax" = my_ymax)
  return(list("name" = name, "point" = my_point, "bbox" = bbox))
}
