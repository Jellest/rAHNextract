#'@inheritParams ahn_area
#'@noRd
get_rectified_grid <- function(name = "", X, Y, LONLAT = FALSE, resolution) {
  #RD new coordinaten systeem
  my_point <- create_spatialpoint(X = X, Y = Y, LONLAT = LONLAT)

  coords <- sf::st_coordinates(my_point)
  if (coords[1, "X"] < 482.06 || coords[1, "X"] > 284182.97) {
    stop("X coordinate out of range.")
  }
  if (coords[1, "Y"] < 284182.97 || coords[1, "Y"] > 637049.52) {
    stop("Y coordinate out of range.")
  }

  ## round number to whole number unless if decimal is 0.5
  #round down if decimal digit is lower than 5 (0.5-)
  #round up if decimal digit greater than 5 (0.5+)
  #don't round if decimal digit = 0.5

  rounding <- function(x) {
    rounded_value <- round(x, 1)  # First round to 1 decimal digit
    # Check if the rounded value is exactly 0.5
    if (abs(rounded_value - floor(rounded_value)) == 0.5) {
      z <- rounded_value  # If it's exactly 0.5, retain the rounded value
    } else {
      z <- round(x)  # Otherwise, round using the round() function
    }
    return(z)
  }

  #round to nearest x,y pixel bbox value to make middle pixel
  xround <- rounding(coords[1, "X"])
  yround <- rounding(coords[1, "Y"])

  ## create 8 pixel around middle pixel
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

get_rectified_grid(X = 150000.6, Y = 450000.3, resolution = 0.5)