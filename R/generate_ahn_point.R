#'Generate AHN point
#'
#'@title Generate AHN area
#'@description Get AHN of a certain area
#'@param name Optional. Give a name of the specified area.
#'@param X X coordinate in RD New or WGS84 (LON)
#'@param Y Y coordinate in RD New or WGS84 (LAT)
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will be in RD New format.
#'@param resolution Default 0.5 meters. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5, 25, and 100 m.
#'@author Jelle Stuurman
#'generate_ahn_point(name = "", X, Y, LONLAT = FALSE, resolution)
#'@return "Name:", "point", "bbox"

generate_ahn_point <- function(name = "", X, Y, LONLAT = FALSE, resolution){
  #RD new coordinaten systeem
  my_point <- create_spatialpoint(X = X, Y = Y, LONLAT = LONLAT)

  coords <- sf::st_coordinates(my_point)
  if(coords[1,"X"] < 12628.0541 || coords[1,"X"] > 283594.4779){
    stop("X coordinate out of range.")
  }
  if(coords[1,"Y"] < 308179.0423 || coords[1,"Y"] > 611063.1429){
    stop("Y coordinate out of range.")
  }

  ##create 9 pixels bbox

  #round number
  #always round 0.5 to 1
  rounding <- function(x, digits) {
    posneg = sign(x)
    z = abs(x)*10^digits
    z = z + 0.5
    z = trunc(z)
    z = z/10^digits
    z*posneg
  }

  #round
  xround <- rounding(coords[1,"X"], digits = 0)
  yround <- rounding(coords[1,"Y"], digits = 0)

  if(resolution == 0.5){
    #x coordinate
    if(coords[1,"X"] -  xround > 0){
      #rounded down
      my_xmin <- xround - (1 * resolution)
      my_xmax <- xround + (2 * resolution)
    } else if(coords[1,"X"] - xround < 0){
      #rounded up
      my_xmin <- xround - (2 * resolution)
      my_xmax <- xround + (1 * resolution)
    } else {
      #whole numbers
      my_xmin <- coords[1,"X"] - (1 * resolution)
      my_xmax <- coords[1,"X"] + (2 * resolution)
    }

    #y coordinate
    if(coords[1,"Y"] - yround > 0){
      #rounded down
      my_ymin <- yround - (1 * resolution)
      my_ymax <- yround + (2 * resolution)
    } else if(coords[1,"Y"] - yround < 0){
     #rounded up
      my_ymin <- yround - (2 * resolution)
      my_ymax <- yround + (1 * resolution)
    } else {
      #whole numbers
      my_ymin <- coords[1,"Y"] - (1 * resolution)
      my_ymax <- coords[1,"Y"] + (2 * resolution)
    }
  } else if(resolution == 5 || resolution == 25 || resolution == 100){
    x_coords <- generate_lowres_ahn_points(n = coords[1,"X"], resolution)
    y_coords <- generate_lowres_ahn_points(n= coords[1,"Y"], resolution)
    my_xmin <- x_coords$min
    my_xmax <- x_coords$max
    my_ymin <- y_coords$min
    my_ymax <- y_coords$max
  } else {
    stop("No correct WCS extract has been implemented yet for this resolution. Please use the WFS method or choose 0.5 as a resolution.")
  }
  bbox <- list("xmin"= my_xmin, "xmax"= my_xmax, "ymin" = my_ymin, "ymax" = my_ymax)
  return(list("name" = name, "point" = my_point, "bbox" = bbox))
}
