#'Generate low resolution AHN
#'
#'@title Generate low resolution AHN
#'@description Get low resolution AHN
#'@param n number
#'@param resolution Default 0.5 meters. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5 and 100 m.
#'@author Jelle Stuurman
#'generate_lowres_ahn_points(n, resolution)
#'@return "min" "max" coordinates.
generate_lowres_ahn_points <- function(n, resolution){
  rounding <- function(n, digits) {
    posneg = sign(n)
    z = abs(n)*10^digits
    z = z + 0.5
    z = trunc(z)
    z = z/10^digits
    z*posneg
  }

  if(resolution == 5){
    digits <- -1
  } else {
    digits <- -2
  }

  my_last <- substr(n, nchar(n) - 2 + 1, nchar(n))
  my_whole_round <- rounding(n = n, digits = 0)
  my_round <- rounding(n = my_whole_round, digits = digits)

  # if(resolution == 25){
  #  res_dif  <- my_whole_round - round(n, -2)
  #  if(res_dif == -25){
  #    my_round <- my_round - resolution
  #  }
  #} else
  if(resolution == 5){
    res_dif  <- my_whole_round - round(n, -1)
    if(res_dif == -5){
      my_round <- my_round - resolution
    }
  } else if(resolution == 100){
    res_dif  <- my_whole_round - round(n, -2)
  }

  # print(paste0("N: ", n))
  # print(paste0("res dif: ", res_dif))
  # print(paste0("my whole: ",my_whole_round))
  # print(paste0("my round: ",my_round))

  if(my_round <= my_whole_round){
    if(res_dif >= resolution){
      #print("0,1,2,3")
      min <- my_round
      max <- my_round + (3 * resolution)
      #print(c(my_round,my_round+resolution,(my_round+(2*resolution)),max))
    } else {
      #print("-1,0,1,2")
      min <- my_round - resolution
      max <- my_round + (2 * resolution)
      #print(c(min,my_round,(my_round + resolution),max))
    }

  } else if(my_round > my_whole_round){
    if(res_dif <= (-1 * resolution)){
      #print("-3,-2,-1,0")
      min <- my_round - (3 * resolution)
      max <- my_round
      #print(c(min,(my_round-(2 *resolution)), my_round -resolution, max))
    } else {
      #print("-2,-1,0,1")
      min <- my_round - (2 * resolution)
      max <- my_round + resolution
      #print(c(min,my_round - resolution,my_round,max))
    }
  }
  return(list("min" = min, "max" = max))
}
