#'Get AHN letter
#'
#'@title Get AHN letter
#'@description Get AHN letter
#'@param AHN Optional. Give a name of the specified area.
#'@param dem Required. Working directory
#'@param resolution resolution
#'@param interpolate Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@author Jelle Stuurman
#'get_ahn_letter(AHN, dem, interpolate)
#'@return AHN letter stirng

get_ahn_letter <- function(AHN, dem, resolution, interpolate){
  if(tolower(AHN) == 'ahn3'){
    if(tolower(dem) == "dsm"){
      ahn_letter <- "C"
    } else if(tolower(dem) == "dtm"){
      ahn_letter <- "M"
    }
  } else if(tolower(AHN) == 'ahn2'){
    if(resolution == 0.5){
      if(tolower(dem) == "dtm"){
        if(interpolate == TRUE){
          ahn_letter <-  "i"
        } else {
          ahn_letter <- "n"
        }
      } else if(tolower(dem) == "dsm"){
        ahn_letter <- "r"
      }
    } else {
      ahn_letter <- ""
    }
  } else if(tolower(AHN) == 'ahn1'){
    ahn_letter <- ""
  } else {
    ahn_letter <- ""
  }
  return (ahn_letter)
}
