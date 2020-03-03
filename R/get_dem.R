#'Download DSM
#'
#'@title Download DSM
#'@description Download DSM
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5, 25, and 100 m.
#'@param interpolate Default TRUE. Olny applicable for AHN2 DTM. It decides if you want the interpolated version of the AHN2 or not.
#'@author Jelle Stuurman
#'get_dem(AHN, dem, resolution, interpolate = TRUE)
#'@return DEM type string
get_dem <- function(AHN, dem, resolution, interpolate = TRUE){
  if(AHN == "AHN1"){
    if(tolower(dem) == "dsm"){
      warning(paste("There is no DSM available for this dataset. DTM (maaiveld)", resolution, "m was used.", sep = " "))
    }
    info <- paste(AHN, resolution, "m resolution DTM (maaiveld) selected.", sep = " ")
    dem <- ""
  } else if(AHN == "AHN2"){
    if(resolution == 5){
      dem <- ""
      info <- "AHN2 5 m resolution DTM (maaiveld) selected."
    } else if(resolution == 0.5){
      if(tolower(dem) == "dtm"){
        if(interpolate == TRUE){
          dem <- "int"
          info <- "AHN2 0.5 m resolution DTM (maaiveld) interpolated (opgevuld) selected."
        } else if(interpolate == FALSE){
          dem <- "non"
          info <- "AHN2 0.5 m resolution DTM (maaiveld) niet opgevuld selected."
        } else {
          stop("No correct interpolated parameter is provided. Please set it to 'TRUE' or 'FALSE'.")
        }
      } else if (tolower(dem) == "dsm"){
        dem <- "ruw"
        info <- "AHN2 0.5 m resolution DSM (ruw) selected."
      } else {
        stop("No correct dem is provided. Please select 'dtm' or 'dsm'.")
      }
    }
  } else if(AHN == "AHN3"){
    if(tolower(dem) != "dtm" && tolower(dem) != "dsm"){
      stop("Provided wrong DEM, Please select 'dsm' or 'dtm'.")
    } else {
      dem <- tolower(dem)
      if(dem == "dtm"){
        specs <- "(maaiveld)"
      } else if(dem == "dsm"){
        specs <- "(ruw)"
      }
      info <- paste("AHN3", resolution, "resolution", dem, specs, "selected.", sep = " ")
    }
  }
  message(info)
  return(dem)
}
