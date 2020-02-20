#'Download DSM
#'
#'@title Download DSM
#'@description Download DSM
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param resolution Default 0.5 meters. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5, 25, and 100 m.
#'@author Jelle Stuurman
#'get_resolution(AHN = "AHN3", resolution)
#'@return "res": resolition in meters, "res_name:" name of resolution string.
#'
get_resolution <- function(AHN = "AHN3", resolution){
  if(AHN == "AHN1"){
    if(missing(resolution)){
      warning("No resolution was found for importing AHN1. Resolution of 5 meters was used.")
      resolution = 5
      resolution_name <- "5m"
    } else if(resolution != 5 && resolution != 25 && resolution != 100){
      warning("No correct resolution was found for importing AHN1. Resolution of 5 m was used.")
      resolution <- 5
      resolution_name <- "5m"
    } else {
      resolution <- resolution
      resolution_name <- paste0(resolution,"m")
    }
  } else if(AHN == "AHN2" || AHN == "AHN3"){
    if(missing(resolution)){
      warning(paste0("No resolution was found for importing ", AHN, ". Resolution of 0.5 meters was used."))
      resolution = 0.5
      resolution_name <- "05m"
    } else {
      if(resolution == 0.5){
        resolution_name <- "05m"
      } else if(resolution == 5){
        resolution_name <- "5m"
      } else {
        warning(paste0("No correct resolution was found for importing ", AHN, ". Resolution of 0.5 meters was used."))
        resolution <- 0.5
        resolution_name <- "05m"
      }
    }
  }
  return(list("res" = resolution, "res_name" = resolution_name))
}