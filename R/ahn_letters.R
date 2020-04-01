#'view AHN letters
#'
#'@title view AHN letter
#'@description Show list of AHN letters to understand the naming scheme used by the PDOK to get the corresponding AHN dataset. The AHN letter is found  in the name of the AHN gefilterd files

#'@author Jelle Stuurman
#'@return data frame with ahn letters and their meaning
#'@export
ahn_letters <- function(){
  ahn_letters <- list_ahn_letters
return (ahn_letters)
}
