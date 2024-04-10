#'@inheritParams ahn_area
#'@noRd
get_resolution <- function(AHN = "AHN", resolution) {
  if (AHN == "AHN1") {
    if (resolution == "") {
      message()("No resolution was found for importing AHN1. Resolution of 5 meters will be used.")
      resolution <- 5
      resolution_name <- "5m"
    } else if (resolution != 5 && resolution != 100) {
      warning("No correct resolution was found for importing AHN1. Resolution of 5 m will be used.")
      resolution <- 5
      resolution_name <- "5m"
    } else {
      resolution <- resolution
      resolution_name <- paste0(resolution, "m")
    }
  } else if (AHN %in% AHN_05res_versions) {
    if (resolution == "") {
      message(paste0("No resolution was found for importing ", AHN, ". Resolution of 0.5 meters will be used."))
      resolution <- 0.5
      resolution_name <- "05m"
    } else {
      if (resolution == 0.5) {
        resolution_name <- "05m"
      } else if (resolution == 5) {
        resolution_name <- "5m"
      } else {
        warning(paste0("No correct resolution was found for importing ", AHN, ". Resolution of 0.5 meters will be used."))
        resolution <- 0.5
        resolution_name <- "05m"
      }
    }
  } else {
    stop("Error in getting the resolution.")
  }
  return(list("res" = resolution, "res_name" = resolution_name))
}