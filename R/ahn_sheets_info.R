#'@title AHN sheets info
#'@description get polygon features that describe the AHN sheets
#'@inheritParams ahn_area
#'@export
#'@author Jelle Stuurman
ahn_sheets_info <- function(AHN, dem, resolution, output.dir) {
  if (missing(AHN) == TRUE) {
    stop("No value provided for the AHN parameter. Please enter a valid AHN parameter.")
  }
  check_ahn_version(AHN)

  if (missing(resolution)) {
    resolution <- ""
    my_resolution <- get_resolution(AHN = AHN, resolution = resolution)
  }

  if (missing(dem) == TRUE) {
    stop("No value provided for the dem parameter. Please enter a valid dem parameter.")
  }

  ahn_bi <- get_bladindex(AHN = AHN, dem = dem, resolution = my_resolution$res)

  if (missing(output.dir) == FALSE) {
    sf::st_write(obj = ahn_bi, dsn = paste(output.dir, "/",  paste0(AHN, "_", "bladindex.gpkg"), sep = ""), append = FALSE)
  }
  return(ahn_bi)
}