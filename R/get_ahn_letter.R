#'@description get the ahn letter
#' @inheritParams ahn_area
#'@noRd
get_ahn_letter <- function(AHN, dem, resolution, interpolate, method, gefilterd) {
  if (tolower(AHN) == "ahn3") {
    if (method == "raster") {
      if (tolower(dem) == "dsm") {
        if (resolution == 0.5) {
          ahn_letter <- "R"
        } else if (resolution == 5) {
          ahn_letter <- "R5"
        }
      } else if (tolower(dem) == "dtm") {
        if (resolution == 0.5) {
          ahn_letter <- "M"
        } else if (resolution == 5) {
          ahn_letter <- "M5"
        }
      }
    } else if (method == "pc") {
      ahn_letter <- "C"
    }
  } else if (tolower(AHN) == "ahn2") {
    if (method == "raster") {
      if (resolution == 0.5) {
        if (tolower(dem) == "dtm") {
          if (interpolate == TRUE) {
            ahn_letter <-  "i"
          } else {
            ahn_letter <- "n"
          }
        } else if (tolower(dem) == "dsm") {
          ahn_letter <- "r"
        }
      } else { #5m resolution
        #no letter needed to be retrieved. Letter added later in code to be included in naming of gefilterd file.
        ahn_letter <- ""

      }
    } else if (method == "pc") {
      if (gefilterd == TRUE) {
        ahn_letter <- "g"
      } else {
        ahn_letter <- "u"
      }
    }
  } else if (tolower(AHN) == "ahn1") {
    if (method == "raster") {
      ahn_letter <- ""
    } else if (method == "pc") {
      if (gefilterd == TRUE) {
        ahn_letter <- "g"
      } else {
        ahn_letter <- "u"
      }
    }
  } else {
    ahn_letter <- ""
  }
  return(ahn_letter)
}