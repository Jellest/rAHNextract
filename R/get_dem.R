#'@inheritParams ahn_area
#'@noRd
get_dem <- function(AHN, dem, resolution, interpolate = TRUE) {
  if (tolower(dem) == "maaiveld" || tolower(dem) == "dtm") {
    dem <- "DTM"
  } else if (tolower(dem) == "ruw" || tolower(dem) == "dsm") {
    dem <- "DSM"
  } else if (dem == "") {
    dem <- "DSM"
  } else {
    stop("Provided wrong DEM. Please select 'DSM' or 'DTM'.")
  }

  if (AHN == latest_pdok_version) {
    info <- paste("Selected AHN4", dem, "from PDOK with resolution of", resolution$res, "m.", sep = " ")
  } else if (AHN == "AHN1") {
    if (tolower(dem) == "dsm") {
      warning(paste("There is no DSM available for this dataset. DTM (maaiveld)", resolution$res, "m will be used.", sep = " "))
    }
    info <- paste(AHN, resolution$res, "m resolution DTM (maaiveld) selected.", sep = " ")
    dem <- ""
  } else if (AHN == "AHN2") {
    if (resolution$res == 5) {
      dem <- ""
      info <- "AHN2 5 m resolution DTM (maaiveld) selected."
    } else if (resolution$res == 0.5) {
      if (tolower(dem) == "dtm") {
        if (interpolate == TRUE) {
          dem <- "int"
          info <- "AHN2 0.5 m resolution DTM (maaiveld) interpolated (opgevuld) selected."
        } else if (interpolate == FALSE) {
          dem <- "non"
          info <- "AHN2 0.5 m resolution DTM (maaiveld) niet opgevuld selected."
        } else {
          stop("No correct interpolated parameter is provided. Please set it to 'TRUE' or 'FALSE'.")
        }
      } else if (tolower(dem) == "dsm") {
        dem <- "ruw"
        info <- "AHN2 0.5 m resolution DSM (ruw) selected."
      }
    }
  } else if (AHN == "AHN3") {
    if (tolower(dem) == "dtm") {
      specs <- "(maaiveld)"
    } else if (tolower(dem) == "dsm") {
      specs <- "(ruw)"
    }
    info <- paste(AHN, resolution$res, "m resolution", toupper(dem), specs, "selected.", sep = " ")
  }
  message(info)
  return(dem)
}