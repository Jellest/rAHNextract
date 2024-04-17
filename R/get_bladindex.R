#'@inheritParams ahn_area
#'@noRd
get_bladindex <- function(AHN, dem, resolution) {
  if (AHN == latest_pdok_version) {
    if (dem == "DSM") {
      bladIndex.sf <- ahn_DSM05_bladIndex
    } else if (dem == "DTM") {
      bladIndex.sf <- ahn_DTM05_bladIndex
    }
    #} else if (AHN == "AHN4") {
    # bladIndex.sf <- ahn4_bladIndex
  } else if (AHN == "AHN3") {
    bladIndex.sf <- ahn3_bladIndex
  } else if (AHN == "AHN2") {
    bladIndex.sf <- ahn2_bladIndex
  } else if (AHN == "AHN1") {
    bladIndex.sf <- ahn1_bladIndex
  }
  return(bladIndex.sf)
}