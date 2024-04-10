#'@description check the version of AHN that is called and whehter it is available within this package.
#'@inheritParams ahn_area
#'@noRd
check_ahn_version <- function(AHN) {
  AHN <- toupper(AHN)
  note <- FALSE
  if (AHN == "AHN" || AHN == latest_pdok_version) {
    if (AHN == "AHN") {
      note <- TRUE
    }
    xml_doc <- xml2::read_xml(pdok_wcs_url)

    # Extract the abstract content
    abstract <- xml2::xml_text(xml2::xml_find_first(x = xml_doc, xpath = ".//ows:Abstract", ns = xml2::xml_ns(xml_doc)))

    # Determine AHN version
    ahn_version <- base::regmatches(abstract, base::regexpr("(?<=versie\\s)\\d+", abstract, perl = TRUE))
    this_ahn <- paste0("AHN", ahn_version)

    if (!toupper(this_ahn) %in% pdok_versions) {
      stop(" The AHN version found is currently not supoorted at the moment with this package.")
    } else {
      AHN <- toupper(this_ahn)
      if (note == TRUE) {
        message(paste("The AHN version found through PDOK is:", AHN))
      }
      return(AHN)
    }
  } else if (AHN %in% pdok_versions) {
    return(AHN)
  } else if (AHN %in% operational_ahns_in_package) {
    stop("Currently there is no support to use other AHN versions than the AHN made available by PDOK.")
  } else {
    stop(" The provided AHN is not found. Please try another name.")
  }
}