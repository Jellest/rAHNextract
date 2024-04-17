## code to prepare `DATASET` dataset goes here
#'@title create global variables
#'@noRd
define_globals <- function() {
  pdok_baseurl <- "https://service.pdok.nl"
  pdok_wcs_url <- paste0(pdok_baseurl, "/rws/ahn/wcs/v1_0?SERVICE=WCS&request=GetCapabilities")

  list_ahn_letters <- data.frame("letter" = c("", "i", "n", "r", "r5", "m", "u", "g", "c"), "dataset" = c("AHN1", "AHN2 DTM interpolated", "AHN2 DTM not interpolated", "AHN2 DSM (0.5m) OR AHN3 DSM (0.5m) OR AHN4 DSM (0.5m)", "AHN2 DSM (5m) OR AHN3 DSM (5m) OR AHN4 DSM (5m)", "AHN3 DTM OR AHN4 DTM", "uitgefilterd point clouds for AHN1 or AHN2", "gefilterd point clouds for AHN1 or AHN2", "AHN3 (point clouds) OR AHN4 (point clouds)"), stringsAsFactors = FALSE)

  epsg_rd <- sf::st_crs(28992)
  epsg_wgs <- sf::st_crs(4326)
  proj_rd <- "epsg:28992"
  proj_wgs <- "epsg:4326"

  RD_min <- list("X" = 482.06, "Y" = 306602.42)
  RD_max <- list("X" = 284182.97, "Y" = 637049.52)

  WGS_min <- list("LON" = 3.2, "LAT" = 50.75)
  WGS_max <- list("LON" = 7.22, "LAT" = 53.7)


  AHN_versions <- c("AHN5", "AHN4", "AHN3", "AHN2", "AHN1")
  AHN_05res_versions <- c("AHN4", "AHN3", "AHN2")
  latest_pdok_version <- "AHN4"
  operational_ahns_in_package <- c("AHN4")

  pdok_versions <- c("AHN4")
  non_pdok_versions <- c("AHN5", "AHN3", "AHN2", "AHN1")

  default.output.dir <- "AHN_output"

  default.sheets.dir <- "AHN_sheets"

  if ("AHN5" %in% operational_ahns_in_package) {
    ahn5_bladIndex <- ""
  } else {
    ahn5_bladIndex <- NULL
  }
  if ("AHN4" %in% operational_ahns_in_package) {
    ahn_DSM05_bladIndex <- sf::st_read("data-raw/AHN_bladIndexes.gpkg", layer = "AHN_DSM05_bladIndex_rd")
    ahn_DTM05_bladIndex <- sf::st_read("data-raw/AHN_bladIndexes.gpkg", layer = "AHN_DTM05_bladIndex_rd")
    ahn4_bladIndex <- "See 'ahn_DSM05_bladIndex' and 'ahn_DTM05_bladIndex' variables."
  } else {
    ahn_DSM05_bladIndex <- NULL
    ahn_DTM05_bladIndex <- NULL
    ahn4_bladIndex <- NULL
  }
  if ("AHN3" %in% operational_ahns_in_package) {
    ahn3_bladIndex <- sf::st_read("data-raw/AHN_bladIndexes.gpkg", layer = "AHN3_bladIndex_rd")
  } else {
    ahn3_bladIndex <- NULL
  }
  if ("AHN2" %in% operational_ahns_in_package) {
    ahn2_bladIndex <- sf::st_read("data-raw/AHN_bladIndexes.gpkg", layer = "AHN2_bladIndex_rd")
  } else {
    ahn2_bladIndex <- NULL
  }
  if ("AHN1" %in% operational_ahns_in_package) {
    ahn1_bladIndex <- sf::st_read("data-raw/AHN_bladIndexes.gpkg", layer = "AHN1_bladIndex_rd")
  } else {
    ahn1_bladIndex <- NULL
  }
  usethis::use_data(pdok_baseurl, pdok_wcs_url, ahn_DSM05_bladIndex, ahn_DTM05_bladIndex, ahn1_bladIndex, ahn2_bladIndex, ahn3_bladIndex, ahn4_bladIndex, ahn5_bladIndex, epsg_rd, epsg_wgs, proj_rd, proj_wgs, RD_min, RD_max, WGS_min, WGS_max, list_ahn_letters, AHN_versions, AHN_05res_versions, latest_pdok_version, operational_ahns_in_package, pdok_versions, non_pdok_versions, default.output.dir, default.sheets.dir, overwrite = TRUE, internal = TRUE)
}

define_globals()
