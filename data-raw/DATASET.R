## code to prepare `DATASET` dataset goes here

ngr <- "https://service.pdok.nl/rws/ahn"

download_ahn_url <- "https://service.pdok.nl/rws/ahn/atom/downloads/"

ahn_bladIndex <- sf::st_read("data-raw/kaartbladindex.gpkg")

epsg_rd <- sf::st_crs(28992)
epsg_wgs <- sf::st_crs(4326)

list_ahn_letters <- data.frame("letter" = c("", "i", "n", "r", "r5", "m", "u", "g", "c"), "dataset" = c("AHN1", "AHN2 DTM interpolated", "AHN2 DTM not interpolated", "AHN2 DSM (0.5m) OR AHN3 DSM (0.5m) OR AHN4 DSM (0.5m)", "AHN2 DSM (5m) OR AHN3 DSM (5m) OR AHN4 DSM (5m)","AHN3 DTM OR AHN4 DTM", "uitgefilterd point clouds for AHN1 or AHN2", "gefilterd point clouds for AHN1 or AHN2", "AHN3 (point clouds) OR AHN4 (point clouds)"), stringsAsFactors = FALSE)

default.output.dir <- "AHN_output"

default.sheets.dir <- "AHN_sheets"

## retired URLs
# download_ahn3_url <- "https://download.pdok.nl/rws/ahn3/v1_0/"
# download_ahn4_url <- "https://download.pdok.nl/rws/ahn4/v1_0/"

# ahn1_bladIndex <- sf::st_read("data-raw/AHN1_bladIndex.gpkg")
# ahn2_bladIndex <- sf::st_read("data-raw/AHN2_bladIndex.gpkg")
# ahn3_bladIndex <- sf::st_read("data-raw/AHN3_bladIndex.gpkg")
# ahn4_bladIndex <- sf::st_read("data-raw/AHN4_bladIndex.gpkg")
# list_ahn_letters <- data.frame("letter" = c("", "i", "n", "r", "r5", "m", "u", "g", "c"), "dataset" = c("AHN1", "AHN2 DTM interpolated", "AHN2 DTM not interpolated", "AHN2 DSM (0.5m) OR AHN3 DSM (0.5m) OR AHN4 DSM (0.5m)", "AHN2 DSM (5m) OR AHN3 DSM (5m) OR AHN4 DSM (5m)","AHN3 DTM OR AHN4 DTM", "uitgefilterd point clouds for AHN1 or AHN2", "gefilterd point clouds for AHN1 or AHN2", "AHN3 (point clouds) OR AHN4 (point clouds)"), stringsAsFactors = FALSE)

usethis::use_data(ngr, download_ahn_url, ahn_bladIndex, epsg_rd, epsg_wgs, list_ahn_letters, default.output.dir, default.sheets.dir, overwrite = TRUE, internal = TRUE)
