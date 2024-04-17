#'@inheritParams ahn_area
#'@noRd
download_ahn_sheets <- function(name, AHN, dem, url, output.dir, sheets.dir, sheets.keep) {
  #print(paste0("Destination directory of DEM sheet: ", ahn_dem_directory))
  ahn_dem_raster_filename <- basename(url)
  kaartNadNr <- substr(ahn_dem_raster_filename, 3, nchar(ahn_dem_raster_filename) - 4)

  # determine sheets location
  if (grepl(output.dir, sheets.dir, fixed = TRUE) == FALSE) {
  } else {

  }

  if (sheets.dir != tempdir()) {
    #ahn directory
    ahn_directory <- paste(sheets.dir, AHN, sep = "/")
    if (dir.exists(ahn_directory) == FALSE) {
      dir.create(ahn_directory, showWarnings = FALSE)
    }

    #dem directory
    ahn_dem_directory <- paste(ahn_directory, toupper(dem), sep = "/")
    if (!dir.exists(ahn_dem_directory)) {
      dir.create(ahn_dem_directory, showWarnings = FALSE)
    }
    print(paste0("Destination directory of output AHN sheet ", ahn_dem_raster_filename, ": ", ahn_dem_directory))
  }

  ahn_dem_file_path <- paste(ahn_dem_directory, ahn_dem_raster_filename, sep = "/")

  #check if sheet exists
  downloaded <- FALSE
  if (!file.exists(ahn_dem_file_path)) {
    print(paste0("Downloading ", AHN, " ", dem, " sheet ", ahn_dem_raster_filename, "..."))
    utils::download.file(url = url, destfile = ahn_dem_file_path, mode = "wb", quiet = FALSE)
    downloaded <- TRUE
  } else {
    message(paste("Corresponding dem sheet at", ahn_dem_file_path, "already exists and will be used.", sep = " "))
    if (sheets.keep == FALSE) {
      warning(paste("Through the parameter `sheets.keep` is it provided that the sheet needs to be removed. The", AHN, dem, kaartNadNr, "sheet already existed on the filepath location and therefore it will not be removed. Only existing sheets that do not exist through the `sheets.dir` wil be removed when `sheets.keep` is set to 'FALSE'.", sep = " "))
    }
  }
  return(list("kaartBlad" = kaartNadNr, "filePath" = ahn_dem_file_path, "downloadedNow" = downloaded))
}
