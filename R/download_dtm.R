#'Download DTM
#'
#'@title Download DTM
#'@description Download DTM
#'download_dtm(name, output.dir, AHN = "AHN3", dem = "DSM", resolution = 0.5, bladnrs, area, interpolate = TRUE, sheets.keep = TRUE)
#'@param name Optional. Give a name of the specified area.
#'@param output.dir Required. Working directory.
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5 and 100 m.
#'@param bladnrs Required. Blad numbers.
#'@param area Required area to be downloaded
#'@param radius Radius of circle or squared BBOX in meters.
#'@param interpolate Default TRUE. Only applicable for AHN2 DTM. It decides if you want the interpolated version of the AHN2 or not.
#'@param sheets.location Optional. Default is the 'AHN_sheets' directory in working directory.. Set directory where all the AHN sheets are stored or will be stored.  Always use the correct directory structure and capitalization within the selected directory when pre-existing sheets will be used. Example directory structure: 'AHN_sheets/AHN3/DSM' or 'AHN_sheets/AHN2/DTM'. Only use extracted files in their original name after download.
#'@param sheets.keep Default TRUE. Only applicable if sheets.method is set to TRUE. Set to FALSE if you want to delete the downloaded sheets (structure).
#'@author Jelle Stuurman
#'@source <https://www.pdok.nl/datasets>
#'@return GeoTIFF of DTM AHN area
download_dtm <- function(name, output.dir, AHN = "AHN3", dem = "DSM", resolution = 0.5, radius, bladnrs, area, interpolate = TRUE, sheets.location = sheets.location, sheets.keep = TRUE){
  if(length(bladnrs) == 0){
    stop("No sheets were found within the area. Please check your input area.")
  }
  dwnld <- FALSE

  if(tolower(AHN) == "ahn3"){
    ahn_atomFeed_BaseUrl <- download_ahn3_url
  } else {
    ahn_atomFeed_BaseUrl <- paste(ngr, "/", tolower(AHN), "/extract/", tolower(AHN), "_", sep="")
  }
  if(missing(resolution) == TRUE){
    resolution = ""
  }
  my_resolution<- get_resolution(AHN = AHN, resolution)

  indiv_dtm_rasters <- list()

  if(radius == ""){
    radiusText <- ""
    overwriteText <- ""
  } else {
    radiusText <- paste0(radius, "m_")
    overwriteText <- paste0("(", radius, "m)")
  }

  #set or create sheets directory
  sheets_directory <- paste(sheets.location, default.sheets.dir, sep="/")
  #create directory if not found
  if(!dir.exists(sheets_directory)){
    dir.create(sheets_directory, showWarnings = FALSE)
  }

  #name directory
  # name_directory <- paste(output.dir, name, sep="/")
  # if (!dir.exists(name_directory)){
  #   dir.create(name_directory)
  # }

  #ahn directory
  ahn_directory <- paste(sheets_directory, AHN, sep="/")
  if(!dir.exists(ahn_directory)){
    dir.create(ahn_directory, showWarnings = FALSE)
  }

  #DTM directory
  ahn_dtm_directory <- paste(ahn_directory, "DTM", sep="/")
  if(!dir.exists(ahn_dtm_directory)){
    dir.create(ahn_dtm_directory, showWarnings = FALSE)
  }

  #print(paste0("Destination directory of DTM sheet: ", ahn_dtm_directory))
  if(output.dir != tempdir()){
    print(paste0("Destination directory of output AHN area: ", output.dir))
  }

  print(sprintf("Found %i sheet(s) with name(s):", length(bladnrs)))
  for(b in bladnrs){
    print(b)
  }

  ahn_dtm_file_paths <- c()
  ahn_dtm_letter <- get_ahn_letter(AHN = AHN, dem = dem, resolution = my_resolution$res, interpolate = interpolate, method = "raster")
  for(t in 1:length(bladnrs)){
    #AHN2: http://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_5m/ahn2_5_01cz1.tif.zip
    #AHN2: https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_05m_non/i32cn1.tif.zip
    #AHN2: https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_05m_int/i32cn1.tif.zip
    #AHN3: https://geodata.nationaalgeoregister.nl/ahn3/extract/ahn3_05m_dtm/M_32CN1.ZIP
    if(tolower(AHN) == "ahn3"){
      tifZip = ".ZIP"
      ahn_dtm_naming <- paste0("_dtm/", ahn_dtm_letter,"_")
      ahn_dtm_downloadLink <- paste(ahn_atomFeed_BaseUrl, my_resolution$res_name, ahn_dtm_naming,  toupper(bladnrs[[t]]), tifZip, sep="")
      sheetFileNameTif <- paste0(ahn_dtm_letter, "_", toupper(bladnrs[[t]]), ".tif")
    } else if(tolower(AHN) == "ahn2"){
      tifZip = ".tif.zip"
      if(my_resolution$res == 0.5){
        if(interpolate == TRUE){
          ahn_dtm_naming <- paste0("_int/",  ahn_dtm_letter)
          sheetFileNameTif <- paste0(ahn_dtm_letter, tolower(bladnrs[[t]]), ".tif")
        } else {
          ahn_dtm_naming <- paste0("_non/", ahn_dtm_letter)
          sheetFileNameTif <- paste0(ahn_dtm_letter, tolower(bladnrs[[t]]), ".tif")
        }
      } else if (my_resolution$res == 5){
        ahn_dtm_naming <- paste0("/", "ahn2_5_")
        sheetFileNameTif <- paste0("ahn2_5_", ahn_dtm_letter, tolower(bladnrs[[t]]), ".tif")
      }
      ahn_dtm_downloadLink <- paste(ahn_atomFeed_BaseUrl, my_resolution$res_name, ahn_dtm_naming,  tolower(bladnrs[[t]]), tifZip, sep="")
    } else if(tolower(AHN) == "ahn1" ){
      tifZip = ".tif.zip"
      if(my_resolution$res == 5){
        #http://geodata.nationaalgeoregister.nl/ahn1/extract/ahn1_5m/01cz2.tif.zip
        ahn_dtm_naming <- paste0("5m/",  ahn_dtm_letter)
        sheetFileNameTif <- paste0(ahn_dtm_letter, tolower(bladnrs[[t]]), ".tif")
        ahn_dtm_downloadLink <- paste(ahn_atomFeed_BaseUrl, ahn_dtm_naming,  tolower(bladnrs[[t]]), tifZip, sep="")
      # } else if(my_resolution$res == 25){
      #   #http://geodata.nationaalgeoregister.nl/ahn1/extract/ahn1_25m/ahn1_25.tif.zip
      #   ahn_dtm_naming <- paste0("25m/",  ahn_dtm_letter)
      #   sheetFileNameTif <- paste0("ahn_25", ".tif")
      #   ahn_dtm_downloadLink <- paste(ahn_atomFeed_BaseUrl, ahn_dtm_naming, "ahn1_25", tifZip, sep="")
      } else if(my_resolution$res == 100){
        #http://geodata.nationaalgeoregister.nl/ahn1/extract/ahn1_100m/ahn1_100.tif.zip
        ahn_dtm_naming <- paste0("100m/",  ahn_dtm_letter)
        sheetFileNameTif <- paste0("ahn_100", ".tif")
        ahn_dtm_downloadLink <- paste(ahn_atomFeed_BaseUrl, ahn_dtm_naming, "ahn1_100", tifZip, sep="")
      }
    }
    ahn_dtm_file_path <- paste(ahn_dtm_directory, "/", sheetFileNameTif, sep="")
    ahn_dtmZip_file_path <- paste(ahn_dtm_directory, "/", ahn_dtm_letter, bladnrs[[t]], ".zip", sep="")
    #check if sheet exists
    if(!file.exists(ahn_dtm_file_path)){
      print("Downloading DTM sheets...")
      #print(ahn_dtm_downloadLink)
      utils::download.file(url = ahn_dtm_downloadLink, destfile = ahn_dtmZip_file_path, mode = "wb")
      utils::unzip(zipfile = ahn_dtmZip_file_path, overwrite = TRUE, exdir = ahn_dtm_directory)
      if(tolower(AHN) == "ahn1"){
        file.rename(paste0(toupper(bladnrs[[t]]), ".tif"), ahn_dtm_file_path)
      }
      file.remove(ahn_dtmZip_file_path)
      dwnld <- TRUE
    } else {
        message(paste("Corresponding DTM sheet", bladnrs[[t]], "already exists and will be used.", sep=" "))
    }
    #print(ahn_dtm_file_path)
    ahn_dtm_file_paths <- cbind(ahn_dtm_file_paths, ahn_dtm_file_path)
    ahn_sheet_dtm <- raster::stack(x = ahn_dtm_file_path)
    raster::crs(ahn_sheet_dtm) <- epsg_rd
    print("Cropping DTM sheet to (part of) the area.")
    ahn_dtm_crop <- raster::crop(x = ahn_sheet_dtm, y = area)
    indiv_dtm_rasters[[t]] <- ahn_dtm_crop

    #break loop if AHN1 and resolution is 100
    if(tolower(AHN) == "ahn1" && (my_resolution$res == 100)){
      break
    }
  }
  if(tolower(AHN) == "ahn2" && my_resolution$res == 5){
    #added here because letter was not needed fr download link but now sed to add in naming of output file.
    ahn_dtm_letter <- "r5"
  }
  ahn_dtm_raster_filename <- paste(output.dir, "/", name, "_", radiusText, tolower(ahn_dtm_letter), AHN , "_", my_resolution$res_name, "_DTM", '.tif', sep="")
  #print(ahn_dtm_raster_filename)
  if(file.exists(ahn_dtm_raster_filename)){
    warning(paste("Cropped DTM raster for ", name, " ", overwriteText, " already exists and was overwritten." ,sep =""))
    #file.remove(ahn_dtm_raster_filename)
  }
  indiv_dtm_rasters$filename <- paste(name, "_", AHN , "_dtm_ahn", '.tif', sep="")
  if(length(bladnrs) > 1){
    indiv_dtm_rasters$overwrite <- TRUE
    print("Merging all DTM rasters...")
    ahn_dtm_raster <- do.call(raster::merge, indiv_dtm_rasters)
    raster::crs(ahn_dtm_raster) <- epsg_rd
    ahn_dtm_mask <- raster::mask(x = ahn_dtm_raster, mask = area, filename = ahn_dtm_raster_filename, overwrite = TRUE)
    #raster::writeRaster(x = ahn_dtm_mask, filename = ahn_dtm_raster_filename, overwrite = TRUE)
    file.remove(paste(name, "_", AHN , "_dtm_ahn.tif", sep=""))
    if(output.dir != tempdir()){
      print(paste0(AHN, " cropped DTM raster and saved on disk at: ", ahn_dtm_raster_filename))
    }
    print("Download, merge and crop of DTM rasters complete.")
  } else if(length(bladnrs) == 1){
    ahn_dtm_mask <- raster::mask(x = indiv_dtm_rasters[[1]], mask = area, filename = ahn_dtm_raster_filename, overwrite = TRUE)
    if(output.dir != tempdir()){
      print(paste0(AHN, " cropped DTM raster and saved on disk at: ", ahn_dtm_raster_filename))
    }
    print("Download and crop of DTM rasters complete.")
  }
  if(sheets.keep == FALSE && dwnld == TRUE){
    for(st in 1:length(ahn_dtm_file_paths)){
      file.remove(ahn_dtm_file_paths[st])
    }
  }
  if(output.dir!= tempdir()){
    print(ahn_dtm_raster_filename)
  }
  return(list("data" = ahn_dtm_mask, "fileDir" = output.dir, "fileName" = ahn_dtm_raster_filename))
}
