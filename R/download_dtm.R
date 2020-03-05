#'Download DTM
#'
#'@title Download DTM
#'@description Download DTM
#'@param name Optional. Give a name of the specified area.
#'@param wd Required. Working directory.
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5, 25, and 100 m.
#'@param bladnrs Required. Blad numbers.
#'@param area Required area to be downloaded
#'@param interpolate Default TRUE. Olny applicable for AHN2 DTM. It decides if you want the interpolated version of the AHN2 or not.
#'@param delete.sheets Deault TRUE. Only applicable if sheets is set to TRUE. Set to FALSE if you want to keep the downloaded sheets (kaartbladen).
#'@param redownload Deafult FALSE. nly applicable if sheets is set to TRUE. Set to TRUE if you want to redownload the sheets (kaartbladen)
#'@author Jelle Stuurman
#'@source <https://www.pdok.nl/datasets>
#'download_dtm(name, wd, AHN = "AHN3", dem = "dsm", resolution = 0.5, bladnrs, area, interpolate = TRUE, delete.sheets = TRUE, redownload = FALSE)
#'@return .tif of DTM AHN area

download_dtm <- function(name, wd, AHN = "AHN3", dem = "dsm", resolution = 0.5, bladnrs, area, interpolate = TRUE, delete.sheets = TRUE, redownload = FALSE){
  ahn_atomFeed_BaseUrl <- paste(ngr, "/", tolower(AHN), "/extract/", tolower(AHN), "_", sep="")
  my_resolution<- get_resolution(AHN = AHN, resolution)
  indiv_dtm_rasters <- list()

  #name directory
  name_directory <- paste(wd, name, sep="/")
  if (!dir.exists(name_directory)){
    dir.create(name_directory)
  }

  #sheets directory
  sheets_directory <- paste(wd, "sheets", sep="/")
  if(!dir.exists(sheets_directory)){
    dir.create(sheets_directory, showWarnings = FALSE)
  }

  #ahn directory
  ahn_directory <- paste(sheets_directory, AHN, sep="/")
  if(!dir.exists(ahn_directory)){
    dir.create(ahn_directory, showWarnings = FALSE)
  }

  #dtm directory
  ahn_dtm_directory <- paste(ahn_directory, "dtm", sep="/")
  if(!dir.exists(ahn_dtm_directory)){
    dir.create(ahn_dtm_directory, showWarnings = FALSE)
  }
  print(ahn_dtm_directory)

  print(paste("Amount of sheets found:", length(bladnrs), sep=" "))
  for(b in bladnrs){
    print(b)
  }
  ahn_dtm_file_paths <- c()
  if(length(bladnrs) == 0){
    stop("No sheets were found within the area. Please check your input area.")
  }

  for(t in 1:length(bladnrs)){
    #AHN2: http://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_5m/ahn2_5_01cz1.tif.zip
    #AHN2: https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_05m_non/i32cn1.tif.zip
    #AHN2: https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_05m_int/i32cn1.tif.zip
    #AHN3: https://geodata.nationaalgeoregister.nl/ahn3/extract/ahn3_05m_dtm/M_32CN1.ZIP
    if(tolower(AHN) == "ahn3"){
      tifZip = ".ZIP"
      if(my_resolution$res == 5){
        ahn_dtm_letter <- "M5"
      } else {
        ahn_dtm_letter <- "M"
      }
      ahn_dtm_naming <- paste0("_dtm/", ahn_dtm_letter,"_")
      ahn_dtm_downloadLink <- paste(ahn_atomFeed_BaseUrl, my_resolution$res_name, ahn_dtm_naming,  toupper(bladnrs[[t]]), tifZip, sep="")
      sheetFileNameTif <- paste0(ahn_dtm_letter, "_", toupper(bladnrs[[t]]), ".tif")
      ahn_dtm_file_path <- paste(ahn_dtm_directory, "/", sheetFileNameTif, sep="")
    } else if(tolower(AHN) == "ahn2"){
      tifZip = ".tif.zip"
      if(my_resolution$res == 0.5){
        if(interpolate == TRUE){
          ahn_dtm_letter <- "i"
          ahn_dtm_naming <- paste0("_int/",  ahn_dtm_letter)
          sheetFileNameTif <- paste0(ahn_dtm_letter, tolower(bladnrs[[t]]), ".tif")
        } else {
          ahn_dtm_letter <- "n"
          ahn_dtm_naming <- paste0("_non/", ahn_dtm_letter)
          sheetFileNameTif <- paste0(ahn_dtm_letter, tolower(bladnrs[[t]]), ".tif")
        }
      } else if (my_resolution$res == 5){
        ahn_dtm_letter <- ""
        ahn_dtm_naming <- paste0("/", "ahn2_5_")
        sheetFileNameTif <- paste0("ahn2_5_", tolower(bladnrs[[t]]), ".tif")
      }
      ahn_dtm_downloadLink <- paste(ahn_atomFeed_BaseUrl, my_resolution$res_name, ahn_dtm_naming,  tolower(bladnrs[[t]]), tifZip, sep="")
      ahn_dtm_file_path <- paste(ahn_dtm_directory, "/", sheetFileNameTif, sep="")
    } else if(tolower(AHN) == "ahn1" ){
      tifZip = ".tif.zip"
      if(my_resolution$res == 5){
        #http://geodata.nationaalgeoregister.nl/ahn1/extract/ahn1_5m/01cz2.tif.zip
        ahn_dtm_letter <- ""
        ahn_dtm_naming <- paste0("5m/",  ahn_dtm_letter)
        sheetFileNameTif <- paste0(ahn_dtm_letter, tolower(bladnrs[[t]]), ".tif")
        ahn_dtm_downloadLink <- paste(ahn_atomFeed_BaseUrl, ahn_dtm_naming,  tolower(bladnrs[[t]]), tifZip, sep="")
        ahn_dtm_file_path <- paste(ahn_dtm_directory, "/", sheetFileNameTif, sep="")
      } else if(my_resolution$res == 25){
        #http://geodata.nationaalgeoregister.nl/ahn1/extract/ahn1_25m/ahn1_25.tif.zip
        ahn_dtm_letter <- ""
        ahn_dtm_naming <- paste0("25m/",  ahn_dtm_letter)
        sheetFileNameTif <- paste0("ahn_25", ".tif")
        ahn_dtm_downloadLink <- paste(ahn_atomFeed_BaseUrl, ahn_dtm_naming, "ahn1_25", tifZip, sep="")
        ahn_dtm_file_path <- paste(ahn_dtm_directory, "/", sheetFileNameTif, sep="")
      } else if(my_resolution$res == 100){
        #http://geodata.nationaalgeoregister.nl/ahn1/extract/ahn1_100m/ahn1_100.tif.zip
        ahn_dtm_letter <- ""
        ahn_dtm_naming <- paste0("100m/",  ahn_dtm_letter)
        sheetFileNameTif <- paste0("ahn_100", ".tif")
        ahn_dtm_downloadLink <- paste(ahn_atomFeed_BaseUrl, ahn_dtm_naming, "ahn1_100", tifZip, sep="")
        ahn_dtm_file_path <- paste(ahn_dtm_directory, "/", sheetFileNameTif, sep="")
      }
    }

    ahn_dtmZip_file_path <- paste(ahn_dtm_directory, "/", ahn_dtm_letter, bladnrs[[t]], ".zip", sep="")
    #check if sheet exists
    if(!file.exists(ahn_dtm_file_path)){
      print("Downloading DTM sheets...")
      #print(ahn_dtm_downloadLink)
      utils::download.file(ahn_dtm_downloadLink, destfile = ahn_dtmZip_file_path, mode = "wb")
      utils::unzip(ahn_dtmZip_file_path, overwrite = TRUE, exdir = ahn_dtm_directory)
      file.remove(ahn_dtmZip_file_path)
    } else {
      if(redownload == TRUE){
        print("Redownloading DTM sheets...")
        file.remove(paste0(ahn_dtm_directory, "/", sheetFileNameTif))
        utils::download.file(ahn_dtm_downloadLink, destfile = ahn_dtmZip_file_path, mode = "wb")
        utils::unzip(ahn_dtmZip_file_path, overwrite = TRUE, exdir = ahn_dtm_directory)
        file.remove(ahn_dtmZip_file_path)
      } else {
        message(paste("Corresponding dtm sheet", bladnrs[[t]], "already exists and will be used.", sep=" "))
      }
    }

    print(ahn_dtm_file_path)
    ahn_dtm_file_paths <- cbind(ahn_dtm_file_paths, ahn_dtm_file_path)
    ahn_sheet_dtm <- raster::stack(ahn_dtm_file_path)
    raster::crs(ahn_sheet_dtm) <- epsg_rd
    print("Cropping dtm sheet to (part of) the area.")
    ahn_dtm_crop <- raster::crop(ahn_sheet_dtm, area)
    indiv_dtm_rasters[[t]] <- ahn_dtm_crop

    #break loop if AHN1 and resolution is 25 or 100
    if(tolower(AHN) == "ahn1" && (my_resolution$res == 25 || my_resolution$res == 100)){
      break
    }
  }

  ahn_dtm_raster_filename <- paste(name_directory, "/", name, "_", AHN , "_", ahn_dtm_letter, "_", my_resolution$res_name, "_DTM", '.tif', sep="")
  if(file.exists(ahn_dtm_raster_filename)){
    warning(paste("Cropped DTM raster for", name, "already exists and was overwritten." ,sep =" "))
    #file.remove(ahn_dtm_raster_filename)
  }
  indiv_dtm_rasters$filename <- paste(name, "_", AHN , "_dtm_ahn", '.tif', sep="")
  if(length(bladnrs) > 1){
    indiv_dtm_rasters$overwrite <- TRUE
    print("Merging all dtm rasters...")
    print(ahn_dtm_raster_filename)
    ahn_dtm_raster <- do.call(raster::merge, indiv_dtm_rasters)
    raster::crs(ahn_dtm_raster) <- epsg_rd
    ahn_dtm_mask <- raster::mask(ahn_dtm_raster, area)
    raster::writeRaster(ahn_dtm_mask, filename = ahn_dtm_raster_filename, overwrite = TRUE)
    print(paste0(AHN, " cropped DTM raster saved on disk at: ", ahn_dtm_raster_filename))
    file.remove(paste(name, "_", AHN , "_dtm_ahn.tif", sep=""))
    print(paste0(AHN, " cropped DTM raster and saved on disk at: ", ahn_dtm_raster_filename))
    print("Download and merge of dtm rasters complete.")
  } else if(length(bladnrs) == 1){
    ahn_dtm_raster <- indiv_dtm_rasters[[1]]
    raster::crs(ahn_dtm_raster) <- epsg_rd
    ahn_dtm_mask <- raster::mask(ahn_dtm_raster, area)
    raster::writeRaster(ahn_dtm_mask, ahn_dtm_raster_filename, overwrite = TRUE)
    print(paste0(AHN, " cropped DTM raster and saved on disk at: ", ahn_dtm_raster_filename))
    print("Download of dtm rasters complete.")
  }

  if(delete.sheets == TRUE){
    for(ft in 1:length(ahn_dtm_file_paths)){
      file.remove(ahn_dtm_file_paths[ft])
    }
  }
  return(ahn_dtm_mask)
}
