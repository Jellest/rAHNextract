#'Download DSM
#'
#'@title Download DSM
#'@description Download DSM
#'@param name Optional. Give a name of the specified area.
#'@param wd Required. Working directory.
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5, 25, and 100 m.
#'@param badnrs Required. Blad numbers.
#'@param area Required area to be downloaded
#'@param interpolate Default TRUE. Olny applicable for AHN2 DTM. It decides if you want the interpolated version of the AHN2 or not.
#'@param delete.sheets Deault TRUE. Only applicable if sheets is set to TRUE. Set to FALSE if you want to keep the downloaded sheets (kaartbladen).
#'@param redownload Deafult FALSE. nly applicable if sheets is set to TRUE. Set to TRUE if you want to redownload the sheets (kaartbladen)
#'@author Jelle Stuurman
#'download_dsm(name, wd, AHN = "AHN3", dem = "dsm", resolution = 0.5, bladnrs, area, interpolate = TRUE, delete.sheets = TRUE, redownload = FALSE)
#'@return .tif of DSM AHN area

download_dsm <- function(name, wd, AHN = "AHN3", dem = "dsm", resolution = 0.5, bladnrs, area, interpolate = TRUE, delete.sheets = TRUE, redownload = FALSE){
  ahn_atomFeed_BaseUrl <- paste(ngr, "/", tolower(AHN), "/extract/", tolower(AHN), "_", sep="")
  my_resolution<- get_resolution(AHN = AHN, resolution)
  #download dsm AHN of corresponding resolultion
  indiv_dsm_rasters <- list()

  ahn_dsm_directory <- paste(wd, "dsm", sep="/")

  dir.create(paste(wd, "dsm", sep="/"), showWarnings = FALSE)
  print(ahn_dsm_directory)

  print(paste("Amount of sheets found:", length(bladnrs), sep=" "))
  for(b in bladnrs){
    print(b)
  }
  ahn_dsm_file_paths <- c()
  if(length(bladnrs) == 0){
    stop("No sheets were found within the area. Please check your input area.")
  }
  for(r in 1:length(bladnrs)){
    #ahn2: https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_05m_ruw/r32cn1.tif.zip
    #ahn3: https://geodata.nationaalgeoregister.nl/ahn3/extract/ahn3_05m_dsm/R_32CN1.ZIP
    if(tolower(AHN) == "ahn3"){
      tifZip = ".ZIP"
      if(my_resolution$res == 5){
        ahn_dsm_letter <- "R5"
      } else {
        ahn_dsm_letter <- "R"
      }
      ahn_dsm_naming <- paste0("_dsm/", ahn_dsm_letter,"_")
      ahn_dsm_downloadLink <- paste(ahn_atomFeed_BaseUrl, my_resolution$res_name, ahn_dsm_naming,  toupper(bladnrs[[r]]), tifZip, sep="")
      sheetFileNameTif <- paste0(ahn_dsm_letter, "_", toupper(bladnrs[[r]]), ".tif")
      ahn_dsm_file_path <- paste(ahn_dsm_directory, "/", sheetFileNameTif, sep="")
    } else if(tolower(AHN) == "ahn2"){
      tifZip = ".tif.zip"
      ahn_dsm_letter <- "r"
      ahn_dsm_naming <- paste0("_ruw/", ahn_dsm_letter)
      ahn_dsm_downloadLink <- paste(ahn_atomFeed_BaseUrl, my_resolution$res_name, ahn_dsm_naming,  tolower(bladnrs[[r]]), tifZip, sep="")
      sheetFileNameTif <- paste0(ahn_dsm_letter, tolower(bladnrs[[r]]), ".tif")
      ahn_dsm_file_path <- paste(ahn_dsm_directory, "/", sheetFileNameTif,sep="")
    } else if(tolower(AHN) == "ahn1"){
      stop("No DSM dataset exists for the AHN1. Please select DTM or choose AHN2 or AHN3.")
    }

    ahn_dsmZip_file_path <- paste(ahn_dsm_directory, "/", ahn_dsm_letter, bladnrs[[r]], ".ZIP", sep="")
    #check if sheet exists
    if(!file.exists(ahn_dsm_file_path)){
      print("Downloading DSM sheets...")
      download.file(ahn_dsm_downloadLink, destfile = ahn_dsmZip_file_path, mode="wb")
      unzip(ahn_dsmZip_file_path, overwrite = TRUE, exdir = ahn_dsm_directory)
      file.remove(ahn_dsmZip_file_path)
    } else {
      if(redownload == TRUE){
        print("Redownloading DSM sheets...")
        file.remove(paste0(ahn_dsm_directory, "/", sheetFileNameTif))
        download.file(ahn_dsm_downloadLink, destfile = ahn_dsmZip_file_path, mode = "wb")
        unzip(ahn_dsmZip_file_path, overwrite = TRUE, exdir = ahn_dsm_directory)
        file.remove(ahn_dsmZip_file_path)
      } else {
        message(paste("Corresponding DSM sheet", bladnrs[[r]], "already exists and will be used.", sep=" "))
      }
    }
    print(ahn_dsm_file_path)
    ahn_dsm_file_paths <- cbind(ahn_dsm_file_paths, ahn_dsm_file_path)
    ahn_sheet_dsm <-stack(ahn_dsm_file_path)
    raster::crs(ahn_sheet_dsm) <- epsg_rd
    raster::proj4string(ahn_sheet_dsm)<- epsg_rd
    print("Cropping dsm sheet to (part of) the area.")
    ahn_dsm_crop <- raster::crop(ahn_sheet_dsm, area)
    indiv_dsm_rasters[[r]] <- ahn_dsm_crop
  }

  ahn_dsm_raster_filename <- paste(ahn_dsm_directory, "/", name, "_", AHN , "_", ahn_dsm_letter, "_", my_resolution$res_name, "_dsm", '.tif', sep="")
  if(file.exists(ahn_dsm_raster_filename)){
    warning(paste("Cropped DSM raster for", name, "already exists and was overwritten." ,sep =" "))
    file.remove(ahn_dsm_raster_filename)
  }
  indiv_dsm_rasters$filename <- paste(name, "_", AHN ,"_dsm_ahn", '.tif', sep="")
  if(length(bladnrs) > 1){
    indiv_dsm_rasters$overwrite <- TRUE
    print("Merging all dsm rasters...")
    print(ahn_dsm_raster_filename)
    ahn_dsm_raster <- do.call(merge, indiv_dsm_rasters)
    raster::crs(ahn_dsm_raster) <- epsg_rd
    raster::proj4string(ahn_dsm_raster) <- epsg_rd
    #ahn_dsm_raster <- projectRaster(ahn_dsm_raster, ahn_dsm_raster_filename, crs = epsg_rd, overwrite = TRUE)
    raster::writeRaster(ahn_dsm_raster, filename = ahn_dsm_raster_filename, overwrite = TRUE)
    print(paste0(AHN, " cropped DSM raster saved on disk at: ",ahn_dsm_raster_filename))
    file.remove(paste(name, "_", AHN , "_dsm_ahn.tif", sep=""))
    print("Download and merge of dsm rasters complete.")
  } else if(length(bladnrs) == 1){
    ahn_dsm_raster <- indiv_dsm_rasters[[1]]
    raster::crs(ahn_dsm_raster) <- epsg_rd
    raster::proj4string(ahn_dsm_raster) <- epsg_rd
    #ahn_dsm_raster <- projectRaster(ahn_dsm_raster, ahn_dsm_raster_filename, crs = epsg_rd, overwrite = TRUE)
    raster::writeRaster(ahn_dsm_raster, ahn_dsm_raster_filename, overwrite = TRUE)
    print(paste0(AHN, " cropped DSM raster and saved on disk at: ",ahn_dsm_raster_filename))
    print("Download of dsm rasters complete.")
  }

  if(delete.sheets == TRUE){
    for(fr in 1:length(ahn_dsm_file_paths)){
      file.remove(ahn_dsm_file_paths[fr])
    }
  }
  return(ahn_dsm_raster)
}