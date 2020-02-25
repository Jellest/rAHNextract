#'Download point cloud
#'
#'@title Download point cloud
#'@description Download DSM
#'@param name Optional. Give a name of the specified area.
#'@param wd Required. Working directory.
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param bladnrs Required. Blad numbers.
#'@param area Required area to be downloaded
#'@param interpolate Default TRUE. Olny applicable for AHN2 DTM. It decides if you want the interpolated version of the AHN2 or not.
#'@param delete.sheets Deault TRUE. Only applicable if sheets is set to TRUE. Set to FALSE if you want to keep the downloaded sheets (kaartbladen).
#'@param redownload Deafult FALSE. nly applicable if sheets is set to TRUE. Set to TRUE if you want to redownload the sheets (kaartbladen)
#'@author Jelle Stuurman
#'@source <https://www.pdok.nl/datasets>
#'download_point_cloud(name, wd, AHN = "AHN3", bladnrs, area, interpolate = TRUE, delete.sheets = TRUE, redownload = FALSE)
#'@return .tif of DSM AHN area

download_point_cloud <- function(name, wd, AHN = "AHN3", bladnrs, area, interpolate = TRUE, delete.sheets = TRUE, redownload = FALSE){
  indiv_pc_rasters <- list()
  ahn_atomFeed_BaseUrl <- paste(ngr, "/", tolower(AHN), "/extract/", tolower(AHN), "_", sep="")

  ahn_pc_directory <- paste(wd, name, sep="/")

  ahn_directory <- paste(wd, AHN, sep="/")
  if(!dir.exists(ahn_directory)){
    dir.create(paste(wd, AHN, sep="/"), showWarnings = FALSE)
  }

  #dtm directory
  ahn_pc_directory <- paste(ahn_directory, "pc", sep="/")
  if(!dir.exists(ahn_pc_directory)){
    dir.create(paste(ahn_directory, "pc", sep="/"), showWarnings = FALSE)
  }
  print(ahn_pc_directory)

  print(paste("Amount of sheets found:", length(bladnrs), sep=" "))
  for(b in bladnrs){
    print(b)
  }
  ahn_pc_file_paths <- c()
  if(length(bladnrs) == 0){
    stop("No sheets were found within the area. Please check your input area.")
  }
  for(r in 1:length(bladnrs)){
    if(tolower(AHN) == "ahn3"){
      #ahn3: https://geodata.nationaalgeoregister.nl/ahn3/extract/ahn3_laz/C_44FN2.LAZ
      lasZIP = ".LAZ"
      ahn_pc_letter <- "C"
      ahn_pc_naming <- paste0("laz/", ahn_pc_letter,"_")
      ahn_pc_downloadLink <- paste(ahn_atomFeed_BaseUrl, ahn_pc_naming,  toupper(bladnrs[[r]]), lasZIP, sep="")
      pcSheetFileNameTif <- paste0(ahn_pc_letter, tolower(bladnrs[[r]]), ".LAZ")
      ahn_pc_file_path <- paste(ahn_pc_directory, "/", pcSheetFileNameTif, sep="")
    } else if(tolower(AHN) == "ahn2"){
      #ahn2u: https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_uitgefilterd/g01cz1.laz.zip
      #ahn2g: https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_gefilterd/g01cz1.laz.zip
      lasZIP = ".laz.zip"
      if(interpolate == TRUE){
        ahn_pc_letter <- "g"
        ahn_pc_naming <- "gefilterd/g"
      } else {
        ahn_pc_letter <- "u"
        ahn_pc_naming <- "uitgefilterd/u"
      }
      ahn_pc_downloadLink <- paste(ahn_atomFeed_BaseUrl, ahn_pc_naming,  tolower(bladnrs[[r]]), lasZIP, sep="")
      pcSheetFileNameTif <- paste0(ahn_pc_letter, tolower(bladnrs[[r]]), ".LAZ")
      ahn_pc_file_path <- paste(ahn_pc_directory, "/", pcSheetFileNameTif,sep="")
    } else if(tolower(AHN) == "ahn1"){
      # #ahn1u: https://geodata.nationaalgeoregister.nl/ahn1/extract/ahn1_uitgefilterd/01cz1.laz.zip
      # #ahn1g: https://geodata.nationaalgeoregister.nl/ahn1/extract/ahn1_gefilterd/01cz1.laz.zip
      lasZIP = ".laz.zip"
      if(interpolate == TRUE){
        ahn_pc_letter <- "g"
        ahn_pc_naming <- "gefilterd/"
      } else {
        ahn_pc_letter <- "u"
        ahn_pc_naming <- "uitgefilterd/"
      }
      ahn_pc_downloadLink <- paste(ahn_atomFeed_BaseUrl, ahn_pc_naming,  tolower(bladnrs[[r]]), lasZIP, sep="")
      pcSheetFileNameTif <- paste0(ahn_pc_letter, tolower(bladnrs[[r]]), ".LAZ")
      ahn_pc_file_path <- paste(ahn_pc_directory, "/", pcSheetFileNameTif,sep="")
    }
    if(tolower(AHN) == "ahn2" || tolower(AHN) == "ahn1"){
      fileext <- ".zip"
    } else {
      fileext <- ".LAZ"
    }
    ahn_pcZip_file_path <- paste(ahn_pc_directory, "/", ahn_pc_letter, bladnrs[[r]], fileext, sep="")
    #check if sheet exists
    if(!file.exists(ahn_pc_file_path)){
      print("Downloading point cloud sheets...")
      print(ahn_pc_downloadLink)
      utils::download.file(ahn_pc_downloadLink, destfile = ahn_pcZip_file_path, mode="wb")
      if(tolower(AHN) == "ahn2" || tolower(AHN) == "ahn1")
        utils::unzip(ahn_pcZip_file_path, overwrite = TRUE, exdir = ahn_pc_directory)
        file.remove(ahn_pcZip_file_path)
    } else {
      if(redownload == TRUE){
        print("Redownloading DSM sheets...")
        file.remove(paste0(ahn_pc_directory, "/", pcSheetFileNameTif))
        utils::download.file(ahn_pc_downloadLink, destfile = ahn_pcZip_file_path)
        utils::unzip(ahn_pcZip_file_path, overwrite = TRUE, exdir = ahn_pc_directory)
        file.remove(ahn_pcZip_file_path)
      } else {
        message(paste("Corresponding DSM sheet", bladnrs[[r]], "already exists and will be used.", sep=" "))
      }
    }
    print(ahn_pc_file_path)

    read_pc(ahn_pc_file_path, area)
    #ahn_pc_file_paths <- cbind(ahn_pc_file_paths, ahn_pc_file_path)
    #ahn_sheet_pc <- raster::stack(ahn_pc_file_path)
    #raster::crs(ahn_sheet_pc) <- epsg_rd
    # print("Cropping pc sheet to (part of) the area.")
    # ahn_pc_crop <- raster::crop(ahn_sheet_pc, area)
    # indiv_pc_rasters[[r]] <- ahn_pc_crop
  }

  #ahn_pc_raster_filename <- paste(ahn_pc_directory, "/", name, "_", AHN , "_", ahn_pc_letter, "_pc", '.tif', sep="")
  # if(file.exists(ahn_pc_raster_filename)){
  #   warning(paste("Cropped DSM raster for", name, "already exists and was overwritten." ,sep =" "))
  #   file.remove(ahn_pc_raster_filename)
  # }
  # indiv_pc_rasters$filename <- paste(name, "_", AHN ,"_pc_ahn", '.tif', sep="")
  # if(length(bladnrs) > 1){
  #   indiv_pc_rasters$overwrite <- TRUE
  #   print("Merging all pc rasters...")
  #   print(ahn_pc_raster_filename)
  #   ahn_pc_raster <- do.call(merge, indiv_pc_rasters)
  #   raster::crs(ahn_pc_raster) <- epsg_rd
  #   #ahn_pc_raster <- projectRaster(ahn_pc_raster, ahn_pc_raster_filename, crs = epsg_rd, overwrite = TRUE)
  #   raster::writeRaster(ahn_pc_raster, filename = ahn_pc_raster_filename, overwrite = TRUE)
  #   print(paste0(AHN, " cropped DSM raster saved on disk at: ",ahn_pc_raster_filename))
  #   file.remove(paste(name, "_", AHN , "_pc_ahn.tif", sep=""))
  #   print("Download and merge of pc rasters complete.")
  # } else if(length(bladnrs) == 1){
  #   ahn_pc_raster <- indiv_pc_rasters[[1]]
  #   raster::crs(ahn_pc_raster) <- epsg_rd
  #   #ahn_pc_raster <- projectRaster(ahn_pc_raster, ahn_pc_raster_filename, crs = epsg_rd, overwrite = TRUE)
  #   raster::writeRaster(ahn_pc_raster, ahn_pc_raster_filename, overwrite = TRUE)
  #   print(paste0(AHN, " cropped DSM raster and saved on disk at: ",ahn_pc_raster_filename))
  #   print("Download of pc rasters complete.")
  # }

  if(delete.sheets == TRUE){
    for(fr in 1:length(ahn_pc_file_paths)){
      file.remove(ahn_pc_file_paths[fr])
    }
  }
  return()
}
