#'Download DSM
#'
#'@title Download DSM
#'@description Download DSM
#'@param name Optional. Give a name of the specified area.
#'@param wd Required. Working directory.
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5, 25, and 100 m.
#'@param bladnrs Required. Blad numbers.
#'@param area Required area to be downloaded
#'@param radius Radius of circle or squared BBOX in meters.
#'@param interpolate Default TRUE. Only applicable for AHN2 DTM. It decides if you want the interpolated version of the AHN2 or not.
#'@param keep.sheets Default TRUE. Only applicable if method.sheets is set to TRUE. Set to FALSE if you want to delete the downloaded sheets (kaartbladen).
#'@param redownload Default FALSE. Only applicable if sheets is set to TRUE. Set to TRUE if you want to redownload the sheets (kaartbladen)
#'@author Jelle Stuurman
#'@source <https://www.pdok.nl/datasets>
#'download_dsm(name, wd, AHN = "AHN3", dem = "DSM", resolution = 0.5, bladnrs, area, interpolate = TRUE, keep.sheets = TRUE, redownload = FALSE)
#'@return .tif of DSM AHN area

download_dsm <- function(name, wd, AHN = "AHN3", dem = "DSM", resolution = 0.5, radius, bladnrs, area, interpolate = TRUE, keep.sheets = TRUE, redownload = FALSE){
  if(tolower(AHN) == "ahn3"){
    ahn_atomFeed_BaseUrl <- "https://download.pdok.nl/rws/ahn3/v1_0/"
  } else {
    ahn_atomFeed_BaseUrl <- paste(ngr, "/", tolower(AHN), "/extract/", tolower(AHN), "_", sep="")
  }
  my_resolution<- get_resolution(AHN = AHN, resolution)
  indiv_dsm_rasters <- list()

  if(radius == ""){
    radiusText <- ""
    overwriteText <- ""
  } else {
    radiusText <- paste0(radius, "m_")
    overwriteText <- paste0("(", radius, "m)")
  }

  #create sheets directory
  if(wd == structured_output_folder){
    sheets_wd <- wd
  } else {
    #temp or custom directory
    sheets_wd <- getwd()
  }

  sheets_directory <- paste(sheets_wd, "AHN_sheets", sep="/")
  if(!dir.exists(sheets_directory)){
    dir.create(sheets_directory, showWarnings = FALSE)
  }


  #name directory
  # name_directory <- paste(wd, name, sep="/")
  # if (!dir.exists(name_directory)){
  #   dir.create(name_directory)
  # }

  #ahn directory
  ahn_directory <- paste(sheets_directory, AHN, sep="/")
  if(!dir.exists(ahn_directory)){
    dir.create(ahn_directory, showWarnings = FALSE)
  }

  #DSM directory
  ahn_dsm_directory <- paste(ahn_directory, "DSM", sep="/")
  if(!dir.exists(ahn_dsm_directory)){
    dir.create(ahn_dsm_directory, showWarnings = FALSE)
  }

  print(ahn_dsm_directory)

  print(paste("Amount of sheets found:", length(bladnrs), sep=" "))
  for(b in bladnrs){
    print(b)
  }
  ahn_dsm_file_paths <- c()
  if(length(bladnrs) == 0){
    stop("No sheets were found within the area. Please check your input area.")
  }

  ahn_dsm_letter <- get_ahn_letter(AHN = AHN, dem = dem, resolution = my_resolution$res, interpolate = interpolate, method = "raster")
  for(r in 1:length(bladnrs)){
    #ahn2: https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_05m_ruw/r32cn1.tif.zip
    #ahn3: https://geodata.nationaalgeoregister.nl/ahn3/extract/ahn3_05m_dsm/R_32CN1.ZIP
    #ahn3: https://download.pdok.nl/rws/ahn3/v1_0/05m_dsm/R_45CZ1.ZIP
    if(tolower(AHN) == "ahn3"){
      tifZip = ".ZIP"
      ahn_dsm_naming <- paste0("_dsm/", ahn_dsm_letter,"_")
      ahn_dsm_downloadLink <- paste(ahn_atomFeed_BaseUrl, my_resolution$res_name, ahn_dsm_naming,  toupper(bladnrs[[r]]), tifZip, sep="")
      sheetFileNameTif <- paste0(ahn_dsm_letter, "_", toupper(bladnrs[[r]]), ".tif")
    } else if(tolower(AHN) == "ahn2"){
      tifZip = ".tif.zip"
      ahn_dsm_naming <- paste0("_ruw/", ahn_dsm_letter)
      ahn_dsm_downloadLink <- paste(ahn_atomFeed_BaseUrl, my_resolution$res_name, ahn_dsm_naming,  tolower(bladnrs[[r]]), tifZip, sep="")
      sheetFileNameTif <- paste0(ahn_dsm_letter, tolower(bladnrs[[r]]), ".tif")
    } else if(tolower(AHN) == "ahn1"){
      stop("No DSM dataset exists for the AHN1. Please select DTM or choose AHN2 or AHN3.")
    }

    ahn_dsm_file_path <- paste(ahn_dsm_directory, "/", sheetFileNameTif,sep="")
    ahn_dsmZip_file_path <- paste(ahn_dsm_directory, "/", ahn_dsm_letter, bladnrs[[r]], ".ZIP", sep="")
    #check if sheet exists
    if(!file.exists(ahn_dsm_file_path)){
      print("Downloading DSM sheets...")
      utils::download.file(ahn_dsm_downloadLink, destfile = ahn_dsmZip_file_path, mode="wb")
      utils::unzip(ahn_dsmZip_file_path, overwrite = TRUE, exdir = ahn_dsm_directory)
      file.remove(ahn_dsmZip_file_path)
    } else {
      if(redownload == TRUE){
        print("Redownloading DSM sheets...")
        file.remove(paste0(ahn_dsm_directory, "/", sheetFileNameTif))
        utils::download.file(ahn_dsm_downloadLink, destfile = ahn_dsmZip_file_path, mode = "wb")
        utils::unzip(ahn_dsmZip_file_path, overwrite = TRUE, exdir = ahn_dsm_directory)
        file.remove(ahn_dsmZip_file_path)
      } else {
        message(paste("Corresponding DSM sheet", bladnrs[[r]], "already exists and will be used.", sep=" "))
      }
    };
    #print(ahn_dsm_file_path)
    ahn_dsm_file_paths <- cbind(ahn_dsm_file_paths, ahn_dsm_file_path)
    ahn_sheet_dsm <- raster::stack(ahn_dsm_file_path)
    raster::crs(ahn_sheet_dsm) <- epsg_rd
    print("Cropping DSM sheet to (part of) the area.")
    ahn_dsm_crop <- raster::crop(ahn_sheet_dsm, area)
    indiv_dsm_rasters[[r]] <- ahn_dsm_crop
  }

  ahn_dsm_raster_filename <- paste(wd, "/", name, "_", radiusText, tolower(ahn_dsm_letter), AHN , "_", my_resolution$res_name, "_DSM", '.tif', sep="")
  #print(ahn_dsm_raster_filename)
  if(file.exists(ahn_dsm_raster_filename)){
    warning(paste("Cropped DSM raster for ", name, "_", overwriteText, " already exists and was overwritten." ,sep =""))
    #file.remove(ahn_dsm_raster_filename)
  }
  indiv_dsm_rasters$filename <- paste(name, "_", AHN ,"_dsm_ahn", '.tif', sep="")
  if(length(bladnrs) > 1){
    indiv_dsm_rasters$overwrite <- TRUE
    print("Merging all DSM rasters...")
    ahn_dsm_raster <- do.call(raster::merge, indiv_dsm_rasters)
    raster::crs(ahn_dsm_raster) <- epsg_rd
    ahn_dsm_mask <- raster::mask(ahn_dsm_raster, area)
    raster::writeRaster(ahn_dsm_mask, filename = ahn_dsm_raster_filename, overwrite = TRUE)
    file.remove(paste(name, "_", AHN , "_dsm_ahn.tif", sep=""))
    if(wd != tempdir()){
      print(paste0(AHN, " cropped DSM raster and saved on disk at: ", ahn_dsm_raster_filename))
    }
    print("Download merge and crop of DSM rasters complete.")
  } else if(length(bladnrs) == 1){
    ahn_dsm_raster <- indiv_dsm_rasters[[1]]
    raster::crs(ahn_dsm_raster) <- epsg_rd
    ahn_dsm_mask <- raster::mask(ahn_dsm_raster, area)
    raster::writeRaster(ahn_dsm_mask, ahn_dsm_raster_filename, overwrite = TRUE)
    if(wd != tempdir()){
      print(paste0(AHN, " cropped DSM raster and saved on disk at: ",ahn_dsm_raster_filename))
    }
    print("Download and crop of DSM rasters complete.")
  }

  if(keep.sheets == FALSE){
    for(fr in 1:length(ahn_dsm_file_paths)){
      file.remove(ahn_dsm_file_paths[fr])
    }
    ahn_dsm_dir <- list.files(ahn_dsm_directory, include.dirs = TRUE)
    if(lengths(ahn_dsm_dir) == 0){
      unlink(ahn_dsm_directory)
    }
    ahn_dir <- list.files(ahn_directory, include.dirs = TRUE)
    if(lengths(ahn_dir) == 0){
      unlink(ahn_directory)
    }
    ahn_dsm_dir <- list.files(ahn_dsm_directory, include.dirs = TRUE)
    if(lengths(ahn_dsm_dir) == 0){
      unlink(ahn_dsm_directory)
    }
    sheets_dir <- list.files(sheets_directory, include.dirs = TRUE)
    if(lengths(sheets_dir) == 0){
      unlink(sheets_directory)
    }
  }
  return(list("data" = ahn_dsm_mask, "fileDir" = wd, "fileName" = ahn_dsm_raster_filename))
}
