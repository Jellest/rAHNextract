#'Download point cloud
#'
#'@title Download point cloud
#'@description Download DSM
#'@param name Optional. Give a name of the specified area.
#'@param wd Required. Working directory.
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param bladnrs Required. Blad numbers.
#'@param area Required area to be downloaded
#'@param bboxes Optional. individual bboxes of cropped sheets
#'@param filtered Default TRUE. Only applicable for AHN1 or AHN2. It decides if you want the filtered point clouds or not.
#'@param keep.sheets Default TRUE. Only applicable if method.sheets is set to TRUE. Set to FALSE if you want to delete the downloaded sheets (kaartbladen).
#'@param redownload Deafult FALSE. Only applicable if sheets is set to TRUE. Set to TRUE if you want to redownload the sheets (kaartbladen)
#'@author Jelle Stuurman
#'@source <https://www.pdok.nl/datasets>
#'download_poinCloud(name, wd, AHN = "AHN3", bladnrs, area, interpolate = TRUE, keep.sheets = TRUE, redownload = FALSE)
#'@return .tif of DSM AHN area

download_pointCloud <- function(name, wd, AHN = "AHN3", bladnrs, area, bboxes, filtered = TRUE, keep.sheets = TRUE, redownload = FALSE){
  indiv_pc_rasters <- list()
  ahn_atomFeed_BaseUrl <- paste(ngr, "/", tolower(AHN), "/extract/", tolower(AHN), "_", sep="")

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
  ahn_pc_directory <- paste(ahn_directory, "pc", sep="/")
  if(!dir.exists(ahn_pc_directory)){
    dir.create(ahn_pc_directory, showWarnings = FALSE)
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
      pcSheetFileNameLaz <- paste0(ahn_pc_letter, "_", toupper(bladnrs[[r]]), ".LAZ")
      ahn_pc_file_path <- paste0(ahn_pc_directory, "/", pcSheetFileNameLaz)
    } else if(tolower(AHN) == "ahn2"){
      #ahn2u: https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_uitgefilterd/g01cz1.laz.zip
      #ahn2g: https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_gefilterd/g01cz1.laz.zip
      lasZIP = ".laz.zip"
      if(filtered == TRUE){
        ahn_pc_letter <- "g"
        ahn_pc_naming <- "gefilterd/g"
      } else {
        ahn_pc_letter <- "u"
        ahn_pc_naming <- "uitgefilterd/u"
      }
      ahn_pc_downloadLink <- paste(ahn_atomFeed_BaseUrl, ahn_pc_naming,  tolower(bladnrs[[r]]), lasZIP, sep="")
      pcSheetFileNameLaz <- paste0(ahn_pc_letter, tolower(bladnrs[[r]]), ".laz")
      ahn_pc_file_path <- paste0(ahn_pc_directory, "/", pcSheetFileNameLaz)
    } else if(tolower(AHN) == "ahn1"){
      # #ahn1u: https://geodata.nationaalgeoregister.nl/ahn1/extract/ahn1_uitgefilterd/01cz1.laz.zip
      # #ahn1g: https://geodata.nationaalgeoregister.nl/ahn1/extract/ahn1_gefilterd/01cz1.laz.zip
      lasZIP = ".laz.zip"
      if(filtered == TRUE){
        ahn_pc_letter <- "g"
        ahn_pc_naming <- "gefilterd/"
      } else {
        ahn_pc_letter <- "u"
        ahn_pc_naming <- "uitgefilterd/"
      }
      ahn_pc_downloadLink <- paste(ahn_atomFeed_BaseUrl, ahn_pc_naming,  tolower(bladnrs[[r]]), lasZIP, sep="")
      pcSheetFileNameLaz <- paste0(ahn_pc_letter, tolower(bladnrs[[r]]), ".laz")
      ahn_pc_file_path <- paste0(ahn_pc_directory, "/", pcSheetFileNameLaz)
    }

    if(tolower(AHN) == "ahn2" || tolower(AHN) == "ahn1"){
      fileext <- ".zip"
      ahn_pcZip_file_path <- paste(ahn_pc_directory, "/", ahn_pc_letter, bladnrs[[r]], fileext, sep="")
    } else {
      fileext <- ".LAZ"
      ahn_pcZip_file_path <- paste(ahn_pc_directory, "/", ahn_pc_letter, "_", toupper(bladnrs[[r]]), fileext, sep="")
    }

    #check if sheet exists and download, else redowload or use it
    if(!file.exists(ahn_pc_file_path)){
      print("Downloading point cloud sheets...")
      print(ahn_pc_downloadLink)
      utils::download.file(ahn_pc_downloadLink, destfile = ahn_pcZip_file_path, mode="wb")
      if(tolower(AHN) == "ahn2" || tolower(AHN) == "ahn1"){
        utils::unzip(ahn_pcZip_file_path, overwrite = TRUE, exdir = ahn_pc_directory)
        if(tolower(AHN) == "ahn1"){
          file.rename(paste0(ahn_pc_directory, "/", tolower(bladnrs[[r]]), ".laz"), ahn_pc_file_path)
        }
        file.remove(ahn_pcZip_file_path)
      }
    } else {
      if(redownload == TRUE){
        print("Redownloading DSM sheets...")
        file.remove(paste0(ahn_pc_directory, "/", pcSheetFileNameLaz))
        utils::download.file(ahn_pc_downloadLink, destfile = ahn_pcZip_file_path, quiet = TRUE)
        utils::unzip(ahn_pcZip_file_path, overwrite = TRUE, exdir = ahn_pc_directory)
        if(tolower(AHN) == "ahn1"){
          file.rename(paste0(ahn_pc_directory, "/", tolower(bladnrs[[r]]), ".laz"), ahn_pc_file_path)
        }
        file.remove(ahn_pcZip_file_path)
      } else {
        message(paste("Corresponding point cloud sheet", bladnrs[[r]], "already exists and will be used.", sep=" "))
      }
    }
    #View(bboxes)
    laz <- read_pc(laz = ahn_pc_file_path, AHN = AHN, filtered_name = ahn_pc_letter, bladnrs = bladnrs[r], area = area, bbox = bboxes[,r], name = name, nr = r, bladnrsLength = length(bladnrs))
  }
  #ahn_sheet_pc <- raster::stack(ahn_pc_file_path)
  #raster::crs(ahn_sheet_pc) <- epsg_rd
  # print("Cropping pc sheet to (part of) the area.")
  # ahn_pc_crop <- raster::crop(ahn_sheet_pc, area)
  # indiv_pc_rasters[[r]] <- ahn_pc_crop

  #laz <- lidR::writeLAS(file = ahn_pc_file_path, file = laz)
  #unlink(paste0("output/", name, "temp_", ahn_pc_letter, AHN), recursive = TRUE)

#   if(keep.sheets == FALSE){
#     for(fr in 1:length(ahn_pc_file_paths)){
#       file.remove(ahn_pc_file_paths[fr])
#     }
#   }
  return(laz)
}
