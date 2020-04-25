#'Download point cloud
#'
#'@title Download point cloud
#'@description Download DSM
#'@param name Optional. Give a name of the specified area.
#'@param output.dir Required. Working directory.
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@param bladnrs Required. Blad numbers.
#'@param area Required area to be downloaded
#'@param radius Radius of circle or squared BBOX in meters.
#'@param bboxes Optional. individual bboxes of cropped sheets
#'@param gefilterd Default TRUE. Only applicable for AHN1 or AHN2. It decides if you want the gefilterd point clouds or not.
#'@param sheets.location Optional. Default is the 'AHN_sheets' directory in working directory. Set directory where all the AHN sheets are stored or will be stored. Always use the correct directory structure and capitalization within the selected directory. Example directory structure: 'AHN_sheets/AHN3/DSM' or 'AHN/sheets/AHN2/DTM'.  Always use the correct directory structure and capitalization within the selected directory when pre-existing sheets will be used.
#'@param sheets.keep Default TRUE. Only applicable if sheets.method is set to TRUE. Set to FALSE if you want to delete the downloaded sheets (structure).
#'@param sheets.redownload Default FALSE. Only applicable if sheets is set to TRUE. Set to TRUE if you want to redownload the sheets (structure)
#'@author Jelle Stuurman
#'@source <https://www.pdok.nl/datasets>
#'@return .laz of AHN area
download_pointCloud <- function(name, output.dir, AHN = "AHN3", bladnrs, area, bboxes, radius, gefilterd = TRUE, sheets.location, sheets.keep = TRUE, sheets.redownload = FALSE){
  if(length(bladnrs) == 0){
    stop("No sheets were found within the area. Please check your input area.")
  }
  dwnld <- FALSE
  indiv_pc_rasters <- list()

  if(tolower(AHN) == "ahn3"){
    ahn_atomFeed_BaseUrl <- download_ahn3_url
  } else {
    ahn_atomFeed_BaseUrl <- paste(ngr, "/", tolower(AHN), "/extract/", tolower(AHN), "_", sep="")
  }

  #set output directory if missing
  if(missing(output.dir)){
    output.dir <- default.output.dir
  }

  #set or get sheets directory
  #set or create sheets directory
  #set sheets drectory to working directory
  sheets_directory <- paste(sheets.location, default.sheets.dir, sep="/")
  #create directory if not found
  if(!dir.exists(sheets_directory)){
    dir.create(sheets_directory, showWarnings = FALSE)
  }

  # #name directory
  # name_directory <- paste(output.dir, name, sep="/")
  # if (!dir.exists(name_directory)){
  #   dir.create(name_directory)
  # }

  #ahn directory
  ahn_directory <- paste(sheets_directory, AHN, sep="/")
  if(!dir.exists(ahn_directory)){
    dir.create(ahn_directory, showWarnings = FALSE)
  }

  #PC directory
  ahn_pc_directory <- paste(ahn_directory, "PC", sep="/")
  if(!dir.exists(ahn_pc_directory)){
    dir.create(ahn_pc_directory, showWarnings = FALSE)
  }

  #print(paste0("Destination directory of pc sheet: ", ahn_pc_directory))

  print(paste0("Destination directory of output point clouds area: ", output.dir))

  print(sprintf("Found %i sheet(s) with name(s):", length(bladnrs)))
  for(b in bladnrs){
    print(b)
  }

  ahn_pc_file_paths <- c()
  ahn_pc_letter <- get_ahn_letter(AHN = AHN, method = "pc", gefilterd = gefilterd)
  for(r in 1:length(bladnrs)){
    if(tolower(AHN) == "ahn3"){
      #ahn3: https://geodata.nationaalgeoregister.nl/ahn3/extract/ahn3_laz/C_44FN2.LAZ
      lasZIP = ".LAZ"
      ahn_pc_naming <- paste0("laz/", ahn_pc_letter,"_")
      ahn_pc_downloadLink <- paste(ahn_atomFeed_BaseUrl, ahn_pc_naming,  toupper(bladnrs[[r]]), lasZIP, sep="")
      pcSheetFileNameLaz <- paste0(ahn_pc_letter, "_", toupper(bladnrs[[r]]), ".LAZ")
      ahn_pc_file_path <- paste0(ahn_pc_directory, "/", pcSheetFileNameLaz)
    } else if(tolower(AHN) == "ahn2"){
      #ahn2u: https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_uitgefilterd/g01cz1.laz.zip
      #ahn2g: https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_gefilterd/g01cz1.laz.zip
      lasZIP = ".laz.zip"
      if(gefilterd == TRUE){
        ahn_pc_naming <- "gefilterd/g"
      } else {
        ahn_pc_naming <- "uitgefilterd/u"
      }
      ahn_pc_downloadLink <- paste(ahn_atomFeed_BaseUrl, ahn_pc_naming,  tolower(bladnrs[[r]]), lasZIP, sep="")
      pcSheetFileNameLaz <- paste0(ahn_pc_letter, tolower(bladnrs[[r]]), ".laz")
      ahn_pc_file_path <- paste0(ahn_pc_directory, "/", pcSheetFileNameLaz)
    } else if(tolower(AHN) == "ahn1"){
      # #ahn1u: https://geodata.nationaalgeoregister.nl/ahn1/extract/ahn1_uitgefilterd/01cz1.laz.zip
      # #ahn1g: https://geodata.nationaalgeoregister.nl/ahn1/extract/ahn1_gefilterd/01cz1.laz.zip
      lasZIP = ".laz.zip"
      if(gefilterd == TRUE){
        ahn_pc_naming <- "gefilterd/"
      } else {
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
      utils::download.file(url = ahn_pc_downloadLink, destfile = ahn_pcZip_file_path, mode="wb")
      if(tolower(AHN) == "ahn2" || tolower(AHN) == "ahn1"){
        utils::unzip(zipfile = ahn_pcZip_file_path, overwrite = TRUE, exdir = ahn_pc_directory)
        if(tolower(AHN) == "ahn1"){
          file.rename(paste0(ahn_pc_directory, "/", tolower(bladnrs[[r]]), ".laz"), ahn_pc_file_path)
        }
        file.remove(ahn_pcZip_file_path)
        dwnld <- TRUE
      }
    } else {
      if(sheets.redownload == TRUE){
        print("Redownloading DSM sheets...")
        file.remove(paste0(ahn_pc_directory, "/", pcSheetFileNameLaz))
        utils::download.file(url = ahn_pc_downloadLink, destfile = ahn_pcZip_file_path, quiet = TRUE)
        utils::unzip(zipfile = ahn_pcZip_file_path, overwrite = TRUE, exdir = ahn_pc_directory)
        if(tolower(AHN) == "ahn1"){
          file.rename(paste0(ahn_pc_directory, "/", tolower(bladnrs[[r]]), ".laz"), ahn_pc_file_path)
        }
        file.remove(ahn_pcZip_file_path)
        dwnld <- TRUE
      } else {
        message(paste("Corresponding point cloud sheet", bladnrs[[r]], "already exists and will be used.", sep=" "))
      }
    }
    laz <- read_pc(output.dir = output.dir, laz = ahn_pc_file_path, AHN = AHN, ahn_letter = tolower(ahn_pc_letter), bladnrs = bladnrs[r], area = area, bbox = bboxes[,r], name = name, nr = r, bladnrsLength = length(bladnrs), radius = radius)
  }
  if(sheets.keep == FALSE && dwnld == TRUE){
    for(spc in 1:length(ahn_pc_file_paths)){
      file.remove(ahn_pc_file_paths[spc])
    }
  }
  if(output.dir!= tempdir()){
    print(ahn_pc_file_path)
  }
  return(list("data" = laz, "fileDir" = output.dir, "fileName" = ahn_pc_file_path))
}
