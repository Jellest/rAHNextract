#'Download WCS raster
#'
#'@title Download WCS raster
#'@description Download WCS raster
#'@param name Required. Name
#'@param wcsUrl Required. WCS URL
#'@param AHN Default 'AHN3'. 'AHN3' 'AHN2' or 'AHN1'
#'@param dem Defailt 'dsm'. Selct DEM: 'dsm' or "dtm'
#'@author Jelle Stuurman
#'@source <https://www.pdok.nl/datasets>
#'download_wcs_raster(name = "elevation", wcsUrl)
#'@return .tif float32 file of BBOX area.
#'
download_wcs_raster <- function(wcsUrl, name = "elevation", AHN = "AHN3", dem = "dsm"){
        if(!dir.exists("output")){
                dir.create(paste("output"), showWarnings = FALSE)
        }

        name_directory <- paste0("output/", name)
        if(!dir.exists(name_directory)){
                dir.create(name_directory, showWarnings = FALSE)
        }


        #ahn directory
        ahn_directory <- paste(name_directory, AHN, sep="/")
        if(!dir.exists(ahn_directory)){
                dir.create(paste(name_directory, AHN, sep="/"), showWarnings = FALSE)
        }

        #working directory
        working_directory <- paste(ahn_directory, dem, sep="/")
        if(!dir.exists(working_directory)){
                dir.create(paste(ahn_directory, dem, sep="/"), showWarnings = FALSE)
        }

        image_name <- paste0(working_directory, "/", name, ".tif")

        utils::download.file(wcsUrl, image_name, mode="wb")
        print("Download raster image succeeded.");
        my_raster <- raster::raster(image_name)
        raster::NAvalue(my_raster) <- -32768.0
        #plot(my_raster, xlab="RD X", ylab="RD Y", main="Elevation (m)")
        ras <- list("raster" = my_raster, "file" = image_name)
        return(ras)
}
