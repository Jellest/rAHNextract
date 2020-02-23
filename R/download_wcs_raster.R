#'Download WCS raster
#'
#'@title Download WCS raster
#'@description Download WCS raster
#'@param name Required. Name
#'@param wcsUrl Required. WCS URL
#'@author Jelle Stuurman
#'@source <https://www.pdok.nl/datasets>
#'download_wcs_raster(name = "elevation", wcsUrl)
#'@return .tif float32 file of BBOX area.
#'
download_wcs_raster <- function(wcsUrl, name = "elevation"){
        if(!dir.exists("output")){
                dir.create(paste("output"), showWarnings = FALSE)
        }
        image_name <- paste0("output/", name, ".tif")
        utils::download.file(wcsUrl, image_name, mode="wb")
        print("Download raster image succeeded.")
        my_raster <- raster::raster(image_name)
        raster::NAvalue(my_raster) <- -32768.0
        #plot(my_raster, xlab="RD X", ylab="RD Y", main="Elevation (m)")
        ras <- list("raster" = my_raster, "file" = image_name)
        return(ras)
}
