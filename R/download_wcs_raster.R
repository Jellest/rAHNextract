#'Download WCS raster
#'
#'@title Download WCS raster
#'@description Download WCS raster
#'@param name Required. Name
#'@param wcsUrl Required. WCS URL
#'@param AHN Default 'AHN3'. 'AHN3' 'AHN2' or 'AHN1'
#'@param dem Default 'DSM'. Select DEM: 'DSM' or "DTM'
#'@param resolution Default 0.5
#'@param radius Radius of circle or squared BBOX in meters.
#'@param interpolate Only applicable for AHN2.
#'@param destfile destination file.
#'@param type 'area' or 'point'
#'@author Jelle Stuurman
#'@source <https://www.pdok.nl/datasets>
#'download_wcs_raster(name = "elevation", wcsUrl)
#'@return .tif float32 file of BBOX area.
download_wcs_raster <- function(wcsUrl, name = "elevation", AHN = "AHN3", dem = "DSM", resolution, radius, interpolate, destfile = "", type = "area"){
        my_resolution <- get_resolution(AHN = AHN, resolution = resolution)
        ahn_letter <- get_ahn_letter(AHN = AHN, dem = dem, resolution = my_resolution$res, interpolate = interpolate, method = "raster")
        if(type == "point"){
                radiusText <- ""
                overwriteText <- ""
        } else {
                radiusText <- paste0(radius, "m_")
                overwriteText <- paste0("(", radius, "m)")
        }
        if(destfile == ""){
                #creat temp file
                image_name <- paste0(tempdir(), name, "_", radius,"m_", tolower(ahn_letter), AHN, "_", my_resolution$res_name,"_", toupper(dem), ".tif")
                #image_name <- paste0(name, "_", tolower(ahn_letter), AHN, "_", my_resolution$res_name,"_", toupper(dem))
                #image_name <- tempfile(pattern = image_name, tmpdir = tempdir(), fileext = ".tif")
                name_directory <- tempdir()
        } else if(destfile == "structured"){
                if(!dir.exists(structured_output_folder)){
                        dir.create(structured_output_folder, showWarnings = FALSE)
                }
                name_directory <- paste0(structured_output_folder, "/", name)
                if(!dir.exists(name_directory)){
                        dir.create(name_directory, showWarnings = FALSE)
                }

                image_name <- paste0(name_directory, "/", name, "_", radiusText, tolower(ahn_letter), AHN, "_", my_resolution$res_name,"_", toupper(dem), ".tif")
        } else {
                name_directory <- destfile
                image_name <- paste0(destfile, "/", name, "_", radiusText, tolower(ahn_letter), AHN, "_", my_resolution$res_name,"_", toupper(dem), ".tif")
        }
        if(file.exists(image_name)){

                warning(paste("Cropped WCS raster for", name, overwriteText, "already exists and was overwritten." ,sep =" "))
                file.remove(image_name)
        }

        #
        #         #ahn directory
        #         ahn_directory <- paste(name_directory, AHN, sep="/")
        #         if(!dir.exists(ahn_directory)){
        #                 dir.create(paste(name_directory, AHN, sep="/"), showWarnings = FALSE)
        #         }
        #
        #         #working directory
        #         working_directory <- paste(ahn_directory, dem, sep="/")
        #         if(!dir.exists(working_directory)){
        #                 dir.create(paste(ahn_directory, dem, sep="/"), showWarnings = FALSE)
        #

        utils::download.file(url = wcsUrl, destfile = image_name, mode = "wb")
        print("Download raster image succeeded.");
        my_raster <- raster::raster(image_name)
        raster::NAvalue(my_raster) <- -32768.0
        #plot(my_raster, xlab="RD X", ylab="RD Y", main="Elevation (m)")
        return(list("data" = my_raster, fileDir = name_directory, fileName = image_name))
}
