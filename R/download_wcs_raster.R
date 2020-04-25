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
#'@param output.dir destination file.
#'@param type 'area' or 'point'
#'@author Jelle Stuurman
#'@source <https://www.pdok.nl/datasets>
#'@return GeoTIFF float32 file of BBOX area.
download_wcs_raster <- function(wcsUrl, name = "elevation", AHN = "AHN3", dem = "DSM", resolution, radius, interpolate, output.dir, type = "raster"){
  ahn_letter <- get_ahn_letter(AHN = AHN, dem = dem, resolution = resolution$res, interpolate = interpolate, method = "raster")

  #define radius
  if(type == "point" || radius == ""){
    radiusText <- ""
    overwriteText <- ""
  } else {
    radiusText <- paste0(radius, "m_")
    overwriteText <- paste0("(", radius, "m)")
  }

  #set or get outut directory
  if(missing(output.dir) == TRUE || output.dir == tempdir()){
    output.dir <- tempdir()
  } else if(output.dir == default.output.dir){
    if(!dir.exists(default.output.dir)){
      dir.create(default.output.dir, showWarnings = FALSE)
    }
  }

  if(output.dir != tempdir()){
    print(paste0("Destination directory of output AHN area: ", output.dir))
  }

  #set image name
  image_name <- paste0(output.dir, "/", name, "_", radiusText, tolower(ahn_letter), AHN, "_", resolution$res_name,"_", toupper(dem), ".tif")
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

  #downloads WCS image
  utils::download.file(url = wcsUrl, destfile = image_name, mode = "wb")
  print("Download raster image succeeded.");
  ahn_raster <- raster::raster(image_name)
  #ahn_raster <- raster::projectRaster(image_name, crs = crs(epsg_rd), overwrite = TRUE)
  raster::NAvalue(ahn_raster) <- -32768.0
  if(output.dir!= tempdir()){
    print(image_name)
  }
  return(list("data" = ahn_raster, "fileDir" = output.dir, "fileName" = image_name))
}
