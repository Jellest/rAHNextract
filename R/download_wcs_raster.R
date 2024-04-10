#'@inheritParams ahn_area
#'@noRd
download_wcs_raster <- function(wcsUrl, name = "elevation", area, AHN = "AHN", dem = "DSM", resolution, radius, interpolate, output.dir, type = "raster") {
  ahn_letter <- get_ahn_letter(AHN = AHN, dem = dem, resolution = resolution$res, interpolate = interpolate, method = "raster")

  #define radius
  if (type == "point" || radius == "") {
    radiusText <- ""
    overwriteText <- ""
  } else {
    radiusText <- paste0(radius, "m_")
    overwriteText <- paste0("(", radius, "m)")
  }

  #set or get outut directory
  if (missing(output.dir) == TRUE || output.dir == tempdir()) {
    output.dir <- tempdir()
  } else if (output.dir == default.output.dir) {
    if (!dir.exists(default.output.dir)) {
      dir.create(default.output.dir, showWarnings = FALSE)
    }
  }

  if (output.dir != tempdir()) {
    print(paste0("Destination directory of output AHN area: ", output.dir))
  }

  #set image name
  image_name <- paste0(output.dir, "/", name, "_", radiusText, tolower(ahn_letter), AHN, "_", resolution$res_name, "_", toupper(dem), ".tif")
  image_name_mask <- paste0(output.dir, "/", name, "_", radiusText, tolower(ahn_letter), AHN, "_", resolution$res_name, "_", toupper(dem), "_mask.tif")

  if (file.exists(image_name)) {
    message(paste("Cropped WCS raster for", name, overwriteText, "already exists and will be overwritten.", sep = " "))
    file.remove(image_name)
  }

  #download WCS image
  utils::download.file(url = wcsUrl, destfile = image_name, mode = "wb", quiet = FALSE)
  print("Download raster image succeeded.")
  ahn_raster <- terra::rast(image_name)

  #print(ahn_raster$extent)
  #ahn_raster <- terra::project(image_name, crs = CRS("+init:epsg:28992"), overwrite = TRUE)
  terra::NAflag(ahn_raster) <- -32768.0
  return(list("data" = ahn_raster, "fileDir" = output.dir, "fileName" = image_name_mask))
}