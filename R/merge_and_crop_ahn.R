#'@inheritParams ahn_area
#'@noRd
merge_and_crop_ahn <- function(name, sheets, AHN, dem, resolution, area, output.dir) {
  # Read all tif images into a single raster stack
  cropped_sheets <- list()
  for (i in seq_along(sheets$filePath)) {
    raster <- terra::rast(sheets$filePath[i])
    cropped_sheets[[i]] <- terra::crop(raster, area)
  }
  # Merge raster
  merged_raster <- cropped_sheets[[1]]
  if (length(cropped_sheets) > 1) {
    for (i in 2:length(cropped_sheets)) {
      merged_raster <- terra::merge(merged_raster, cropped_sheets[[i]])
    }
  }

  # Set the CRS of the merged raster to EPSG 28992
  terra::crs(merged_raster) <- proj_rd

  #mask the raster

  ahn_masked_raster_filepath <- paste(output.dir, "/", name, "_", AHN, "_", dem, resolution, ".tif", sep = "")
  if (file.exists(ahn_masked_raster_filepath)) {
    message(paste("Masked raster at ", ahn_masked_raster_filepath, " already exists and will be overwritten.", sep = ""))
  }
  # masked raster
  ahn_masked_raster <- terra::mask(x = merged_raster, mask = area, filename = ahn_masked_raster_filepath, overwrite = TRUE)

  if (output.dir != tempdir()) {
    # Write the cropped raster to a new tif file
    print(paste0("Masked raster and saved on disk at: ", ahn_masked_raster_filepath))
  }
  return(list("data" = ahn_masked_raster, "fileName" = ahn_masked_raster_filepath))
}