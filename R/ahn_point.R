#'@title AHN elevation point
#'@description Get elevation of specific point location.
#'
#'Requires the X and Y coordinates as input. AHN data is obtained from the AHN in the available resolutions. Default resolution is always the highest resolution (smallest number).
#'
#'Available for the Digital Surface Model (DSM) and Digital Terrain Model (DTM).
#'
#'You can download the AHN data using the WCS method (default, sheets.method = FALSE) returning a GeoTIFF format. The sheets method (sheets.method = TRUE) returns a regular raster GeoTIFF output file that is extracted from the PDOK sheets. The WCS method is recommended if only a few AHN elevation points need to be extracted. The sheets method always requires more data to be downloaded to the client but may be more efficient if many elevatiomns need to be retrieved from a small area. Choosing your method depends on speed and your desired output format. See documentation for all available parameters.
#'@inheritParams ahn_area
#'@param decimals Default 2. Maximum Determines the number of decimal places in the output elevation.
#'@param extract.method Default 'simple'. Choose between 'simple' (nearest) and 'bilinear'. Intersection is done using [\code{extract()}](https://www.rdocumentation.org/packages/terra/versions/1.7-71/topics/extract) function from the \code{terra} package. See [this](https://gisgeography.com/raster-resampling/) article that explains the difference between the two methods.
#'@return AHN elevation in meters.
#'@export
ahn_point <- function(name = "AHNelevation", X, Y, AHN = "AHN", dem, resolution, output.dir, LONLAT = FALSE, extract.method = "simple", decimals = 2, sheets.method = FALSE, sheets.dir, sheets.keep = TRUE) {
  interpolate <- TRUE
  loadNamespace("terra")
  # check for selected AHN
  AHN <- check_ahn_version(AHN)

  if (missing(dem) == TRUE) {
    dem <- ""
  }
  name_trim <- gsub(" ", "_", name)

  #set tmp folder if applicable or create output and directory
  if (missing(output.dir) == TRUE) {
    output.dir <- tempdir()
  } else {
    if (dir.exists(output.dir) == FALSE) {
      dir.create(output.dir)
      print(paste(output.dir, "directory was not found and was created.", sep = " "))
    }
  }

  #get resolution
  if (missing(resolution) == TRUE) {
    resolution <- ""
  }
  my_resolution <- get_resolution(AHN = AHN, resolution = resolution)

  #get dem type
  dem <- get_dem(AHN = AHN, resolution = my_resolution, dem = dem, interpolate = interpolate)

  #get and create a point
  my_point <- generate_ahn_point(name = name_trim, X = X, Y = Y, LONLAT = LONLAT, resolution = my_resolution$res)

  #get AHN data
  bladIndex.sf <- get_bladindex(AHN = AHN, dem = dem, resolution = my_resolution$res)
  if (sheets.method == FALSE) {
    ##get elevation through WCS method (fast)
    wcs_source <- bladIndex.sf$wcs_url[1]
    print(bladIndex.sf$wcs_url[1])
    wcs_url <- create_wcs_url(type = "point", bbox = my_point$bbox, AHN = AHN, dem = dem, resolution = my_resolution, interpolate = interpolate, wcs = wcs_source)
    raster_data <- download_wcs_raster(wcsUrl = wcs_url, name = name_trim, area = ahn_area, AHN = AHN, dem = tolower(dem), resolution = my_resolution, output.dir = output.dir, interpolate = interpolate, type = "point")
    my_elevation <- extract_elevation(raster_data$data, my_point$point, extract.method = extract.method)
  } else if (sheets.method == TRUE) {
    #download AHN sheets and get data (slow)

    #set AHN sheets location
    if (missing(sheets.dir) == TRUE) {
      #sheets directory
      if (output.dir == tempdir()) {
        sheets.dir <- getwd()
      } else {
        sheets.dir <- output.dir
      }
    }
    if (sheets.dir == tempdir()) {
      stop("Due to the size of these AHN sheets, this script does not allow to put these in the RAM memory. Please adjust the location of the sheets.dir.")
    }
    if (grepl(default.sheets.dir, sheets.dir, fixed = TRUE) == TRUE) {
      ahn_sheets_directory <- sheets.dir
    } else {
      if (dir.exists(sheets.dir) == FALSE) {
        dir.create(sheets.dir, showWarnings = FALSE)
      }
      ahn_sheets_directory <- paste(sheets.dir, default.sheets.dir, sep = "/")
    }
    if (dir.exists(ahn_sheets_directory) == FALSE) {
      dir.create(ahn_sheets_directory, showWarnings = FALSE)
    }
    print(sprintf("The AHN sheets are loaded from or downloaded in: %s. If no AHN sheet in the correct directory or if no correct name of AHN sheet is found, sheet will be downloaded. For first use it is recommended to use the default output directory.", ahn_sheets_directory))

    #create area
    ahn_area <- create_area(radius = "", bbox = my_point$bbox, LONLAT = LONLAT, type = "point")

    #download AHN sheets and get data (slow)
    bladnrs <- find_ahn_sheets(name, area = ahn_area, type = "point", bladIndex = bladIndex.sf)
    #get AHN area
    sheets_df <- data.frame(kaartBlad = character(), filePath = character(), dwnld = logical(), stringsAsFactors = FALSE)
    for (b in bladnrs) {
      url <- bladIndex.sf$atom_url[bladIndex.sf$kaartbladNr == b]
      path_sheet <- download_ahn_sheets(name = name_trim, AHN = AHN, dem = dem, url = url, output.dir = output.dir, sheets.dir = ahn_sheets_directory, sheets.keep = sheets.keep)
      sheets_df <- rbind(sheets_df, path_sheet)
    }
    raster_data <- merge_and_crop_ahn(name = name_trim, sheets = sheets_df, area = ahn_area, AHN = AHN, dem = tolower(dem), resolution = my_resolution$res_name, output.dir = output.dir)

    #get elevation
    my_elevation <- extract_elevation(raster_data$data, my_point$point, extract.method = extract.method)
  } else {
    stop("Ncorrect value is provided for the sheets.method parameter. Please set it to 'TRUE' or 'FALSE'.")
  }

  if (is.na(my_elevation)) {
    print(paste("No elevation is available for this point in the ", AHN, " ", dem, ".", sep = ""))
  } else {
    my_elevation <- format(round(my_elevation, decimals), nsmall = decimals)
    print(paste("Elevation of ", name, ": ", my_elevation, " m.", sep = ""))
    my_elevation <- as.numeric(my_elevation)
  }
  if (output.dir == tempdir()) {
    base::unlink(raster_data$fileName)
  }
  if (sheets.keep == FALSE) {
    delete_ahn_sheet(sheets_df)
  }
  return(my_elevation)
}
