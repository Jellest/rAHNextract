#'@title AHN raster area
#'@description Get AHN raster area of a specified area.
#'
#'AHN Area is retrieved through a circle (x, y and radius), BBOX (x, y and radius, OR bbox coordinates), or own geometry (polygon).
#'
#'AHN data is obtained from the AHN3 (default), AHN2 or AHN 1 from the available resolutions.
#'
#'Available for the Digital Surface Model (DSM) and Digital Terrain Model (DTM).
#'
#'You can download the AHN data using the WCS method (default, \code{sheets.method = FALSE}) returning a GeoTIFF format. The sheets method (\code{sheets.method = TRUE}) returns a regular raster GeoTIFF output file that is extracted from the PDOK sheets. The WCS method is recommended if only a few AHN elevation points need to be extracted. The sheets method always requires more data to be downloaded to the client but may be more efficient if many elevatiomns need to be retrieved from a small area. Choosing your method depends on speed and your desired output format. See documentation for all available parameters.
#'@param name Optional. Give a name of the specified area. This name will be used in the output file names.
#'@param X Required. X coordinate in RD New or WGS84 (LON).
#'@param Y Required. Y coordinate in RD New or WGS84 (LAT).
#'@param output.dir Optional but unnecessary. Set location of output raster files. Leaving blank (default) will make all output point files be temporary files. This output directory excludes the location of the AHN sheets which is depicted with the \code{sheets.dir} parameter.
#'@param radius Required for circle or BBOX when no BBOX coordinates are provided. Set radius in meters of area around a point to create a buffer area (circle).
#'@param bbox Required when using a BBOX. Create BBOX of an area. Use c(XMIN, YMIN, XMAX, YMAX) OR set to TRUE when a radius is provided. Use RD New or WGS84 (LONLAT) coordinates.
#'@param polygon Required when create an area of custom geometry. Use spatial object as your area. Refer to [sf](https://r-spatial.github.io/sf/articles/sf2.html) which formats are supported for polygons.
#'@param AHN Default 'AHN'. 'AHN' This is the latest full version made available by [PDOK](https://www.pdok.nl/introductie/-/article/actueel-hoogtebestand-nederland-ahn) through their OGC Web services. Choose between 'AHN4', 'AHN3', 'AHN2', or 'AHN1'.
#'@param dem Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.
#'@param resolution Default 0.5 meters for AHN2/AHN3/AHN4, 5 meters for AHN1. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5 and 100 m.
#'@param sheets Only required when only full AHN sheets need to be downloaded. To do so, fill the list() object with strings of the sheet numbers.
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will be in RD New format.
#'@param sheets.method Default FALSE. When TRUE, it downloads AHN area through the available GeoTIFF AHN sheets available on [PDOK](https://www.pdok.nl/atom-downloadservices/-/article/actueel-hoogtebestand-nederland-ahn).
#'@param sheets.dir Default is the 'AHN_sheets' directory in the working directory or \code{output.dir} (when defined). In this sheets directory all the AHN sheets are downloaded. If sheets already exists in this directory, they will be reused and no redownload will take place. Within this sheets directory, a directory structure will be created automatically: if `sheets.dir` is set to 'myFolder', all sheets will be downloaded in their apropriate AHN version and DEM folder. Eg. '\code{output.dir}/myFolder/AHN_sheets/AHN4/DSM'. You want to use this parameter if you want to have a central location where all the AHN sheets are stored so that they can be accessed elsewhere for other purposes. It is recommended to always use the their original file name after download. Due to the size of these GeoTiff sheets, it is not allowed to download this is the RAM memory (tempdir).
#'@param sheets.keep Default TRUE. Only applicable if \code{sheets.method} is set to TRUE and sheets were downloaded. Set to FALSE if you want to delete the downloaded sheet. It is recommended to keep the sheets if ahn elevation extarction will be followed.
#'@author Jelle Stuurman
#'@return GeoTIFF file of AHN area
#'@export
ahn_area <- function(name = "AHNarea", output.dir, X, Y, radius, bbox, polygon, AHN = "AHN", dem, resolution, sheets = list(), LONLAT = FALSE, sheets.method = FALSE, sheets.dir, sheets.keep = TRUE) {
  interpolate <- TRUE
  loadNamespace("terra")

  # check for selected AHN
  AHN <- check_ahn_version(AHN)

  if (missing(dem) == TRUE) {
    dem <- ""
  }
  name_trim <- gsub(" ", "_", name)

  #set tmp folder if applicable or create output directory
  if (missing(output.dir) == TRUE) {
    output.dir <- tempdir()
  } else {
    if (dir.exists(output.dir) == FALSE) {
      dir.create(output.dir)
      print(paste(output.dir, "directory was not found and was created.", sep = " "))
    }
    #print(output.dir)
  }

  #complete geometry parameters
  if (missing(bbox)) {
    bbox <- FALSE
  }
  if (missing(X) == TRUE && missing(Y) == TRUE && (missing(polygon) == TRUE || bbox == FALSE) && missing(radius) == TRUE) {
    #creating BBOX or shape
    radius <- ""
  }

  #get resolution
  if (missing(resolution) == TRUE) {
    resolution <- ""
  }
  my_resolution <- get_resolution(AHN = AHN, resolution = resolution)

  #get dem type
  dem <- get_dem(AHN = AHN, resolution = my_resolution, dem = dem, interpolate = interpolate)

  if (length(sheets) == 0) {
    #get and create area
    ahn_area <- create_area(X = X, Y = Y, radius = radius, bbox = bbox, polygon = polygon, LONLAT = LONLAT, type = "raster")
  } else {
    if (!is.list(sheets)) {
      stop("No list is provided as the input for the 'sheets' parameter. Please use a list() with the needed sheet nrs.")
    } else {
      #will download full AHN sheets
      sheets.method <- TRUE
    }
  }

  #get AHN data
  bladIndex.sf <- get_bladindex(AHN = AHN, dem = dem, resolution = my_resolution$res)
  if (sheets.method == FALSE) {
    #use WCS method and get data (fast)
    wcs_source <- bladIndex.sf$wcs_url[1]
    wcs_url <- create_wcs_url(bbox = ahn_area$bbox, type = "area", AHN = AHN, resolution = my_resolution, dem = dem, interpolate = interpolate, wcs = wcs_source)
    raster_data <- download_wcs_raster(wcsUrl = wcs_url, name = name_trim, AHN = AHN, dem = tolower(dem), radius = radius, resolution = my_resolution, interpolate = interpolate, output.dir = output.dir, type = "raster")
    ahn_data <- terra::mask(x = raster_data$data, mask = ahn_area$area, filename = raster_data$fileName, overwrite = TRUE)
    output <- ahn_data
    if (output.dir == tempdir()) {
      base::unlink(ahn_data)
    }
  } else if (sheets.method == TRUE) {
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
    print(sprintf("The AHN sheets are loaded from or downloaded into: %s. If no AHN sheet(s) are found in this directory or if no correct name of AHN sheet(s) are found, the AHN sheet(s) will be downloaded.", ahn_sheets_directory))

    #download AHN sheets and get data (slow)
    sheets_df <- data.frame(kaartBlad = character(), filePath = character(), dwnld = logical(), stringsAsFactors = FALSE)
    if (length(sheets) == 0) {
      #get ahn area from ahn sheets
      bladnrs <- find_ahn_sheets(name = name_trim, area = ahn_area, type = "area", bladIndex = bladIndex.sf)
      #print(bladnrs)
      for (b in bladnrs) {
        url <- bladIndex.sf$atom_url[bladIndex.sf$kaartbladNr == b]
        print(url)
        path_sheet <- download_ahn_sheets(name = name_trim, AHN = AHN, dem = dem, url = url, output.dir = output.dir, sheets.dir = ahn_sheets_directory, sheets.keep = sheets.keep)
        sheets_df <- rbind(sheets_df, path_sheet)
      }
      ahn_data <- merge_and_crop_ahn(name = name_trim, sheets = sheets_df, area = ahn_area, AHN = AHN, dem = tolower(dem), resolution = my_resolution$res_name, output.dir = output.dir)

      if (LONLAT == TRUE) {
        warning("The input geometry was provided using Longitude and Latitude coordinates. The output is exported as a raster using the the RD New (epsg 28992) coordinate system.")
      }
      output <- ahn_data$data
      if (output.dir == tempdir()) {
        base::unlink(ahn_data$fileName)
      }
    } else {
      #get complete ahn sheet only
      if (length(sheets) == 0) {
        stop("No sheet numbers are provided or found.")
      }
      for (s in sheets){
        url <- bladIndex.sf$atom_url[bladIndex.sf$kaartbladNr == toupper(s)]
        path_sheet <- download_ahn_sheets(name = name_trim, AHN = AHN, dem = dem, url = url, output.dir = output.dir, sheets.dir = ahn_sheets_directory, sheets.keep = sheets.keep)
        sheets_df <- rbind(sheets_df, path_sheet)
      }
      if (LONLAT == TRUE) {
        warning("The input geometry was provided using Longitude and Latitude coordinates. The downloaded sheet(s) are geotif files with the RD New cordinate system.")
      }
      bladen <- data.frame(filePath = character(), stringsAsFactors = FALSE)
      for (d in seq_along(sheets_df$kaartBlad)) {
        bladen[d, "filePath"] <- sheets_df[d, "filePath"]
        if (sheets_df[d, "downloadedNow"] == TRUE) {
          print(paste("The AHN sheet", sheets_df[d, "kaartBlad"], "in", sheets_df[d, "filePath"], "is downloaded succesfully.", sep = " "))
        }
      }
      output <- bladen$filePath
    }
  } else {
    stop("No correct value is provided for the sheets.method parameter. Please set it to 'TRUE' or 'FALSE'.")
  }
  if (sheets.keep == FALSE) {
    delete_ahn_sheet(sheets_df)
  }
  return(output)
}
