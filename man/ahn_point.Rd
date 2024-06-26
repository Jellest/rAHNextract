% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ahn_point.R
\name{ahn_point}
\alias{ahn_point}
\title{AHN elevation point}
\usage{
ahn_point(
  name = "AHNelevation",
  X,
  Y,
  AHN = "AHN",
  dem,
  resolution,
  output.dir,
  LONLAT = FALSE,
  extract.method = "simple",
  decimals = 2,
  sheets.method = FALSE,
  sheets.dir,
  sheets.keep = TRUE
)
}
\arguments{
\item{name}{Optional. Give a name of the specified area. This name will be used in the output file names.}

\item{X}{Required. X coordinate in RD New or WGS84 (LON).}

\item{Y}{Required. Y coordinate in RD New or WGS84 (LAT).}

\item{AHN}{Default 'AHN'. 'AHN' This is the latest full version made available by \href{https://www.pdok.nl/introductie/-/article/actueel-hoogtebestand-nederland-ahn}{PDOK} through their OGC Web services. Choose between 'AHN4', 'AHN3', 'AHN2', or 'AHN1'.}

\item{dem}{Default 'DSM'. Choose type of Digital Elevation Model. 'DSM' or 'DTM'. AHN1 only has 'DTM'.}

\item{resolution}{Default 0.5 meters for AHN2/AHN3/AHN4, 5 meters for AHN1. Choose resolution of AHN in meters. AHN3 and AHN2 both have 0.5 and 5 meters. AHN1 has 5 and 100 m.}

\item{output.dir}{Optional but unnecessary. Set location of output raster files. Leaving blank (default) will make all output point files be temporary files. This output directory excludes the location of the AHN sheets which is depicted with the \code{sheets.dir} parameter.}

\item{LONLAT}{Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will be in RD New format.}

\item{extract.method}{Default 'simple'. Choose between 'simple' (nearest) and 'bilinear'. Intersection is done using \href{https://www.rdocumentation.org/packages/terra/versions/1.7-71/topics/extract}{\code{extract()}} function from the \code{terra} package. See \href{https://gisgeography.com/raster-resampling/}{this} article that explains the difference between the two methods.}

\item{decimals}{Default 2. Maximum Determines the number of decimal places in the output elevation.}

\item{sheets.method}{Default FALSE. When TRUE, it downloads AHN area through the available GeoTIFF AHN sheets available on \href{https://www.pdok.nl/atom-downloadservices/-/article/actueel-hoogtebestand-nederland-ahn}{PDOK}.}

\item{sheets.dir}{Default is the 'AHN_sheets' directory in the working directory or \code{output.dir} (when defined). In this sheets directory all the AHN sheets are downloaded. If sheets already exists in this directory, they will be reused and no redownload will take place. Within this sheets directory, a directory structure will be created automatically: if \code{sheets.dir} is set to 'myFolder', all sheets will be downloaded in their appropriate AHN version and DEM folder. Eg. '\code{output.dir}/myFolder/AHN_sheets/AHN4/DSM'. You want to use this parameter if you want to have a central location where all the AHN sheets are stored so that they can be accessed elsewhere for other purposes. It is recommended to always use the their original file name after download. Due to the size of these GeoTiff sheets, it is not allowed to download this is the RAM memory (tempdir).}

\item{sheets.keep}{Default TRUE. Only applicable if \code{sheets.method} is set to TRUE and sheets were downloaded. Set to FALSE if you want to delete the downloaded sheet. It is recommended to keep the sheets if ahn elevation extraction will be followed.}
}
\value{
AHN elevation in meters.
}
\description{
Get elevation of specific point location.

Requires the X and Y coordinates as input. AHN data is obtained from the AHN in the available resolutions. Default resolution is always the highest resolution (smallest number).

Available for the Digital Surface Model (DSM) and Digital Terrain Model (DTM).

You can download the AHN data using the WCS method (default, sheets.method = FALSE) returning a GeoTIFF format. The sheets method (sheets.method = TRUE) returns a regular raster GeoTIFF output file that is extracted from the PDOK sheets. The WCS method is recommended if only a few AHN elevation points need to be extracted. The sheets method always requires more data to be downloaded to the client but may be more efficient if many elevations need to be retrieved from a small area. Choosing your method depends on speed and your desired output format. See documentation for all available parameters.
}
