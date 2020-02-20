#'Download bladnrs
#'
#'@title Download bladnrs
#'@description Download blad numbers.
#'@param wd Required. Working directory.
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'.
#'@author Jelle Stuurman
#'ahn_area(name, X, Y, radius, bbox, geom, LONLAT = FALSE, AHN = "AHN3", dem = "dsm", resolution, interpolate = TRUE, decimals = 2, sheets = FALSE, delete.sheets = FALSE, redownload = FALSE)
#'@return all bladnrs geometry

download_bladnrs <- function(wd, AHN = "AHN3"){
  bladIndex_gpkgpath <- paste(wd , sep="/")
  bladIndex_gpkg <- paste(bladIndex_gpkgpath, "/", AHN, "_bladIndex", ".gpkg", sep="")
  print(bladIndex_gpkg)
  #if(!file.exists(bladIndex_gpkg)){
    print("Download AHN wfs blad Index")
    ahn_WFS_baseUrl <- paste0(ngr, "/", tolower(AHN), "/wfs?SERVICE=WFS&VERSION=1.0.0&REQUEST=GetFeature&TYPENAME=", tolower(AHN), ":", tolower(AHN), "_bladindex")
    print(ahn_WFS_baseUrl)
    ahn_wfs <- paste("WFS:", ahn_WFS_baseUrl, "&SRSNAME=EPSG:28992&outputFormat=application/json", sep="")
    print(ahn_wfs)

    ahn_bi <- sf::st_read(ahn_wfs)
    #gdalUtils::ogr2ogr(src_datasource_name = ahn_bi , dst_datasource_name = bladIndex_gpkg, layer = paste0(tolower(AHN),":", tolower(AHN), "_bladindex"), overwrite = TRUE)
    sf::st_write(ahn_bi, bladIndex_gpkg)
  #}
  #load intersected blad indexes
  # bladIndex.shp <- rgdal::readOGR(dsn = wd, layer = paste0(AHN, "_bladIndex"), stringsAsFactors=FALSE)
  # bladIndex.shp <- sp::spTransform(bladIndex.shp, epsg_rd)
  # bladIndex.sf <- sf::st_as_sf(bladIndex.shp)
  return(ahn_bi)
}
