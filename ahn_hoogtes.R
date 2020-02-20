##Author: Jelle Stuurman
##Updated: 15-02-2020

ahn_hoogtes <- function(name, X, Y, LONLAT = FALSE, AHN = "AHN3", resolution = 0.5, dem = "dsm", interpolated = TRUE, rm = FALSE, decimals = 2, type = "points"){
  ##functies##

  ##resolutie bepalen
  get_resolution <- function(AHN = "AHN3", resolution){
    if(AHN == "AHN1"){
      if(missing(resolution)){
        warning("No resolution was found for importing AHN1. Resolution of 5 meters was used.")
        resolution = 5
        resolution_name <- "5m"
      } else if(resolution != 5 && resolution != 25 && resolution != 100){
        warning("No correct resolution was found for importing AHN1. Resolution of 5 m was used.")
        resolution <- 5
        resolution_name <- "5m"
      } else {
        resolution <- resolution
        resolution_name <- paste0(resolution,"m")
      }
    } else if(AHN == "AHN2" || AHN == "AHN3"){
      if(missing(resolution)){
        warning(paste0("No resolution was found for importing ", AHN, ". Resolution of 0.5 meters was used."))
        resolution = 0.5
        resolution_name <- "05m"
      } else {
        if(resolution == 0.5){
          resolution_name <- "05m"
        } else if(resolution == 5){
          resolution_name <- "5m"
        } else {
          warning(paste0("No correct resolution was found for importing ", AHN, ". Resolution of 0.5 meters was used."))
          resolution <- 0.5
          resolution_name <- "05m"
        }
      }
    }
    return(list("res" = resolution, "res_name" = resolution_name))
  }

  ##dem bepalen
  get_dem <- function(AHN, dem, resolution, interpolated = TRUE){
    if(AHN == "AHN1"){
      if(tolower(dem) == "dsm"){
        warning(paste("There is no dsm available for this dataset. Dtm (maaiveld)", resolution, "m was used.", sep = " "))
      }
      info <- paste(AHN, resolution, "m resolution dtm (maaiveld) selected.", sep = " ")
      dem <- ""
    } else if(AHN == "AHN2"){
      if(resolution == 5){
        dem <- ""
        info <- "AHN2 5 m resolution dtm (maaiveld) selected."
      } else if(resolution == 0.5){
        if(tolower(dem) == "dtm"){
          if(interpolated == TRUE){
            dem <- "int"
            info <- "AHN2 0.5 m resolution dtm (maaiveld) interpolated (opgevuld) selected."
          } else if(interpolated == FALSE){
            dem <- "non"
            info <- "AHN2 0.5 m resolution dtm (maaiveld) (niet opgevuld) selected."
          } else {
            stop("No correct interpolated parameter is provided. Please set it to 'TRUE' or 'FALSE'.")
          }
        } else if (tolower(dem) == "dsm"){
          dem <- "ruw"
          info <- "AHN2 0.5 m resolution dsm (ruw) selected."
        } else {
          stop("No correct dem is provided. Please select 'dtm' or 'dsm'.")
        }
      }
    } else if(AHN == "AHN3"){
      if(tolower(dem) != "dtm" && tolower(dem) != "dsm"){
        stop("Provided wrong dem, Please select 'dsm' or 'dtm'.")
      } else {
        dem <- tolower(dem)
        if(dem == "dtm"){
          specs <- "(ruw)"
        } else if(dem == "dsm"){
          specs <- "(maaiveld)"
        }
        info <- paste("AHN3", resolution, "resolution", dem, specs, "selected.", sep = " ")
      }
    }
    message(info)
    return(dem)
  }


  ##ahn punt gebied ophalen
  get_ahn_point <- function(name = "", X, Y, LONLAT = FALSE, resolution){
    epsg_rd <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs"

    if(name != ""){
      #name_trim <- getName_trim(name = name, addition = addition)
    }

    #create point
    spatialpoint <- data.frame("X"=X,"Y"=Y)
    coordinates(spatialpoint) <- ~X+Y

    #convert LATLON
    if(LONLAT == TRUE){
      crs(spatialpoint) <- CRS("+init=epsg:4326")
    } else {
      crs(spatialpoint) <- CRS(epsg_rd)
    }
    if(LONLAT == TRUE){
      spatialpoint <- spTransform(spatialpoint, CRS = CRS(epsg_rd))
    }
    coords <- coordinates(spatialpoint)

    if(coords[1,"X"] < 12628.0541 || coords[1,"X"] > 283594.4779){
      stop("X coordinate out of range.")
    }
    if(coords[1,"Y"] < 308179.0423 || coords[1,"Y"] > 611063.1429){
      stop("Y coordinate out of range.")
    }

    #create 9 pixels bbox
    xround <- round(X)
    yround <- round(Y)

    if(X - xround > 0){
      my_xmin <- xround - (1 * resolution)
      my_xmax <- xround + (2 * resolution)
    } else if(X - xround < 0){
      my_xmin <- xround - (2 * resolution)
      my_xmax <- xround + (1 * resolution)
    } else {
      my_xmin <- X - (1 * resolution)
      my_xmax <- X + (2 * resolution)
    }

    if(Y - yround > 0){
      my_ymin <- yround - (1 * resolution)
      my_ymax <- yround + (2 * resolution)
    } else if(Y - yround < 0){
      my_ymin <- yround - (2 * resolution)
      my_ymax <- yround + (1 * resolution)
    } else {
      my_ymin <- Y - (1 * resolution)
      my_ymax <- Y + (2 * resolution)
    }

    bbox <- list("xmin"= my_xmin, "xmax"= my_xmax, "ymin" = my_ymin, "ymax" = my_ymax)
    return(list("name" = name, "point" = spatialpoint, "bbox" = bbox))
  }

  ##WCS url maken
  create_wcs_url <- function(wcs_baseUrl = "https://geodata.nationaalgeoregister.nl/ahn3/wcs?SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage", bbox, point, AHN = "AHN3", resolution = 0.5, dem = "dsm", interpolated = TRUE){
    if(AHN == "AHN1"){
      wcs_baseUrl = "https://geodata.nationaalgeoregister.nl/ahn1/wcs?SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage"
    }
    else if(AHN == "AHN2"){
      wcs_baseUrl = "https://geodata.nationaalgeoregister.nl/ahn2/wcs?SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage"
    }
    #get resolution
    resolution <- get_resolution(AHN, resolution)

    #get dem type
    dem <- get_dem(AHN = AHN, resolution = resolution$res, dem = dem)

    #get BBOX extent of buffer area
    my_bbox <- paste(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax, sep=",")
    bbox_url <- paste0("BBOX=", my_bbox)

    #create image pixel dimensions
    if(point == TRUE){
      my_width <- 3
      my_height <- 3
    } else {
      my_width <- bbox$xmax - bbox$xmin
      my_height <- bbox$ymax - bbox$ymin
    }

    dimensions_url <- paste0("WIDTH=", my_width, "&HEIGHT=", my_height)
    #name of layer
    if(dem == ""){
      underscore <-  ""
    } else {
      underscore <- "_"
    }
    name_layer_url <- paste0("COVERAGE=", tolower(AHN), "_" , resolution$res_name , underscore, dem)

    #wcs image format
    imgFormat_url <- "FORMAT=GEOTIFF_FLOAT32"

    #coordinate system
    crs_url <- "CRS=EPSG:28992&RESPONSE_CRS=EPSG:28992"

    #generate URL
    wcsUrl <- paste(wcs_baseUrl, name_layer_url, bbox_url, crs_url, imgFormat_url, dimensions_url, sep="&")

    print(wcsUrl)

    return (wcsUrl)
  }

  ##download ahn raster
  download_wcs_raster <- function(wcsUrl, name = "elevation"){
    image_name <- paste0(name, ".tif")
    download.file(wcsUrl, image_name, mode = "wb")
    print("Download raster image succeeded.")
    my_raster <- raster(image_name)
    NAvalue(my_raster) <- -32768.0
    plot(my_raster, xlab="RD X", ylab="RD Y", main="Elevation (m)")
    ras <- list("raster" = my_raster, "file" = image_name)
    return(ras)
  }

  ##bereken AHN hoogte
  intersect_raster <- function(ras, point){
    print("Intersecting raster. Getting elevation...")
    my_elevation <- raster::extract(ras, point, method = "bilinear")
    return (my_elevation)
  }
  #Uitvoering van functies

  #selected AHN layer
  ahn_lower <- tolower(AHN)
  if(ahn_lower != "ahn1" && ahn_lower != "ahn2" && ahn_lower != "ahn3"){
    stop("No correct AHN is provided. Please select 'AHN3' or 'AHN2'")
  } else {
    my_ahn <- toupper(AHN)
  }

  #AHN gebied maken en AHN raster ophalen
  my_point <- get_ahn_point(name = name, X = X, Y = Y, LONLAT = LONLAT, resolution = resolution)
  my_url <- create_wcs_url(bbox = my_point$bbox, point = TRUE, AHN = my_ahn, resolution = resolution, dem = dem, interpolated = interpolated)
  my_raster <- download_wcs_raster(my_url, name)

  #hoogte berkenen
  my_elevation <- intersect_raster(my_raster$raster, my_point$point)

  if(rm == TRUE){
    file.remove(my_raster$file)
  }
  my_elevation <- format(round(my_elevation, decimals), nsmall = decimals)
  my_elevation <- as.numeric(my_elevation)
  print(paste("Hoogte:", my_elevation, "m.", sep=" "))
  return (my_elevation)
}


##INSTRUCTIES en UITVOERING##

##instaleer benodigde packages
#Deze stap hoeft maar een keer gedaan te worden in R.
install.packages("raster")
install.packages("sp")


##benodigde libraries (packages) aanzetten. Deze stap moet elke als je R opstart
library(sp)
library(raster)

##Functie laden
#Laad de functie 'ahn_hoogtes'

##bepaal je working directory.
#In deze map worden je rasters gedowload en je uiteindelijk tabel met hoogtes opgeslagen.
setwd("C:/Users/jelle/Documents")

##punten inladen
#zorg ervoor datt lijst de kolommen "ID", "X" en "Y" hebben. "ID is een unieke naam en tevens de naam van het raster dat je download.
punten <- read.table("C:/Users/jelle/Documents/coordinaten_Zwaanshoek.csv", header = TRUE, sep=",")

##Bepaal parameters
#zet my_ahn parameter op "AHN2" of "AHN3" om de AHN te bepalen.
my_ahn <- "AHN3"
#zet my_resolution parameter op 0.5 of 5, 25 of 100 om de resolutie van de AHN te bepalen. Niet alle resoluties zijn beschikbaar voor AHN1, AHN2 en AHN3.
my_resolution <- 0.5
#zet my_dem parameter op "dtm" (maaiveld) of "dsm" (ruw). Voor AHN1 is alleen dtm beschikbaar.
my_dem <- "DTM"
#zet interpolated parameter op TRUE als je opgevulde gegevens wilt ophalen. Dit is alleen van toepassing wanneer AHN2 wordt gebruikt.
my_intp <- TRUE
#Zet my_rm parameter of FALSE als je NIET wilt dat de gedownloade rasters weer verwijderd worden van je computer nadat de analyse is uitgevoerd.
my_rm <- TRUE
#bepaal aantal decimalen
my_decimalen <- 2

##run de functie ahn_hoogtes van hierboven met de bijbehorende parameters
alle_hoogtes <- mapply(ahn_hoogtes, name = punten$ID, X = punten$X, Y = punten$Y, LONLAT = FALSE, AHN =  my_ahn, resolution = my_resolution,  dem = my_dem, interpolated = my_intp, rm = my_rm, decimals = my_decimalen)


#voeg resultaten samen met coordinaten in een tabel en maak kolom namen
alle_hoogtes_tabel_ruw <- data.frame(my_dem, punten, alle_hoogtes)
colnames(alle_hoogtes_tabel_ruw) <- c("DEM", "ID", "X", "Y", "hoogte")
#View(alle_hoogtes_tabel_ruw)

#rond af alles af naar 2 decimalen
alle_hoogtes_tabel <- alle_hoogtes_tabel_ruw
is.num <- sapply(alle_hoogtes_tabel, is.numeric)
alle_hoogtes_tabel[is.num] <- lapply(alle_hoogtes_tabel[is.num], round, my_decimalen)


#sla hoogtes op als .csv bestand. Bestand word opgelsagen in je working directory (die je hierboven hebt ingesteld met setwd())
write.table(alle_hoogtes_tabel, "coordinate.csv", sep=",", col.names = TRUE, row.names = FALSE)

##bereken individuele hoogte indien wenselijk

#bepaal ID
my_ID <- 47

enkele_hoogte <- ahn_hoogtes(name = punten[my_ID, "ID"], X = punten[my_ID, "X"], Y = punten[my_ID, "Y"], LONLAT = FALSE, AHN =  my_ahn, resolution = my_resolution,  dem = my_dem, interpolated = my_intp, rm = my_rm, decimals = my_decimalen)
enkele_hoogte_tabel <- data.frame(my_dem, punten[my_ID,], enkele_hoogte)
colnames(enkele_hoogte_tabel) <- c("DEM", "ID", "X", "Y", "hoogte")
View(enkele_hoogte_tabel)
