#'@inheritParams ahn_area
#'@noRd
find_ahn_sheets <- function(name, area, type = "", bladIndex) {
  shape_area <- area$area
  shape_area <- sf::st_transform(shape_area, sf::st_crs(bladIndex))

  sf::st_agr(bladIndex) <- "constant"
  sf::st_agr(shape_area) <- "constant"

  if (type == "point" || type == "area") {
    #download raster sheets for area or point intersection
    bladnrsIntersect.sf <- sf::st_intersection(bladIndex, sf::st_buffer(shape_area, 0))
    bladnrs <- bladnrsIntersect.sf$kaartbladNr
    if (length(bladnrs) == 0) {
      stop("No intersection found between the area and the AHN sheets.")
    }
    if (type == "point") {
      #if (length(bladnrs) == 4) {
      #stop("The selected point is exactly on the intersect of 4 AHN sheets. Pease adjust the X and Y coordinates by at least 1 meter.")
      #} else if (length(bladnrs) == 2) {
      #stop("The selected point is exactly on the intersect of 2 AHN sheets. Pease adjust the X OR Y coordinates by at least 1 meter. If changinig either coordinate doe not work, change both.")
      #}
    } else if (type == "area") {
      geom_types <- sf::st_geometry_type(bladnrsIntersect.sf, by_geometry = TRUE)
      if (length(unique(geom_types)) > 1) {
        polygon_bladnrsIntersect.sf <- sf::st_collection_extract(x = bladnrsIntersect.sf, type = "POLYGON", warn = FALSE)
        bladnrs <- polygon_bladnrsIntersect.sf$bladnr
      }
    }
    output <- bladnrs
  } else if (type == "pc") {
    # #download point clouds sheets
    # bladnrsIntersect.sf <- sf::st_crop(bladIndex, sf::st_buffer(shape_area, 0))
    # bladnrs <- bladnrsIntersect.sf$bladnr
    # bboxes <- c()
    # for (f in bladnrs){
    #   bladnr <- bladnrsIntersect.sf$bladnr == bladnrs[f]
    #   singlebladNr.sf <- bladnrsIntersect.sf[bladnr, ]
    #   sf::st_agr(singlebladNr.sf) <- "constant"
    #   singlebladNr.sf <- sf::st_crop(singlebladNr.sf, sf::st_buffer(shape_area, 0))
    #   my_bbox <- sf::st_bbox(singlebladNr.sf)
    #   bboxes <- cbind(bboxes, my_bbox)
    # }
    # ahn_data <- download_pointCloud(name = name_trim, output.dir = output.dir, AHN = AHN, bladnrs = bladnrs, area = shape_area, radius = radius, bboxes = bboxes, gefilterd = gefilterd, sheets.location = sheets.location, sheets.keep = sheets.keep)
    # output <- ahn_data
  }
  return(output)
}
