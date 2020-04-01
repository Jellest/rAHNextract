#'Create BBOX polygon
#'
#'@title create BBOX polygon
#'@description Create BBOX geometry of certain area
#'@param bbox Set bbox of area. c(XMIN, YMIN, XMAX, YMAX)
#'@author Jelle Stuurman
#'@return BBOX polygon

create_bbox_polygon <- function(bbox){
  polygon_list <- list(rbind(c(bbox$xmin, bbox$ymin), c(bbox$xmin, bbox$ymax), c(bbox$xmax, bbox$ymax), c(bbox$xmax, bbox$ymin), c(bbox$xmin, bbox$ymin)))
  polygon_sfc <- sf::st_polygon(polygon_list)
  polygon_geom <- sf::st_sfc(polygon_sfc, crs = 28992)
  my_area.sf <- sf::st_sf(geometry = polygon_geom)
  return (my_area.sf)
}
