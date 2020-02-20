#'Create BBOX polygon
#'
#'@title create BBOX polygon
#'@description Get AHN of a certain area
#'@param bbox Optional. Set bbox of area. c(XMIN, YMIN, XMAX, YMAX)
#'@author Jelle Stuurman
#'create_bbox_polygon(bbox)
#'@return BBOX polygon

create_bbox_polygon <- function(bbox){
  polygon_list <- list(rbind(c(bbox$xmin, bbox$ymin), c(bbox$xmin, bbox$ymax), c(bbox$xmax, bbox$ymax), c(bbox$xmax, bbox$ymin), c(bbox$xmin, bbox$ymin)))
  polygon_sfc <- sf::st_polygon(polygon_list)
  polygon_geom <- sf::st_sfc(polygon_sfc, crs = 28992)
  my_area.sf <- sf::st_sf(geometry = polygon_geom)
  return (my_area.sf)
}
