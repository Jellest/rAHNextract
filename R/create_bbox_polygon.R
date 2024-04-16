#'@inheritParams ahn_area
#'@noRd
create_bbox_polygon <- function(name, bbox, resolution) {
  #create BBOX on rectified grid by correcting the BBOX coordinates
  new_bbox <- list(
    "xmin" = get_rectified_coordinates(x = bbox$xmin, resolution = resolution, rounding = "down"),
    "ymin" = get_rectified_coordinates(x = bbox$ymin, resolution = resolution, rounding = "down"),
    "xmax" = get_rectified_coordinates(x = bbox$xmax, resolution = resolution, rounding = "up"),
    "ymax" = get_rectified_coordinates(x = bbox$ymax, resolution = resolution, rounding = "up")
  )

  if (new_bbox$xmin != bbox$xmin || new_bbox$xmax != bbox$xmax || new_bbox$ymin != bbox$ymin || new_bbox$ymax != bbox$ymax) {
    warning(paste0("Found bbox coordinates for '", name, "' did not align to the rectified grid of the AHN. The BBOX coordinates are therefore adjusted to align the rectified grid of the AHN to avoid resampling."))
  }
  polygon_list <- list(rbind(c(new_bbox$xmin, new_bbox$ymin), c(new_bbox$xmin, new_bbox$ymax), c(new_bbox$xmax, new_bbox$ymax), c(new_bbox$xmax, new_bbox$ymin), c(new_bbox$xmin, new_bbox$ymin)))
  polygon_sfc <- sf::st_polygon(polygon_list)
  polygon_geom <- sf::st_sfc(polygon_sfc, crs = 28992)
  my_area.sf <- sf::st_sf(geometry = polygon_geom)
  return(list("rectified_area" = my_area.sf, "rectified_bbox" = new_bbox))
}
