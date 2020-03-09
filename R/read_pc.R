#'read point cloud
#'
#'@title read point cloud
#'@description read point cloud
#'@param name Optional. Give a name of the specified area.
#'@param laz Required. Laz data
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'
#'@param bladnrs Required. Blad numbers.
#'@param area Required area to be downloaded
#'@param radius Optional. Set radius in meters of area around a point to create a buffer area (circle).
#'@param bbox Optional. individual bboxes of cropped sheets
#'@param filtered_name Code name of (un)filtered data.
#'@param nr bladnr
#'@param bladnrsLength Total amunt of blad nrs
#'@author Jelle Stuurman
#'@source <https://www.pdok.nl/datasets>
#'read_pc(laz, AHN = "AHN3", filtered_name, area, radius, bladnrs, bbox, name, nr, bladnrsLength)
#'@return .laz data

read_pc <- function(laz, AHN = "AHN3", filtered_name, area, radius, bladnrs, bbox, name, nr, bladnrsLength){
  temp_dir <- paste0("output/", name, "/temp_", filtered_name, AHN)
  if(!dir.exists(temp_dir)){
    dir.create(temp_dir, showWarnings = FALSE)
  }
  temp_name <- paste0(temp_dir, "/", name,"_", filtered_name, AHN, "_", nr, ".laz")
  ahn_pc_name <- paste0("output/", name, "/", name,"_", filtered_name, AHN, ".laz")
  if(!file.exists(temp_name)){
    filterstring <- paste("-keep_xy", floor(bbox["xmin"]), floor(bbox["ymin"]), ceiling(bbox["xmax"]), ceiling(bbox["ymax"]), sep=" ")
    print(filterstring)
    message("reading .LAZ file. This may take a while...")
    my_laz <- lidR::readLAS(files = laz, filter = filterstring)
    lidR::writeLAS(file = temp_name, las = my_laz)
  } else {
    print(".laz file already exists and will be used.")
  }
  laz <- write_pc(temp_name = temp_name, ahn_pc_name = ahn_pc_name, nr = nr, bladnrsLength = bladnrsLength, area = area)
  return(laz)
}
