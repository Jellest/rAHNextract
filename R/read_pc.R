#'read point cloud
#'
#'@title read point cloud
#'@description read point cloud
#'@param output.dir Required. working directory
#'@param name Optional. Give a name of the specified area.
#'@param laz Required. .laz data
#'@param AHN Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'
#'@param bladnrs Required. Blad numbers.
#'@param area Required area to be downloaded
#'@param radius Required for circle or squared bbox. Radius of selected AHN area.
#'@param bbox Optional. individual bboxes of cropped sheets
#'@param ahn_letter Code name of (uit)gefilterd data.
#'@param nr bladnr
#'@param bladnrsLength Total amount of blad nrs
#'@author Jelle Stuurman
#'@source <https://www.pdok.nl/datasets>
#'@return .laz data
read_pc <- function(output.dir, laz, AHN = "AHN3", ahn_letter, area, radius, bladnrs, bbox, name, nr, bladnrsLength){
  if(radius == ""){
    radiusText <- ""
    overwriteText <- ""

  } else {
    radiusText <- paste0(radius, "m_")
    overwriteText <- paste0(" (", radius,"m)")
  }

  temp_dir <- paste0(output.dir, "/temp_", name,  "_", radiusText, ahn_letter, AHN)
  if(!dir.exists(temp_dir)){
    dir.create(temp_dir, showWarnings = FALSE)
  }

  temp_name <- paste0(temp_dir, "/", name, "_", radiusText, ahn_letter, AHN, "_", nr, ".laz")

  ahn_pc_filename <- paste0(output.dir, "/", name, "_", radiusText, ahn_letter, AHN, ".laz")
  if(!file.exists(temp_name)){
    filterstring <- paste("-keep_xy", floor(bbox["xmin"]), floor(bbox["ymin"]), ceiling(bbox["xmax"]), ceiling(bbox["ymax"]), sep=" ")
    print(paste0("Filter string: ", filterstring))
    message("reading .LAZ file. This may take a while...")
    my_laz <- lidR::readLAS(files = laz, filter = filterstring)
    lidR::writeLAS(file = temp_name, las = my_laz)
  } else {
    print(".laz file already exists and will be used.")
  }
  laz <- write_pc(temp_name = temp_name, name = name, ahn_pc_filename = ahn_pc_filename, nr = nr, bladnrsLength = bladnrsLength, area = area, radius = radius)
  return(laz)
}
