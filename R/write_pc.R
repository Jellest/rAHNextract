#'write point cloud
#'
#'@title write point cloud
#'@description read point cloud
#'@param temp_name Required. .laz data
#'@param name name of file
#'@param ahn_pc_name Default 'AHN3'. Set to 'AHN1', 'AHN2', or 'AHN3'
#'@param area Required area to be downloaded
#'@param nr bladnr
#'@param bladnrsLength Total amount of blad nrs
#'@param radius radius. Required for correct naming.
#'@author Jelle Stuurman
#' write_pc(temp_name, ahn_pc_name, nr, bladnrsLength, area)
write_pc <- function(temp_name, name, ahn_pc_name, nr, bladnrsLength, area, radius){
  my_laz <- lidR::readLAS(files = temp_name)
  if(nr == 1){
    print(ahn_pc_name)
    if(file.exists(ahn_pc_name)){
      warning(paste("Point cloud for ", name, " (", radius, "m) already exists and was overwritten." ,sep =""))
    }

    print("writing .laz")
    if(nr == bladnrsLength){
      my_laz <- lidR::lasclip(las = my_laz, geometry = area)
    }
    lidR::writeLAS(file = ahn_pc_name, las  = my_laz)
  } else {
    new_laz_data <- lidR::readLAS(files = ahn_pc_name)
    my_laz <- rbind(my_laz, new_laz_data)
    file.remove(ahn_pc_name)
    lidR::writeLAS(file = ahn_pc_name, las = my_laz)
    if(nr == bladnrsLength){
      my_laz <- lidR::readLAS(files = ahn_pc_name)
      my_laz <- lidR::lasclip(las = my_laz, geometry = area)
    }
    file.remove(ahn_pc_name)
    lidR::writeLAS(file = ahn_pc_name, las = my_laz)
  }
  my_laz <- lidR::readLAS(files = ahn_pc_name)

return(my_laz)}
