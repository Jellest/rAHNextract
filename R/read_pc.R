read_pc <- function(laz, area){
  my_laz <- lidR::readLAS(files = laz)
  View(my_laz)

  #my_laz <- lidR::lasclip(laz, area)


}

#read_pc("output/circle_test/AHN1/pc/u32bz1.LAZ", )
