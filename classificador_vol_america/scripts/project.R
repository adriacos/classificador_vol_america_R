

project_vect <- function(vect, crs){
  spTransform(vect, crs)
}

project_rast <- function(rast, crs){
  projectRaster(rast, crs)
} 

project_EPSG_25831_vect <- function(vect){
  crs <- CRS("EPSG:25831","+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
  project_vect(vect, crs)
}

project_EPSG_25831_rast <- function(rast){
  crs <- CRS("EPSG:25831","+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
  project_rast(rast, crs)
}
