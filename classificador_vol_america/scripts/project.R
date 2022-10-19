
project_vect <- function(vect, crs){
  proj4string(vect) <- crs
  vect
}

project_rast <- function(rast, crs){
  projection(rast) <- crs
  rast
} 

project_EPSG_25831_vect <- function(vect){
  crs <- CRS("+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
  project_vect(vect, crs)
}

project_EPSG_25831_rast <- function(rast){
  crs <- CRS("+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
  project_rast(rast, crs)
}




reproject_vect <- function(vect, crs){
  spTransform(vect, crs)
}

reproject_rast <- function(rast, crs){
  projectRaster(rast, crs)
} 

reproject_EPSG_25831_vect <- function(vect){
  crs <- CRS("+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
  reproject_vect(vect, crs)
}

reproject_EPSG_25831_rast <- function(rast){
  crs <- CRS("+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
  reproject_rast(rast, crs)
}