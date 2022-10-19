
library(rgeos)

project_vect <- function(vect, crs){
  proj4string(vect) <- crs
  vect
}

project_rast <- function(rast, crs){
  projection(rast) <- crs
  rast
} 

project_EPSG_25831_vect <- function(vect){
  crs <- CRS(SRS_string = "EPSG:25831")
  #crs <- CRS("+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
  project_vect(vect, crs)
}

project_EPSG_25831_rast <- function(rast){
  crs <- CRS(SRS_string = "EPSG:25831")
  #crs <- CRS("+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
  project_rast(rast, crs)
}




reproject_vect <- function(vect, crs){
  spTransform(vect, crs)
}

reproject_rast <- function(rast, crs){
  projectRaster(rast, crs)
} 

reproject_EPSG_4326_vect <- function(vect){
   crs <- CRS(SRS_string = "EPSG:4326")
   reproject_vect(vect, crs)
}

reproject_EPSG_25831_vect <- function(vect){
  crs <- CRS(SRS_string = "EPSG:25831")
  #crs <- CRS("+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
  reproject_vect(vect, crs)
}

reproject_EPSG_25831_rast <- function(rast){
  crs <- CRS(SRS_string = "EPSG:25831")
  #crs <- CRS("+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
  reproject_rast(rast, crs)
}

get_EPSG_25831_vectors_centroids_lat_lng <- function(vects){
  vects$id___ <- as.numeric(row.names(vects))
  if(vects[1,]$id___==0){
    vects$id___ <- vects$id___ +1
  }
  do.call(rbind, lapply(vects$id___, get_EPSG_25831_vectors_centroids_lat_lng_, vects))
}
get_EPSG_25831_vectors_centroids_lat_lng_ <- function(id, vects){
  get_EPSG_25831_vector_centroids_lat_lng(vects[vects$id___==id,])
}


get_EPSG_25831_vector_centroids_lat_lng <- function(vect){
  v <- reproject_EPSG_4326_vect(vect)
  centr <- gCentroid(v)
  coordinates(centr)
}