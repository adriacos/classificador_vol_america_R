
library(rgeos)

project_vect <- function(vect, crs){
  proj4string(vect) <- crs
  vect
}

project_rast <- function(rast, crs){
  projection(rast) <- crs
  rast
} 

project_EPSG_4258_vect <- function(vect){
  crs <- CRS(SRS_string = "EPSG:4258")
  #crs <- CRS("+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
  project_vect(vect, crs)
}

project_EPSG_4258_rast <- function(rast){
  crs <- CRS(SRS_string = "EPSG:4258")
  #crs <- CRS("+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
  project_rast(rast, crs)
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
  projectRaster(rast, crs=crs)
} 

reproject_EPSG_4258_vect <- function(vect){
  crs <- CRS(SRS_string = "EPSG:4258")
  reproject_vect(vect, crs)
}

reproject_EPSG_4258_rast <- function(rast){
  crs <- CRS(SRS_string = "EPSG:4258")
  rast <- reproject_rast(rast, crs)
  projection(rast) <- crs
  rast
}


reproject_EPSG_4326_vect <- function(vect){
   crs <- CRS(SRS_string = "EPSG:4326")
   reproject_vect(vect, crs)
}

reproject_EPSG_4326_rast <- function(rast){
  crs <- CRS(SRS_string = "EPSG:4326")
  rast <- reproject_rast(rast, crs)
  projection(rast) <- crs
  rast
}

reproject_EPSG_25831_vect <- function(vect){
  crs <- CRS(SRS_string = "EPSG:25831")
  #crs <- CRS("+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
  reproject_vect(vect, crs)
}

reproject_EPSG_25831_rast <- function(rast){
  crs <- CRS(SRS_string = "EPSG:25831")
  #crs <- CRS("+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
  rast <- reproject_rast(rast, crs)
  projection(rast) <- crs
  rast
}



get_EPSG_4258_vectors_centroids_lat_lng <- function(vects){
  vects$id___ <- as.numeric(row.names(vects))
  if(vects[1,]$id___==0){
    vects$id___ <- vects$id___ +1
  }
  do.call(rbind, lapply(vects$id___, get_EPSG_4258_vectors_centroids_lat_lng_, vects))
}
get_EPSG_4258_vectors_centroids_lat_lng_ <- function(id, vects){
  get_EPSG_4258_vector_centroids_lat_lng(vects[vects$id___==id,])
}


get_EPSG_4258_vector_centroids_lat_lng <- function(vect){
  v <- reproject_EPSG_4258_vect(vect)
  get_coordinates(get_centroids(v))
  # centr <- gCentroid(v)
  # coordinates(centr)
}

get_vectors_centroids_coords <- function(vects){
  vects$id___ <- as.numeric(row.names(vects))
  if(vects[1,]$id___==0){
    vects$id___ <- vects$id___ +1
  }
  do.call(rbind, lapply(vects$id___, get_vectors_centroids_coords_, vects))
}
get_vectors_centroids_coords_ <- function(id, vects){
  get_centroids_coordinates(vects[vects$id___==id,])
}

get_centroids_coordinates <- function(vect){
  get_coordinates(get_centroids(vect))
}

get_coordinates <- function(points){
  coordinates(points)
}

get_centroids <- function(vect){
  gCentroid(vect)
}


convert_coordinates <- function(lats, longs, src.proj, dst.proj) {
  require(sp)
  as.data.frame(
    spTransform(
      SpatialPointsDataFrame(
        coords = data.frame(Xsrc = longs,
                            Ysrc = lats),
        data = data.frame(Xdst = longs,
                          Ydst = lats),
        proj4string = src.proj), dst.proj))
}

