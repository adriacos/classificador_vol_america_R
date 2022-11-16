
library(raster)
library(SpaDES)
library(rgdal)
library(parallel)


smoothen_raster <- function(id){
  print(paste("smoothen_raster", id, Sys.time(),sep="-"))

  dir <- paste("./classificador_vol_america/rasters/smoothen/",id, sep="")
  
  i_st <- 1
  rast <- raster(paste("./classificador_vol_america/rasters/exported/", id, ".tif", sep=""))

  if(dir.exists(dir)){
    list <- list.files(dir, pattern = "\\.tif$")
    if(length(list)>0){
      i_st <-length(list) + 1 
      rast <- raster(paste(dir, list[length(list)],sep="/"))  
    }
  }else{
    dir.create(dir)
  }
 
  raster_split <- splitRaster(rast, 5,4, buffer=c(2,2))

  n.cores <- detectCores()
  
  clust <- makeCluster(n.cores)
  clusterExport(clust, c("raster_split","smoothen_raster_"), envir = environment())
  clusterEvalQ(clust, library(raster))

  for(i in i_st:12){
    print(paste("start ", i, Sys.time()))
    raster_split <- parLapply(clust, raster_split,smoothen_raster_, seed=i)
    rast <- mergeRaster(raster_split)
    writeRaster(rast,paste(dir, "/", id, "_smth_", i, ".tif", sep=""), overwrite=TRUE)
    print(paste("end ", i, Sys.time()))
  }
  stopCluster(clust)
  rm(clust)
  rm(raster_split)
  rast <- rast*10
  
  #values(rast) <- round_any(values(rast), 0.5)
  
  res <- writeRaster(rast,paste("./classificador_vol_america/rasters/smoothen/",id,"_smth.tif", sep=""), overwrite=TRUE)
  res <- writeRaster(rast,paste("./classificador_vol_america/rasters/smoothen/bkp/",id,"_smth.tif", sep=""), overwrite=TRUE)
  if(exists("res")){
    unlink(dir, recursive = TRUE)
    #file.remove(paste("./classificador_vol_america/rasters/exported/", id, ".tif", sep=""))
  }
  rm(dir)
  rm(res)
  rast
}

smoothen_raster_ <- function(rast, seed=8){
  smoothen_raster__ <- function(x, rast){
    adj <- adjacent(rast, x, 8, include=TRUE)
    m <- mean(rast[adj[,2]][rast[adj[,2]]>rast[x]-0.15&rast[adj[,2]]<rast[x]+0.15], na.rm=TRUE)
    m
  }
  
  set.seed(seed)
  cells <- cellFromRow(rast, c(1:nrow(rast)))
  cells <- sample(cells)
  
  values(rast)[cells] <- sapply(cells, smoothen_raster__, rast=rast)
  rast
}


smoothen_raster_dp <- function(id, threshold=0.15){
  print(paste("smoothen_raster", id, Sys.time(),sep="-"))
  
  rast <- raster(paste("./classificador_vol_america/rasters/", id, ".tif", sep=""))
  
  raster_split <- splitRaster(rast, 2,2, buffer=c(2,2))
  
  n.cores <- detectCores()
  
  clust <- makeCluster(n.cores, outfile="log_smoothen.txt")
  clusterExport(clust, c("raster_split","smoothen_raster_dp_"), envir =  environment())
  clusterEvalQ(clust, library(raster))
  
  raster_split <- parLapply(clust, raster_split,smoothen_raster_dp_, threshold=threshold, seed=21)
  rast_1 <- mergeRaster(raster_split)
  
  stopCluster(clust)
  
  
  raster_split <- splitRaster(rast, 2,2, buffer=c(2,2))
  
  n.cores <- detectCores()
  
  clust <- makeCluster(n.cores, outfile="log_smoothen.txt")
  clusterExport(clust, c("raster_split","smoothen_raster_dp_"), envir =  environment())
  clusterEvalQ(clust, library(raster))
  
  raster_split <- parLapply(clust, raster_split,smoothen_raster_dp_, threshold=threshold, seed=4)
  rast_2 <- mergeRaster(raster_split)
  
  stopCluster(clust)
  
  
  raster_split <- splitRaster(rast, 2,2, buffer=c(2,2))
  
  n.cores <- detectCores()
  
  clust <- makeCluster(n.cores, outfile="log_smoothen.txt")
  clusterExport(clust, c("raster_split","smoothen_raster_dp_"), envir =  environment())
  clusterEvalQ(clust, library(raster))
  
  raster_split <- parLapply(clust, raster_split,smoothen_raster_dp_, threshold=threshold, seed=15)
  rast_3 <- mergeRaster(raster_split)
  
  stopCluster(clust)
  rm(raster_split)
  gc()
  
  rast <- mean(rast_1, rast_2, rast_3, na.rm=T)
  
  rast <- rast*10
  res <- writeRaster(rast,paste("./classificador_vol_america/rasters/smoothen/",id,"_smth_dp.tif", sep=""), overwrite=TRUE)
  print(paste("smoothen_raster END", id, Sys.time(),sep="-"))
  rast
}





smoothen_raster_dp_ <- function(rast, threshold=0.15, seed=8){
  set.seed(seed)
  cells <- cellFromRow(rast, c(1:nrow(rast)))
  cells <- sample(cells)
  
  for (x in 1:length(cells)){
    if(is.na(rast[x])){
      rast[x] <- NA
    }
    adj <- adjacent(rast, x, 8, include=TRUE)
    t <- rast[adj[,2]][!is.na(rast[adj[,2]]) & rast[adj[,2]]>1] - 1
    t <- t[abs((t)-rast[x])<threshold]
    if(length(t)==0){
      rast[x] <- mean(rast[adj[,2]][!is.na(rast[adj[,2]])&rast[adj[,2]]>rast[x]-threshold&rast[adj[,2]]<rast[x]+threshold], na.rm=TRUE)+1
      #return(mean(rast[adj[,2]][!is.na(rast[adj[,2]])&rast[adj[,2]]>rast[x]-threshold&rast[adj[,2]]<rast[x]+threshold], na.rm=TRUE)+1)
    } else {
      t <- t[which.min(abs((t)-rast[x]))]
      rast[x] <- t[1]+1
      #return(t[1]+1)
    }
  }
  rast-1
}


count_classes_by_polygons <- function(vect, rast, threshold){
  if(!"id" %in% (ids(vect))){
    vect$id <- as.numeric(row.ids(vect))
    if(vect[1,]$id==0){
      vect$id <- vect$id +1
    }
  }
  ids <- vect$id
  
  n.cores <- detectCores()

  clust <- makeCluster(n.cores, outfile="log_smoothen.txt")
  clusterExport(clust, c("ids", "vect","vect", "threshold", "count_classes_by_polygons_", "count_classes_by_polygon", "count_classes"), envir =  environment())
  clusterEvalQ(clust, library(raster))
  c <- parSapply(clust, ids, count_classes_by_polygons_, vect, rast, threshold)
  stopCluster(clust)
  c
}

count_classes_by_polygons_ <- function(id, vect, rast, threshold){
  print(id)
  count_classes_by_polygon(vect[vect$id==id,], rast, threshold)
}

count_classes_by_polygon <- function(polygon, rast, threshold){
  r <- crop(rast, polygon)
  r <- mask(r, polygon)
  count_classes(r, threshold)
}


count_classes <- function(r, threshold){
  
  count <- 0
  set.seed(8)
  cells <- cellFromRow(rast, c(1:nrow(rast)))
  cells <- sample(cells)
  
  for (x in 1:length(cells)){
    if(is.na(r[x])){
      r[x] <- NA
    }
    adj <- adjacent(r, x, 8, include=TRUE)
    t <- r[adj[,2]][!is.na(r[adj[,2]]) & r[adj[,2]]>1] - 1
    t <- t[abs((t)-r[x])<threshold]
    if(length(t)==0){
      count <- count + 1
      r[x] <- mean(r[adj[,2]][!is.na(r[adj[,2]])&r[adj[,2]]>r[x]-threshold&r[adj[,2]]<r[x]+threshold], na.rm=TRUE)+1
      #return(mean(r[adj[,2]][!is.na(r[adj[,2]])&r[adj[,2]]>r[x]-threshold&r[adj[,2]]<r[x]+threshold], na.rm=TRUE)+1)
    } else {
      t <- t[which.min(abs((t)-r[x]))]
      r[x] <- t[1]+1
      #return(t[1]+1)
    }
  }
  count
}





calc_TPI_by_polygons <- function(vect, rast){
  if(!"id" %in% (names(vect))){
    vect$id <- as.numeric(row.names(vect))
    if(vect[1,]$id==0){
      vect$id <- vect$id +1
    }
  }
  ids <- vect$id
  
  n.cores <- detectCores()
  
  clust <- makeCluster(n.cores, outfile="log_smoothen.txt")
  clusterExport(clust, c("ids", "vect","rast", "calc_TPI_by_polygons_", "calc_TPI_by_polygon", "calc_TPI_raster"), envir =  environment())
  clusterEvalQ(clust, library(raster))
  c <- parSapply(clust, ids, calc_TPI_by_polygons_, vect, rast)
  stopCluster(clust)
  
  c
}

calc_TPI_by_polygons_ <- function(id, vect, rast){
  print(id)
  calc_TPI_by_polygon(vect[vect$id==id,], rast)
}

calc_TPI_by_polygon <- function(polygon, rast){
  r <- crop(rast, polygon)
  r <- mask(r, polygon)
  r <- calc_TPI_raster(r)
  mean(values(r), na.rm=T)
}

calc_TPI_raster <- function(rast){
  cells <- cellFromRow(rast, c(1:nrow(rast)))
  c <- c()
  for (x in 1:length(cells)){
    if(is.na(rast[x])){
      rast[x] <- NA
    }
    adj <- adjacent(rast, x, 8, include=FALSE)
    c <- append(c, abs(mean(rast[adj[,2]][!is.na(rast[adj[,2]])], na.rm=T)-rast[x]))
  }
  values(rast) <- c
  rast
}


calc_TRI_by_polygons <- function(vect, rast){
  if(!"id" %in% (names(vect))){
    vect$id <- as.numeric(row.names(vect))
    if(vect[1,]$id==0){
      vect$id <- vect$id +1
    }
  }
  ids <- vect$id
  
  n.cores <- detectCores()
  
  clust <- makeCluster(n.cores, outfile="log_smoothen.txt")
  clusterExport(clust, c("ids", "vect","rast", "calc_TRI_by_polygons_", "calc_TRI_by_polygon", "calc_TRI_raster"), envir =  environment())
  clusterEvalQ(clust, library(raster))
  c <- parSapply(clust, ids, calc_TRI_by_polygons_, vect, rast)
  stopCluster(clust)
  c
}

calc_TRI_by_polygons_ <- function(id, vect, rast){
  print(id)
  calc_TRI_by_polygon(vect[vect$id==id,], rast)
}

calc_TRI_by_polygon <- function(polygon, rast){
  r <- crop(rast, polygon)
  r <- mask(r, polygon)
  r <- calc_TRI_raster(r)
  mean(values(r), na.rm=T)
}

calc_TRI_raster <- function(rast){
  cells <- cellFromRow(rast, c(1:nrow(rast)))
  c <- c()
  for (x in 1:length(cells)){
    if(is.na(rast[x])){
      rast[x] <- NA
    }
    adj <- adjacent(rast, x, 8, include=FALSE)
    c <- append(c, mean(abs(rast[adj[,2]][!is.na(rast[adj[,2]])]-rast[x]), na.rm=T))
  }
  values(rast) <- c
  rast
}



calc_roughness_by_polygons <- function(vect, rast){
  if(!"id" %in% (names(vect))){
    vect$id <- as.numeric(row.names(vect))
    if(vect[1,]$id==0){
      vect$id <- vect$id +1
    }
  }
  ids <- vect$id
  
  n.cores <- detectCores()
  
  clust <- makeCluster(n.cores, outfile="log_smoothen.txt")
  clusterExport(clust, c("ids", "vect","rast", "calc_roughness_by_polygons_", "calc_roughness_by_polygon", "calc_roughness_raster"), envir =  environment())
  clusterEvalQ(clust, library(raster))
  c <- parSapply(clust, ids, calc_roughness_by_polygons_, vect, rast)
  stopCluster(clust)
  c
}

calc_roughness_by_polygons_ <- function(id, vect, rast){
  print(id)
  calc_roughness_by_polygon(vect[vect$id==id,], rast)
}

calc_roughness_by_polygon <- function(polygon, rast){
  r <- crop(rast, polygon)
  r <- mask(r, polygon)
  r <- calc_roughness_raster(r)
  mean(values(r), na.rm=T)
}

calc_roughness_raster <- function(rast){
  cells <- cellFromRow(rast, c(1:nrow(rast)))
  c <- c()
  for (x in 1:length(cells)){
    if(is.na(rast[x])){
      rast[x] <- NA
    }
    adj <- adjacent(rast, x, 8, include=TRUE)
    max <- max(rast[adj[,2]][!is.na(rast[adj[,2]])], na.rm=T)
    min <- min(rast[adj[,2]][!is.na(rast[adj[,2]])], na.rm=T)
    c <- append(c, max-min)
  }
  values(rast) <- c
  rast
}


#C:/PROGRA~1/QGIS32~1.3/apps/grass/grass78
# C:/Program Files/QGIS 3.26.3/apps/grass/grass78
# C:/Users/acosd/Desktop/CREAF/Proves/classificador_vol_america_R/classificador_vol_america/rasters
# 
# initGRASS(gisBase = "C:/PROGRA~1/QGIS32~1.3/apps/grass/grass78/",
#           gisDbase = "C:/Users/acosd/Documents/grassdata/",
#           location = "demolocation", 
#           mapset = "PERMANENT")