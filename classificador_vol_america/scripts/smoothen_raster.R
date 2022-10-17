
library(raster)
library(SpaDES)
library(rgdal)
library(parallel)

smoothen_raster <- function(fileName){
  print(paste("smoothen_raster", fileName, Sys.time(),sep="-"))

  dir <- paste("./classificador_vol_america/rasters/smoothen/",sub(".tif","",fileName), sep="")
  
  i_st <- 1
  rast <- raster(paste("./classificador_vol_america/rasters/", fileName, sep=""))

  if(dir.exists(dir)){
    list <- list.files(dir)
    if(length(list)>0){
      i_st <-length(list) + 1 
      rast <- raster(paste(dir, list[length(list)],sep="/"))  
    }
  }else{
    dir.create(dir)
  }
  
  fileName <- sub(".tif","",fileName)
 
  raster_split <- splitRaster(rast, 10,10, buffer=c(2,2))

  n.cores <- detectCores()
  
  clust <- makeCluster(n.cores)
  clusterExport(clust, c("raster_split","smoothen_raster_"), envir = environment())
  clusterEvalQ(clust, library(raster))

  for(i in i_st:20){
    print(paste("start ", i, Sys.time()))
    raster_split <- parLapply(clust, raster_split,smoothen_raster_)
    rast <- mergeRaster(raster_split)
    
    writeRaster(rast,paste(dir, "/", fileName, "_smth_", i, ".tif", sep=""), overwrite=TRUE)
    print(paste("end ", i, Sys.time()))
  }
  stopCluster(clust)
  
  rast <- rast*10
  res <- writeRaster(rast,paste("./classificador_vol_america/rasters/smoothen/",fileName,"_smth_20.tif", sep=""), overwrite=TRUE)
  if(exists("res")){
    #unlink(dir, recursive = TRUE)
  }
  rast
}


smoothen_raster_1_2 <- function(fileName){
  print(paste("smoothen_raster", fileName, Sys.time(),sep="-"))
  
  dir <- paste("./classificador_vol_america/rasters/smoothen/1_2/",sub(".tif","",fileName), sep="")
  
  i_st <- 1
  rast <- raster(paste("./classificador_vol_america/rasters/", fileName, sep=""))
  
  if(dir.exists(dir)){
    list <- list.files(dir)
    if(length(list)>0){
      i_st <-length(list) + 1 
      rast <- raster(paste(dir, list[length(list)],sep="/"))  
    }
  }else{
    dir.create(dir)
  }
  
  fileName <- sub(".tif","",fileName)
  
  raster_split <- splitRaster(rast, 10,10, buffer=c(2,2))
  
  n.cores <- detectCores()
  
  clust <- makeCluster(n.cores)
  clusterExport(clust, c("raster_split","smoothen_raster_1_2_"), envir = environment())
  clusterEvalQ(clust, library(raster))
  
  for(i in i_st:15){
    print(paste("start ", i, Sys.time()))
    raster_split <- parLapply(clust, raster_split,smoothen_raster_1_2_)
    rast <- mergeRaster(raster_split)
    
    writeRaster(rast,paste(dir, "/", fileName, "_smth_", i, ".tif", sep=""), overwrite=TRUE)
    print(paste("end ", i, Sys.time()))
  }
  stopCluster(clust)
  
  rast <- rast*10
  res <- writeRaster(rast,paste("./classificador_vol_america/rasters/smoothen/",fileName,"_smth_1_2.tif", sep=""), overwrite=TRUE)
  if(exists("res")){
    #unlink(dir, recursive = TRUE)
  }
  rast
}

smoothen_raster_16 <- function(fileName){
  print(paste("smoothen_raster", fileName, Sys.time(),sep="-"))
  
  dir <- paste("./classificador_vol_america/rasters/smoothen/16/",sub(".tif","",fileName), sep="")
  
  i_st <- 1
  rast <- raster(paste("./classificador_vol_america/rasters/", fileName, sep=""))
  
  if(dir.exists(dir)){
    list <- list.files(dir)
    if(length(list)>0){
      i_st <-length(list) + 1 
      rast <- raster(paste(dir, list[length(list)],sep="/"))  
    }
  }else{
    dir.create(dir)
  }
  
  fileName <- sub(".tif","",fileName)
  
  raster_split <- splitRaster(rast, 10,10, buffer=c(2,2))
  
  n.cores <- detectCores()
  
  clust <- makeCluster(n.cores)
  clusterExport(clust, c("raster_split","smoothen_raster_16_"), envir = environment())
  clusterEvalQ(clust, library(raster))
  
  for(i in i_st:15){
    print(paste("start ", i, Sys.time()))
    raster_split <- parLapply(clust, raster_split,smoothen_raster_16_)
    rast <- mergeRaster(raster_split)
    
    writeRaster(rast,paste(dir, "/", fileName, "_smth_", i, ".tif", sep=""), overwrite=TRUE)
    print(paste("end ", i, Sys.time()))
  }
  stopCluster(clust)
  
  rast <- rast*10
  res <- writeRaster(rast,paste("./classificador_vol_america/rasters/smoothen/",fileName,"_smth_16.tif", sep=""), overwrite=TRUE)
  if(exists("res")){
    #unlink(dir, recursive = TRUE)
  }
  rast
}

smoothen_raster_1_2_16 <- function(fileName){
  print(paste("smoothen_raster", fileName, Sys.time(),sep="-"))
  
  dir <- paste("./classificador_vol_america/rasters/smoothen/1_2_16",sub(".tif","",fileName), sep="")
  
  i_st <- 1
  rast <- raster(paste("./classificador_vol_america/rasters/", fileName, sep=""))
  
  if(dir.exists(dir)){
    list <- list.files(dir)
    if(length(list)>0){
      i_st <-length(list) + 1 
      rast <- raster(paste(dir, list[length(list)],sep="/"))  
    }
  }else{
    dir.create(dir)
  }
  
  fileName <- sub(".tif","",fileName)
  
  raster_split <- splitRaster(rast, 10,10, buffer=c(2,2))
  
  n.cores <- detectCores()
  
  clust <- makeCluster(n.cores)
  clusterExport(clust, c("raster_split","smoothen_raster_1_2_16_"), envir = environment())
  clusterEvalQ(clust, library(raster))
  
  for(i in i_st:15){
    print(paste("start ", i, Sys.time()))
    raster_split <- parLapply(clust, raster_split,smoothen_raster_1_2_16_)
    rast <- mergeRaster(raster_split)
    
    writeRaster(rast,paste(dir, "/", fileName, "_smth_", i, ".tif", sep=""), overwrite=TRUE)
    print(paste("end ", i, Sys.time()))
  }
  stopCluster(clust)
  
  rast <- rast*10
  res <- writeRaster(rast,paste("./classificador_vol_america/rasters/smoothen/",fileName,"_smth_1_2_16.tif", sep=""), overwrite=TRUE)
  if(exists("res")){
    #unlink(dir, recursive = TRUE)
  }
  rast
}

smoothen_raster_test <- function(fileName){
  print(paste("smoothen_raster_test", fileName, Sys.time(),sep="-"))
  
  dir <- paste("./classificador_vol_america/rasters/smoothen/test/",sub(".tif","",fileName), sep="")
  
  i_st <- 1
  rast <- raster(paste("./classificador_vol_america/rasters/", fileName, sep=""))
  
  if(dir.exists(dir)){
    list <- list.files(dir)
    if(length(list)>0){
      i_st <-length(list) + 1 
      rast <- raster(paste(dir, list[length(list)],sep="/"))  
    }
  }else{
    dir.create(dir)
  }
  
  fileName <- sub(".tif","",fileName)
  
  raster_split <- splitRaster(rast, 10,10, buffer=c(2,2))
  
  n.cores <- detectCores()
  
  clust <- makeCluster(n.cores, outfile="log_smoothen.txt")
  clusterExport(clust, c("raster_split","smoothen_raster_test_"), envir = environment())
  clusterEvalQ(clust, library(raster))
  
  for(i in 1:1){
    print(paste("start ", i, Sys.time()))
    raster_split <- parLapply(clust, raster_split,smoothen_raster_test_)
    rast <- mergeRaster(raster_split)
    rast <- rast-1
    #writeRaster(rast,paste(dir, "/", fileName, "_smth_test2_", i, ".tif", sep=""), overwrite=TRUE)
    print(paste("end ", i, Sys.time()))
  }
  stopCluster(clust)
  
  rast <- rast*10
  res <- writeRaster(rast,paste("./classificador_vol_america/rasters/smoothen/",fileName,"_smth_test2.tif", sep=""), overwrite=TRUE)
  if(exists("res")){
    #unlink(dir, recursive = TRUE)
  }
  rast
}



smoothen_raster_test_ <- function(rast){
  smoothen_raster__ <- function(x, rast){
    adj <- adjacent(rast, x, 8, include=TRUE)
    t <- rast[adj[,2][rast[adj[,2]]>1]] - 1
    t <- t[abs((t)-rast[x])<0.15]
    t <- t[min(abs((t)-rast[x]))]
    #t <- (abs(t-rast[x]))
    if(length(t)==0){
      #print("m")
      return(mean(rast[adj[,2]][rast[adj[,2]]>rast[x]-0.15&rast[adj[,2]]<rast[x]+0.15], na.rm=TRUE)+1)
    } else {
      print("t")
      return(t[1]+1)
    }
    
    # m <- mean(rast[adj[,2]][rast[adj[,2]]>rast[x]-0.12&rast[adj[,2]]<rast[x]+0.12], na.rm=TRUE)
    # m
  }
  cells <- cellFromRow(rast, c(1:nrow(rast)))
  values(rast) <- sapply(cells, smoothen_raster__, rast=rast)
  rast
}

smoothen_raster_ <- function(rast){
  smoothen_raster__ <- function(x, rast){
    adj <- adjacent(rast, x, 8, include=TRUE)
    m <- mean(rast[adj[,2]][rast[adj[,2]]>rast[x]-0.15&rast[adj[,2]]<rast[x]+0.15], na.rm=TRUE)
    m
  }
  
  cells <- cellFromRow(rast, c(1:nrow(rast)))
  values(rast) <- sapply(cells, smoothen_raster__, rast=rast)
  rast
}

smoothen_raster_16_ <- function(rast){
  smoothen_raster__ <- function(x, rast){
    adj <- adjacent(rast, x, 16, include=TRUE)
    m <- mean(rast[adj[,2]][rast[adj[,2]]>rast[x]-0.15&rast[adj[,2]]<rast[x]+0.15], na.rm=TRUE)
    m
  }
  
  cells <- cellFromRow(rast, c(1:nrow(rast)))
  values(rast) <- sapply(cells, smoothen_raster__, rast=rast)
  rast
}

smoothen_raster_1_2_ <- function(rast){
  smoothen_raster__ <- function(x, rast){
    adj <- adjacent(rast, x, 8, include=TRUE)
    m <- mean(rast[adj[,2]][rast[adj[,2]]>rast[x]-0.12&rast[adj[,2]]<rast[x]+0.12], na.rm=TRUE)
    m
  }
  
  cells <- cellFromRow(rast, c(1:nrow(rast)))
  values(rast) <- sapply(cells, smoothen_raster__, rast=rast)
  rast
}


smoothen_raster_1_2_16_ <- function(rast){
  smoothen_raster__ <- function(x, rast){
    adj <- adjacent(rast, x, 16, include=TRUE)
    m <- mean(rast[adj[,2]][rast[adj[,2]]>rast[x]-0.12&rast[adj[,2]]<rast[x]+0.12], na.rm=TRUE)
    m
  }
  
  cells <- cellFromRow(rast, c(1:nrow(rast)))
  values(rast) <- sapply(cells, smoothen_raster__, rast=rast)
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