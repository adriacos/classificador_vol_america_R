
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

  for(i in i_st:15){
    print(paste("start ", i, Sys.time()))
    raster_split <- parLapply(clust, raster_split,smoothen_raster_)
    rast <- mergeRaster(raster_split)
    
    writeRaster(rast,paste(dir, "/", fileName, "_smth_", i, ".tif", sep=""), overwrite=TRUE)
    print(paste("end ", i, Sys.time()))
  }
  stopCluster(clust)
  
  rast <- rast*10
  res <- writeRaster(rast,paste("./classificador_vol_america/rasters/smoothen/",fileName,"_smth.tif", sep=""), overwrite=TRUE)
  if(exists("res")){
    unlink(dir, recursive = TRUE)
  }
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
