
library(raster)
library(SpaDES)
library(rgdal)
library(parallel)

smoothen_raster <- function(fileName){
  print(paste("smoothen_raster", fileName, Sys.time(),sep="-"))
  rast <- raster(paste("./classificador_vol_america/rasters/", fileName, sep=""))
  fileName <- sub(".tif","",fileName)
  
  dir <- paste("./classificador_vol_america/rasters/",fileName, sep="")
  dir.create(dir)
  
  raster_split <- splitRaster(rast, 10,10, buffer=c(2,2))
  
  n.cores <- detectCores()
  if (n.cores > 10){
    n.cores <- 10
  }
  clust <- makeCluster(n.cores)
  clusterExport(clust, c("raster_split","smoothen_raster_"))
  clusterEvalQ(clust, library(raster))
  
  for(i in 1:10){
    print(paste("start ", i, Sys.time()))
    raster_split <- parLapply(clust, raster_split,smoothen_raster_)
    #raster_split <- lapply(raster_split,smoothen_raster_)
    rast <- mergeRaster(raster_split)
    
    writeRaster(rast,paste(dir, "/", fileName, "_smth_", i, ".tif", sep=""), overwrite=TRUE)
    print(paste("end ", i, Sys.time()))
  }
  stopCluster(clust)
  
  rast <- rast*10
  res <- writeRaster(rast,paste("./classificador_vol_america/rasters/",fileName,"_smth.tif", sep=""), overwrite=TRUE)
  if(exists("res")){
    unlink(dir, recursive = TRUE)
  }
  rast
  #raster_recl <- mergeRaster(raster_split)
  #greyscale <- grey(seq(0, 1, length = 256))
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




# test <- function(x, rast){
#   adj <- adjacent(rast, x, 8, include=TRUE)
#   m <- mean(rast[adj[,2]][rast[adj[,2]]>rast[x]-0.15&rast[adj[,2]]<rast[x]+0.15], na.rm=TRUE)
#   m
# }
# 
# 
# test3 <- function(){
#   r <- rast
#   r<- focal(r, w=matrix(1/9,nrow=3,ncol=3)) 
#   r <- reclassify(r, reclass_m5)
#   r<- focal(r, w=matrix(1/25,nrow=5,ncol=5)) 
#   r <- reclassify(r, reclass_m5)
#   r<- focal(r, w=matrix(1/25,nrow=5,ncol=5)) 
#   r <- reclassify(r, reclass_m5)
#   r<- focal(r, w=matrix(1/9,nrow=3,ncol=3)) 
#   r <- reclassify(r, reclass_m5)
#   
#   plot(r, col=grey_scale(6))
# }


