
library(raster)
library(SpaDES)
library(rgdal)
library(parallel)

contrast_raster_dp <- function(fileName){
  print(paste("contrast_raster", fileName, Sys.time(),sep="-"))

  dir <- paste("./classificador_vol_america/rasters/contrast/",sub(".tif","",fileName), sep="")
  
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
  clusterExport(clust, c("raster_split","contrast_raster_"), envir = environment())
  clusterEvalQ(clust, library(raster))

  for(i in i_st:15){
    print(paste("start ", i, Sys.time()))
    raster_split <- parLapply(clust, raster_split,contrast_raster_)
    rast <- mergeRaster(raster_split)
    
    writeRaster(rast,paste(dir, "/", fileName, "_cntr_", i, ".tif", sep=""), overwrite=TRUE)
    print(paste("end ", i, Sys.time()))
  }
  stopCluster(clust)
  
  rast <- rast*10
  res <- writeRaster(rast,paste("./classificador_vol_america/rasters/contrast/",fileName,"_cntr_20.tif", sep=""), overwrite=TRUE)
  if(exists("res")){
    #unlink(dir, recursive = TRUE)
  }
  rast
}



contrast_raster <- function(fileName, threshold=0.15){
  print(paste("contrast_raster", fileName, Sys.time(),sep="-"))
  
  rast <- raster(paste("./classificador_vol_america/rasters/", fileName, sep=""))
  fileName <- sub(".tif","",fileName)
  
  raster_split <- splitRaster(rast, 10,10, buffer=c(2,2))
  
  n.cores <- detectCores()
  
  clust <- makeCluster(n.cores, outfile="log_contrast.txt")
  clusterExport(clust, c("raster_split","contrast_raster_"), envir =  environment())
  clusterEvalQ(clust, library(raster))
  
  raster_split <- parLapply(clust, raster_split,contrast_raster_, threshold=threshold)
  rast <- mergeRaster(raster_split)
  
  stopCluster(clust)
  
  rast <- rast*10
  res <- writeRaster(rast,paste("./classificador_vol_america/rasters/contrast/",fileName,"_cntr.tif", sep=""), overwrite=TRUE)

  rast
}



contrast_raster_ <- function(rast, threshold=0.15){
  cells <- cellFromRow(rast, c(1:nrow(rast)))
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

# contrast_raster_ <- function(rast){
#   
#   r <- rast 
#   contrast_raster__ <- function(x, rast){
#     if(is.na(rast[x])){
#       return(NA)
#     }
#     adj <- adjacent(rast, x, 8, include=TRUE)
#     t <- rast[adj[,2]][!is.na(rast[adj[,2]]) & rast[adj[,2]]>1] - 1
#     t <- t[abs((t)-rast[x])<0.15]
#     if(length(t)==0){
#       return(mean(rast[adj[,2]][!is.na(rast[adj[,2]])&rast[adj[,2]]>rast[x]-0.15&rast[adj[,2]]<rast[x]+0.15], na.rm=TRUE)+1)
#     } else {
#       t <- t[which.min(abs((t)-rast[x]))]
#       return(t[1]+1)
#     }
#   }
#   cells <- cellFromRow(rast, c(1:nrow(rast)))
#   values(rast) <- sapply(cells, contrast_raster__, rast=rast)
#   rast
# }

contrast_raster_dp_ <- function(rast){
  contrast_raster__ <- function(x, rast){
    adj <- adjacent(rast, x, 8, include=TRUE)
    m <- mean(rast[adj[,2]][rast[adj[,2]]>rast[x]-0.09&rast[adj[,2]]<rast[x]+0.09], na.rm=TRUE)
    m
  }
  
  cells <- cellFromRow(rast, c(1:nrow(rast)))
  values(rast) <- sapply(cells, contrast_raster__, rast=rast)
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