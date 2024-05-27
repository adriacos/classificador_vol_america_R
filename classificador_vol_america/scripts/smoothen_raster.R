
library(raster)
# library(SpaDES)
# library(rgdal)
library(parallel)
library(supercells)
library(stars)
library(plyr)
library(stringr)
library(terra)
library(exactextractr)

#TODO: change directory to rasters/smoothen + rasters/original

# source("./classificador_vol_america/scripts/clump_vector.R")

smoothen_raster_ini <- function(){
  # print("smoothen_raster_ini")
  print(paste(Sys.time()," smoothen_raster_ini"))
  if(!file.exists("./classificador_vol_america/temp/smoothen_raster_current.RData")){
    smoothen_raster_current <- "ini"
    save(smoothen_raster_current,file="./classificador_vol_america/temp/smoothen_raster_current.RData")
    rm(smoothen_raster_current)
    smoothen_raster_ini()
  }else{
    load("./classificador_vol_america/temp/smoothen_raster_current.RData")
    if(smoothen_raster_current=="ini"){
      smoothen_raster_set()
    }
    if(smoothen_raster_current=="set"){
      library(rstudioapi)
      restartSession(command=source("./classificador_vol_america/scripts/smoothen_raster_supercells_do.R"))
    }else if(smoothen_raster_current=="smoothen"){
      library(rstudioapi)
      restartSession(command=source("./classificador_vol_america/scripts/merge_rasters_mosaic.R"))
    }else if(smoothen_raster_current=="merged"){
      # unlink("./classificador_vol_america/temp/smoothen_raster_grid.RData")
      current_global <- "smoothen"
      save(current_global,file="./classificador_vol_america/temp/current_global.RData")
      unlink("./classificador_vol_america/temp/smoothen_raster_current.RData")
      library(rstudioapi)
      restartSession(command=source("./classificador_vol_america/scripts/continue.R"))
      # continue()
    }
  }
  
}

smoothen_raster_set <- function(){
  # print("smoothen_raster_set")
  print(paste(Sys.time()," smoothen_raster_set"))
  source("./classificador_vol_america/scripts/clump_vector.R")
  rast <- raster("./classificador_vol_america/rasters/original/original.tif")
  # limit <- st_read("./classificador_vol_america/vect/grids/limit.gpkg")
  if(file.exists("./classificador_vol_america/temp/smoothen_raster_grid.RData")){
    load("./classificador_vol_america/temp/smoothen_raster_grid.RData")
  }else{
    # max_val <- 255
    # save(max_val,file="./classificador_vol_america/temp/max_val.RData")
    load("./classificador_vol_america/temp/max_val.RData")
    load("./classificador_vol_america/temp/resolution.RData")
    limit <- st_read("./classificador_vol_america/vect/limit.gpkg")
    area <- as.numeric(st_area(limit))
    library(parallel)
    factor <- sqrt(detectCores()/4)*sqrt(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024/12000)
    smoothen_raster_grid <- create_grid(limit,round(sqrt(area)/(factor*resolution*6000)),round(sqrt(area)/(factor*resolution*6000)))
    save(smoothen_raster_grid,file="./classificador_vol_america/temp/smoothen_raster_grid.RData")
    rm(area)
    rm(limit)
    rm(resolution)
  }
  dir.create("./classificador_vol_america/rasters/original/split",showWarnings=F)
  ids_done <- sapply(list.files("./classificador_vol_america/rasters/original/split",pattern=".tif$"),function(x){
    as.numeric(sub(".tif","",x))
  })
  ids_done <- ids_done[-which.max(sapply(list.files("./classificador_vol_america/rasters/original/split",pattern=".tif$",full.names=T),function(x){
    file.info(x)$mtime
  }))]
  smoothen_raster_grid <- smoothen_raster_grid[!smoothen_raster_grid$id%in%ids_done,]
  print(paste("Smoothen set - grid of ",nrow(smoothen_raster_grid), " elements to cut from raster",sep=""))
  n.cores <- detectCores()
  cl <- create_cluster_clump_(n.cores,"smoothen_raster_set")
  parLapplyLB(cl,smoothen_raster_grid$id,function(id, smoothen_raster_grid, rast){
    print(smoothen_raster_grid[smoothen_raster_grid$id==id,]$id)
    buf <- st_buffer(smoothen_raster_grid[smoothen_raster_grid$id==id,], 10)
    crop <- mask(crop(rast,buf),buf)
    unlink(paste("./classificador_vol_america/rasters/original/split/",id,".tif",sep=""))
    writeRaster(crop,paste("./classificador_vol_america/rasters/original/split/",id,".tif",sep=""))
    # crop
  },smoothen_raster_grid,rast)
  stopCluster(cl)
  rm(cl)
  unlink("./classificador_vol_america/logs/smoothen_raster_set.txt")
  ids_done <- sapply(list.files("./classificador_vol_america/rasters/original/split",pattern=".tif$"),function(x){
    as.numeric(sub(".tif","",x))
  })
  smoothen_raster_grid <- smoothen_raster_grid[!smoothen_raster_grid$id%in%ids_done,]
  if(nrow(smoothen_raster_grid)>0){
    restartSession(command=source("./classificador_vol_america/scripts/smoothen_raster.R"))
  }
  smoothen_raster_current <- "set"
  save(smoothen_raster_current,file="./classificador_vol_america/temp/smoothen_raster_current.RData")
  smoothen_raster_ini()
}


# smoothen_raster_supercells_do <- function(max_val,smoothen_raster_grids){
#   load("./classificador_vol_america/temp/max_val.RData")
#   load("./classificador_vol_america/temp/smoothen_raster_grids.RData")
#   raster_split <- lapply(paste("./classificador_vol_america/rasters/pnoa/split/",list.files("./classificador_vol_america/rasters/pnoa/split/",pattern="\\.tif$"),sep=""),raster)
#   names(raster_split) <- lapply(raster_split,function(r){
#     sub(" ","",sub(" ","",sub(".tif","",basename(r@file@name))))
#   })
#   ids_done <- sub(".tif","",list.files("./classificador_vol_america/rasters/pnoa/split/supercells",pattern="\\.tif$"))
#   raster_split <- raster_split[!names(raster_split)%in%ids_done]
#   
#   tryCatch(
#     {
#       n.cores <- detectCores()
#       clust <- create_cluster_clump_(n.cores)
#       raster_split <- parLapplyLB(clust, raster_split,smoothen_raster_supercells_,max_val)
#       stopCluster(clust)
#     },
#     error=function() {
#       print("error")
#       stopCluster(cl)
#       rm(cl)
#       rm(raster_split)
#       gc()
#       smoothen_raster_supercells_do(raster_split,max_val,smoothen_raster_grids)
#       restartSession(command=source("./classificador_vol_america/scripts/smoothen_raster_supercells_do.R"))
#     }
#   )
#   merge_rasters_mosaic()
# } 


smoothen_raster_supercells_ <- function(rast,max_val,smoothen_raster_grid,pretty=T){
  id <- sub(" ","",sub(" ","",sub(".tif","",basename(rast@file@name))))
  print(id)
  unlink(paste("./classificador_vol_america/rasters/smoothen/split/",id,".tif",sep=""))
  print(Sys.time())
  if(length(unique(values(rast)))==1&is.na(unique(values(rast))[1])){
    unlink(paste("./classificador_vol_america/rasters/original/split/",id,".tif",sep=""))
    return(T)
  }
  g <- smoothen_raster_grid[smoothen_raster_grid$id==id,]
  
  rast <- rast(rast)
  area <- as.numeric(st_area(g))
  rm(g)
  if(ceiling(9*area/500)<3){
    writeRaster(raster(rast),paste("./classificador_vol_america/rasters/smoothen/split/",id,".tif",sep=""))
    unlink(paste("./classificador_vol_america/rasters/original/split/",id,".tif",sep=""))
    return()
  }
  rast_sc = supercells(rast, k = ceiling(9*area/500), compactness = 0.5)
  rm(area)
  ex <- exact_extract(rast, rast_sc, "mean", progress=F)
  rast_sc$value <- ex
  rm(ex)
  # 
  # v <- vect(rast_sc)
  # r <- rast(v, ncols=ncol(rast), nrows=nrow(rast))
  # z <- rasterize(v, r, "layer")
  # values(z) <- round_any(10*values(z)/max_val, 0.5) 
  # z <- rast(z)
  # v <- as.polygons(z, dissolve=T, trunc=F)
  # # rm(z)
  # v <- disagg(v)
  # # vect <- as(vect, "Spatial")
  # v <- st_as_sf(v)
  # 
  # # 
  # rast_sc <-st_rasterize(rast_sc[,"value"],st_as_stars(st_bbox(rast_sc),res=res(rast)))
  rast_sc <- st_rasterize(rast_sc[,"value"],st_as_stars(pretty=pretty,st_bbox(rast_sc),nx=nrow(rast),ny=ncol(rast)))
  rast_sc <- rast(rast_sc)
  rast_sc <- raster(rast_sc)
  rast_sc <- resample(rast_sc,raster(rast))
  values(rast_sc) <- round_any(10*values(rast_sc)/max_val, 0.5) 
  writeRaster(rast_sc,paste("./classificador_vol_america/rasters/smoothen/split/",id,".tif",sep=""))
  unlink(paste("./classificador_vol_america/rasters/original/split/",id,".tif",sep=""))
  # rast_sc
  return(TRUE)
}

smoothen_failed_ <- function(){
  files <- list.files("./classificador_vol_america/rasters/original/split/",pattern="\\.tif$")
  if(length(files>0)){
    raster_split <- lapply(paste("./classificador_vol_america/rasters/original/split/",files,sep=""),raster)
    names(raster_split) <- lapply(raster_split,function(r){
      sub(" ","",sub(" ","",sub(".tif","",basename(r@file@name))))
    })
    smoothen_raster_do_ids_failed <- names(raster_split)
    rm(smoothen_raster_do_ids_failed)
    n.cores <- detectCores()
    unlink("./classificador_vol_america/logs/smoothen_raster_supercells_failed_.txt")
    clust <- create_cluster_clump_(n.cores,"smoothen_raster_supercells_failed_")
    parLapplyLB(clust,raster_split,smoothen_raster_supercells_,max_val,smoothen_raster_grid,pretty=F)
    stopCluster(clust)
    rm(raster_split)
    rm(clust)
  }
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








#C:/PROGRA~1/QGIS32~1.3/apps/grass/grass78
# C:/Program Files/QGIS 3.26.3/apps/grass/grass78
# C:/Users/acosd/Desktop/CREAF/Proves/classificador_vol_america_R/classificador_vol_america/rasters
# 
# initGRASS(gisBase = "C:/PROGRA~1/QGIS32~1.3/apps/grass/grass78/",
#           gisDbase = "C:/Users/acosd/Documents/grassdata/",
#           location = "demolocation", 
#           mapset = "PERMANENT")