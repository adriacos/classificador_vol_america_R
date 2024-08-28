
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

smoothen_raster_2_ini <- function(){
  # print("smoothen_raster_2_ini")
  print(paste(Sys.time()," smoothen_raster_2_ini"))
  if(!file.exists("./classificador_vol_america/temp/smoothen_raster_2_current.RData")){
    smoothen_raster_2_current <- "ini"
    save(smoothen_raster_2_current,file="./classificador_vol_america/temp/smoothen_raster_2_current.RData")
    rm(smoothen_raster_2_current)
    smoothen_raster_2_ini()
  }else{
    load("./classificador_vol_america/temp/smoothen_raster_2_current.RData")
    if(smoothen_raster_2_current=="ini"){
      smoothen_raster_2_set()
    }
    if(smoothen_raster_2_current=="set"){
      library(rstudioapi)
      restartSession(command=source("./classificador_vol_america/scripts/smoothen_raster_2_supercells_do.R"))
    }else if(smoothen_raster_2_current=="smoothen"){
      library(rstudioapi)
      restartSession(command=source("./classificador_vol_america/scripts/merge_rasters_mosaic_2.R"))
    }else if(smoothen_raster_2_current=="merged"){
      # unlink("./classificador_vol_america/temp/smoothen_raster_2_grid.RData")
      current_global <- "smoothen2"
      save(current_global,file="./classificador_vol_america/temp/current_global.RData")
      unlink("./classificador_vol_america/temp/smoothen_raster_2_current.RData")
      library(rstudioapi)
      restartSession(command=source("./classificador_vol_america/scripts/continue.R"))
      # continue()
    }
  }
  
}

smoothen_raster_2_set <- function(){
  # print("smoothen_raster_2_set")
  print(paste(Sys.time()," smoothen_raster_2_set"))
  source("./classificador_vol_america/scripts/clump_vector.R")
  rast <- raster("./classificador_vol_america/rasters/smoothen/merged.tif")
  # limit <- st_read("./classificador_vol_america/vect/grids/limit.gpkg")
  if(file.exists("./classificador_vol_america/temp/smoothen_raster_2_grid.RData")){
    load("./classificador_vol_america/temp/smoothen_raster_2_grid.RData")
  }else{
    max_val <- 10
    save(max_val,file="./classificador_vol_america/temp/max_val.RData")
    load("./classificador_vol_america/temp/resolution.RData")
    limit <- st_read("./classificador_vol_america/vect/limit.gpkg")
    area <- as.numeric(st_area(limit))
    library(parallel)
    # factor <- sqrt(detectCores()/4)*sqrt(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024/12000)
    factor <- sqrt(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024/9000)/(sqrt(detectCores())/3.2)
    smoothen_raster_2_grid <- create_grid(limit,round(sqrt(area)/(factor*resolution*9000)),round(sqrt(area)/(factor*resolution*9000)))
    save(smoothen_raster_2_grid,file="./classificador_vol_america/temp/smoothen_raster_2_grid.RData")
    rm(area)
    rm(limit)
    rm(resolution)
  }
  dir.create("./classificador_vol_america/rasters/smoothen/split",showWarnings=F)
  # dir.create("./classificador_vol_america/rasters/original/split",showWarnings=F)
  n.cores <- detectCores()
  free.mem <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024
  if(ceiling(sqrt(free.mem)/(252/7))<=n.cores){
    n.cores<-ceiling(sqrt(free.mem)/(252/7))
  }
  
  # n.cores <- detectCores()
  sapply(list.files("./classificador_vol_america/rasters/smoothen/split",pattern=".tif$")[order(sapply(list.files("./classificador_vol_america/rasters/smoothen/split",pattern=".tif$"),function(x){
    file.info(paste("./classificador_vol_america/rasters/smoothen/split/",x,sep=""))$mtime
  }),decreasing=T)][1:n.cores],function(x){
    unlink(paste("./classificador_vol_america/rasters/smoothen/split/",x,sep=""))
  })
  sapply(list.files("./classificador_vol_america/rasters/smoothen/split",pattern=".tif$"),function(x){
    if(file.info(paste("./classificador_vol_america/rasters/smoothen/split/",x,sep=""))$size==0){
      unlink(paste("./classificador_vol_america/rasters/smoothen/split/",x,sep=""))
    }
  })
  
  ids_done <- sapply(list.files("./classificador_vol_america/rasters/smoothen/split",pattern=".tif$"),function(x){
    as.numeric(sub(".tif","",x))
  })
  # ids_done <- ids_done[-which.max(sapply(list.files("./classificador_vol_america/rasters/original/split",pattern=".tif$",full.names=T),function(x){
  #   file.info(x)$mtime
  # }))]
  smoothen_raster_2_grid <- smoothen_raster_2_grid[!smoothen_raster_2_grid$id%in%ids_done,]
  print(paste("Smoothen set - grid of ",nrow(smoothen_raster_2_grid), " elements to cut from raster",sep=""))
  # n.cores <- detectCores()
  cl <- create_cluster_clump_(n.cores,"smoothen_raster_2_set")
  parLapplyLB(cl,smoothen_raster_2_grid$id,function(id, smoothen_raster_2_grid, rast){
    print(smoothen_raster_2_grid[smoothen_raster_2_grid$id==id,]$id)
    buf <- st_buffer(smoothen_raster_2_grid[smoothen_raster_2_grid$id==id,], 10)
    crop <- mask(crop(rast,buf),buf)
    unlink(paste("./classificador_vol_america/rasters/smoothen/split/",id,".tif",sep=""))
    writeRaster(crop,paste("./classificador_vol_america/rasters/smoothen/split/",id,".tif",sep=""))
    # crop
  },smoothen_raster_2_grid,rast)
  stopCluster(cl)
  rm(cl)
  unlink("./classificador_vol_america/logs/smoothen_raster_2_set.txt")
  ids_done <- sapply(list.files("./classificador_vol_america/rasters/smoothen/split",pattern=".tif$"),function(x){
    as.numeric(sub(".tif","",x))
  })
  smoothen_raster_2_grid <- smoothen_raster_2_grid[!smoothen_raster_2_grid$id%in%ids_done,]
  if(nrow(smoothen_raster_2_grid)>0){
    restartSession(command=source("./classificador_vol_america/scripts/smoothen_raster_2.R"))
  }
  smoothen_raster_2_current <- "set"
  save(smoothen_raster_2_current,file="./classificador_vol_america/temp/smoothen_raster_2_current.RData")
  smoothen_raster_2_ini()
}


# smoothen_raster_2_supercells_do <- function(max_val,smoothen_raster_2_grids){
#   load("./classificador_vol_america/temp/max_val.RData")
#   load("./classificador_vol_america/temp/smoothen_raster_2_grids.RData")
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
#       raster_split <- parLapplyLB(clust, raster_split,smoothen_raster_2_supercells_,max_val)
#       stopCluster(clust)
#     },
#     error=function() {
#       print("error")
#       stopCluster(cl)
#       rm(cl)
#       rm(raster_split)
#       gc()
#       smoothen_raster_2_supercells_do(raster_split,max_val,smoothen_raster_2_grids)
#       restartSession(command=source("./classificador_vol_america/scripts/smoothen_raster_2_supercells_do.R"))
#     }
#   )
#   merge_rasters_mosaic()
# } 


smoothen_raster_2_supercells_ <- function(rast,max_val,smoothen_raster_2_grid,pretty=T){
  id <- sub(" ","",sub(" ","",sub(".tif","",basename(rast@file@name))))
  print(paste("somethen raster 2 id:",id," - ",Sys.time(),sep=""))
  unlink(paste("./classificador_vol_america/rasters/smoothen2/split/",id,".tif",sep=""))
  # print(Sys.time())
  if(length(unique(values(rast)))==1&is.na(unique(values(rast))[1])){
    unlink(paste("./classificador_vol_america/rasters/smoothen/split/",id,".tif",sep=""))
    return(T)
  }
  g <- smoothen_raster_2_grid[smoothen_raster_2_grid$id==id,]
  
  rast <- rast(rast)
  area <- as.numeric(st_area(g))
  rm(g)
  if(ceiling(area/500)<3){
    writeRaster(raster(rast),paste("./classificador_vol_america/rasters/smoothen2/split/",id,".tif",sep=""))
    unlink(paste("./classificador_vol_america/rasters/smoothen/split/",id,".tif",sep=""))
    return(T)
  }
  rast_sc = supercells(rast, k = ceiling(area/500), compactness = 0.5)
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
  writeRaster(rast_sc,paste("./classificador_vol_america/rasters/smoothen2/split/",id,".tif",sep=""))
  unlink(paste("./classificador_vol_america/rasters/smoothen/split/",id,".tif",sep=""))
  # rast_sc
  return(TRUE)
}

smoothen_failed_2_ <- function(){
  files <- list.files("./classificador_vol_america/rasters/smoothen/split/",pattern="\\.tif$")
  if(length(files>0)){
    raster_split <- lapply(paste("./classificador_vol_america/rasters/smoothen/split/",files,sep=""),raster)
    names(raster_split) <- lapply(raster_split,function(r){
      sub(" ","",sub(" ","",sub(".tif","",basename(r@file@name))))
    })
    smoothen_raster_2_do_ids_failed <- names(raster_split)
    rm(smoothen_raster_2_do_ids_failed)
    n.cores <- detectCores()
    unlink("./classificador_vol_america/logs/smoothen_raster_2_supercells_failed_.txt")
    clust <- create_cluster_clump_(n.cores,"smoothen_raster_2_supercells_failed_")
    parLapplyLB(clust,raster_split,smoothen_raster_2_supercells_,max_val,smoothen_raster_2_grid,pretty=F)
    stopCluster(clust)
    rm(raster_split)
    rm(clust)
  }
}


