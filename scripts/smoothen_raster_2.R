
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

# source("./scripts/clump_vector.R")

smoothen_raster_2_ini <- function(){
  # print("smoothen_raster_2_ini")
  print(paste(Sys.time()," smoothen_raster_2_ini"))
  if(!file.exists("./temp/smoothen_raster_2_current.RData")){
    smoothen_raster_2_current <- "ini"
    save(smoothen_raster_2_current,file="./temp/smoothen_raster_2_current.RData")
    rm(smoothen_raster_2_current)
    smoothen_raster_2_ini()
  }else{
    load("./temp/smoothen_raster_2_current.RData")
    if(smoothen_raster_2_current=="ini"){
      smoothen_raster_2_set()
    }
    if(smoothen_raster_2_current=="set"){
      library(rstudioapi)
      restartSession(command=source("./scripts/smoothen_raster_2_supercells_do.R"))
    }else if(smoothen_raster_2_current=="smoothen"){
      library(rstudioapi)
      restartSession(command=source("./scripts/merge_rasters_mosaic_2.R"))
    }else if(smoothen_raster_2_current=="merged"){
      # unlink("./temp/smoothen_raster_2_grid.RData")
      current_global <- "smoothen2"
      save(current_global,file="./temp/current_global.RData")
      unlink("./temp/smoothen_raster_2_current.RData")
      library(rstudioapi)
      restartSession(command=source("./scripts/continue.R"))
      # continue()
    }
  }
  
}

smoothen_raster_2_set <- function(){
  # print("smoothen_raster_2_set")
  print(paste(Sys.time()," smoothen_raster_2_set"))
  source("./scripts/clump_vector.R")
  rast <- raster("./rasters/smoothen/merged.tif")
  # limit <- st_read("./vect/grids/limit.gpkg")
  if(file.exists("./temp/smoothen_raster_2_grid.RData")){
    load("./temp/smoothen_raster_2_grid.RData")
  }else{
    max_val <- 10
    # save(max_val,file="./temp/max_val.RData")
    load("./temp/resolution.RData")
    limit <- st_read("./vect/limit.gpkg")
    area <- as.numeric(st_area(limit))
    library(parallel)
    factor <- sqrt(detectCores()/4)*sqrt(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024/27000)
    smoothen_raster_2_grid <- create_grid(limit,round(sqrt(area)/(factor*resolution*3000)),round(sqrt(area)/(factor*resolution*3000)))
    save(smoothen_raster_2_grid,file="./temp/smoothen_raster_2_grid.RData")
    rm(area)
    rm(limit)
    rm(resolution)
    unlink("./rasters/smoothen/split",recursive=T)
    unlink("./vect/smoothen_raster_2_grid",recursive=T)
    dir.create("./vect/smoothen_raster_2_grid",showWarnings=F)
    lapply(smoothen_raster_2_grid$id,function(id, smoothen_raster_2_grid){
      # if(!file.exists(paste("./rasters/smoothen/split/",id,".tif",sep=""))&
      #    !file.exists(paste("./vect/smoothen_raster_2_grid/",id,".gpkg",sep=""))){
      #   buf <- st_buffer(smoothen_raster_2_grid[smoothen_raster_2_grid$id==id,], 10)
      #   st_write(buf,paste("./vect/smoothen_raster_2_grid/",id,".gpkg",sep=""))
      #   return(T)  
      # }
      # return(T)
      buf <- st_buffer(smoothen_raster_2_grid[smoothen_raster_2_grid$id==id,], 10)
      st_write(buf,paste("./vect/smoothen_raster_2_grid/",id,".gpkg",sep=""))
      return(T)
    },smoothen_raster_2_grid)
    gc()
  }
  dir.create("./rasters/smoothen",showWarnings=F)
  dir.create("./rasters/smoothen/split",showWarnings=F)
  # ids_done <- sapply(list.files("./rasters/smoothen2/split",pattern=".tif$"),function(x){
  #   as.numeric(sub(".tif","",x))
  # })
  # ids_done <- ids_done[-which.max(sapply(list.files("./rasters/smoothen2/split",pattern=".tif$",full.names=T),function(x){
  #   file.info(x)$mtime
  # }))]
  # smoothen_raster_2_grid <- smoothen_raster_2_grid[!smoothen_raster_2_grid$id%in%ids_done,]
  ids <- sapply(list.files("./vect/smoothen_raster_2_grid"),function(f){
    gsub(".gpkg","",f)
  })
  ids <- ids[order(as.numeric(ids))]
  print(paste("Smoothen 2 set - grid of ",length(ids), " elements to cut from raster",sep=""))
  n.cores <- detectCores()
  # n.cores <- 18
  unlink("./logs/smoothen_raster_2_set.txt")
  unlink("./temp/split/",recursive=T)
  dir.create("./temp/split/")
  rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024*1.8/n.cores
  cl <- create_cluster_clump_(n.cores,"smoothen_raster_2_set")
  parLapplyLB(cl,ids,function(id, rast,rasterinmemory){
    print(id)
    # unlink(paste("./rasters/original/split/",id,".tif",sep=""))
    rasterOptions(maxmemory=rasterinmemory)
    unlink(paste("./rasters/smoothen/split/",id,".tif",sep=""))
    unlink(paste("./temp/split/",id,".tif",sep=""))
    buf <- st_read(paste("./vect/smoothen_raster_2_grid/",id,".gpkg",sep=""))
    # buf <- st_buffer(smoothen_raster_2_grid[smoothen_raster_2_grid$id==id,], 10)
    crop <- crop(rast,buf,filename=paste("./temp/split/",id,".tif",sep=""))
    mask <- mask(crop,buf,filename=paste("./rasters/smoothen/split/",id,".tif",sep=""))
    unlink(crop@file@name)
    # crop <- mask(crop(rast,buf),buf)
    # writeRaster(crop,paste("./rasters/smoothen/split/",id,".tif",sep=""))
    rm(crop)
    rm(mask)
    gc()
    unlink(paste("./vect/smoothen_raster_2_grid/",id,".gpkg",sep=""))
    # crop
  },rast,rasterinmemory)
  stopCluster(cl)
  rm(cl)
  gc()
  unlink("./logs/smoothen_raster_2_set.txt")
  ids_done <- sapply(list.files("./rasters/smoothen/split",pattern=".tif$"),function(x){
    as.numeric(sub(".tif","",x))
  })
  smoothen_raster_2_grid <- smoothen_raster_2_grid[!smoothen_raster_2_grid$id%in%ids_done,]
  if(nrow(smoothen_raster_2_grid)>0){
    restartSession(command=source("./scripts/smoothen_raster_2.R"))
  }
  unlink("./vect/smoothen_raster_2_grid",recursive=T)
  smoothen_raster_2_current <- "set"
  save(smoothen_raster_2_current,file="./temp/smoothen_raster_2_current.RData")
  smoothen_raster_2_ini()
}


# smoothen_raster_2_supercells_do <- function(max_val,smoothen_raster_2_grids){
#   load("./temp/max_val.RData")
#   load("./temp/smoothen_raster_2_grids.RData")
#   raster_split <- lapply(paste("./rasters/pnoa/split/",list.files("./rasters/pnoa/split/",pattern="\\.tif$"),sep=""),raster)
#   names(raster_split) <- lapply(raster_split,function(r){
#     sub(" ","",sub(" ","",sub(".tif","",basename(r@file@name))))
#   })
#   ids_done <- sub(".tif","",list.files("./rasters/pnoa/split/supercells",pattern="\\.tif$"))
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
#       restartSession(command=source("./scripts/smoothen_raster_2_supercells_do.R"))
#     }
#   )
#   merge_rasters_mosaic()
# } 


smoothen_raster_2_supercells_ <- function(rast,max_val,smoothen_raster_2_grid,pretty=T){
  id <- sub(" ","",sub(" ","",sub(".tif","",basename(rast@file@name))))
  print(paste("somethen raster 2 id:",id," - ",Sys.time(),sep=""))
  unlink(paste("./rasters/smoothen2/split/",id,".tif",sep=""))
  # print(Sys.time())
  if(length(unique(values(rast)))==1&is.na(unique(values(rast))[1])){
    unlink(paste("./rasters/smoothen/split/",id,".tif",sep=""))
    return(T)
  }
  g <- smoothen_raster_2_grid[smoothen_raster_2_grid$id==id,]
  
  rast <- rast(rast)
  area <- as.numeric(st_area(g))
  rm(g)
  if(ceiling(area/500)<3){
    writeRaster(raster(rast),paste("./rasters/smoothen2/split/",id,".tif",sep=""))
    unlink(paste("./rasters/smoothen/split/",id,".tif",sep=""))
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
  writeRaster(rast_sc,paste("./rasters/smoothen2/split/",id,".tif",sep=""))
  unlink(paste("./rasters/smoothen/split/",id,".tif",sep=""))
  gc()
  # rast_sc
  return(TRUE)
}

smoothen_failed_2_ <- function(){
  files <- list.files("./rasters/smoothen2/split/",pattern="\\.tif$")
  if(length(files>0)){
    raster_split <- lapply(paste("./rasters/smoothen2/split/",files,sep=""),raster)
    names(raster_split) <- lapply(raster_split,function(r){
      sub(" ","",sub(" ","",sub(".tif","",basename(r@file@name))))
    })
    smoothen_raster_2_do_ids_failed <- names(raster_split)
    rm(smoothen_raster_2_do_ids_failed)
    n.cores <- detectCores()
    unlink("./logs/smoothen_raster_2_supercells_failed_.txt")
    clust <- create_cluster_clump_(n.cores,"smoothen_raster_2_supercells_failed_")
    parLapplyLB(clust,raster_split,smoothen_raster_2_supercells_,max_val,smoothen_raster_2_grid,pretty=F)
    stopCluster(clust)
    rm(raster_split)
    rm(clust)
  }
}


