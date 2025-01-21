library(terra)
library(raster)
# library(rgdal)
library(plyr)
source("./scripts/clump_vector.R")

vectorise_raster_ini <- function(){
  # stop("vectorise_ini")
  # print("vectorise_raster_ini")
  print(paste(Sys.time()," vectorise_raster_ini"))
  if(!file.exists("./temp/vectorise_raster_current.RData")){
    vectorise_raster_current <- "ini"
    save(vectorise_raster_current,file="./temp/vectorise_raster_current.RData")
    vectorise_raster_ini()
  }else{
    load("./temp/vectorise_raster_current.RData")
    if(vectorise_raster_current=="ini"){
      vectorise_raster_set()
    }else if(vectorise_raster_current=="set"){
      library(rstudioapi)
      restartSession(command=source("./scripts/vectorise_raster_do.R"))
    }else if(vectorise_raster_current=="vectorised"){
      library(rstudioapi)
      # stop("tosimplify")
      restartSession(command=source("./scripts/vectorise_raster_simplify.R"))
    }else if(vectorise_raster_current=="simplified"){
    #   library(rstudioapi)
    #   restartSession(command=source("./scripts/vectorise_rasters_merge.R"))
    # }else if(vectorise_raster_current=="merged"){
    #   library(rstudioapi)
    #   restartSession(command=source("./scripts/vectorise_raster_merge_secondstep.R"))
    # }else if(vectorise_raster_current=="merged_secondstep"){
      current_global <- "vectorised"
      save(current_global,file="./temp/current_global.RData")
      # stop()
      unlink("./temp/vectorise_raster_current.RData")
      restartSession(command=source("./scripts/continue.R"))
      # continue()
    }
  }
}

vectorise_raster_set <- function(rast){
  # print("vectorise_raster_set")
  print(paste(Sys.time()," vectorise_raster_set"))
  rast <- raster("./rasters/smoothen2/merged.tif")
  limit <- st_read("./vect/limit.gpkg")
  # area <- as.numeric(st_area(limit))
  #TODO: save as "vectorise_raster_grid
  load("./temp/resolution.RData")
  source("./scripts/clump_vector.R")
  # load("./temp/smoothen_raster_2_grid.RData")
  
  if(file.exists("./temp/vectorise_raster_grid.RData")){
    load("./temp/vectorise_raster_grid.RData")
  }else{
    # max_val <- 255
    # save(max_val,file="./temp/max_val.RData")
    load("./temp/max_val.RData")
    load("./temp/resolution.RData")
    limit <- st_read("./vect/limit.gpkg")
    area <- as.numeric(st_area(limit))
    library(parallel)
    factor <- sqrt(detectCores()/4)*sqrt(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024/28000)
    vectorise_raster_grid <- create_grid(limit,round(sqrt(area)/(factor*resolution*1800)),round(sqrt(area)/(factor*resolution*1800)))
    save(vectorise_raster_grid,file="./temp/vectorise_raster_grid.RData")
    rm(area)
    rm(limit)
    rm(resolution)
    unlink("./rasters/original/split",recursive=T)
    unlink("./vect/vectorise_raster_grid",recursive=T)
    dir.create("./vect/vectorise_raster_grid",showWarnings=F)
    lapply(vectorise_raster_grid$id,function(id, vectorise_raster_grid){
      # if(!file.exists(paste("./rasters/smoothen2/split/",id,".tif",sep=""))&
      #    !file.exists(paste("./vect/smoothen_raster_2_grid/",id,".gpkg",sep=""))){
      #   buf <- st_buffer(smoothen_raster_2_grid[smoothen_raster_2_grid$id==id,], 10)
      #   st_write(buf,paste("./vect/smoothen_raster_2_grid/",id,".gpkg",sep=""))
      #   return(T)  
      # }
      # return(T)
      buf <- st_buffer(vectorise_raster_grid[vectorise_raster_grid$id==id,], 10)
      st_write(buf,paste("./vect/vectorise_raster_grid/",id,".gpkg",sep=""))
      return(T)
    },vectorise_raster_grid)
    gc()
  }
  
  
  # if(!dir.exists("./vect/vectorise_raster_grid")){
  #   unlink("./rasters/smoothen2/split",recursive=T)
  #   unlink("./vect/vectorise_raster_grid",recursive=T)
  #   dir.create("./vect/vectorise_raster_grid",showWarnings=F)
  #   lapply(vectorise_raster_grid$id,function(id, vectorise_raster_grid){
  #     r <- smoothen_raster_2_grid[vectorise_raster_grid$id==id,]
  #     st_write(r,paste("./vect/smoothen_raster_2_grid/",id,".gpkg",sep=""))
  #     return(T)
  #   },smoothen_raster_2_grid)
  #   gc()
  # }
  
  # grid <- create_grid(limit,round(sqrt(area)/(resolution*2000)),round(sqrt(area)/(resolution*2000)))
  # grid <- create_grid(limit,round(sqrt(area)/(sqrt(resolution)*2000)),round(sqrt(area)/(sqrt(resolution)*2000)))
  
  dir.create("./rasters/smoothen2/split", showWarnings = FALSE)
  # ids_done <- sapply(list.files("./vect/vectorised/split/ori",pattern=".rds$"),function(x){
  # # ids_done <- sapply(list.files("./rasters/smoothen2/split",pattern=".tif$"),function(x){
  #   # as.numeric(sub(".tif","",x))
  #   as.numeric(sub(".rds","",x))
  # })

  # ids_done <- sapply(list.files("./vect/vectorised/split/ori",pattern=".rds$"),function(x){
  #   as.numeric(sub(".rds","",x))
  # })
  
  # vectorise_raster_grid <- vectorise_raster_grid[!vectorise_raster_grid$id%in%ids_done,]

  ids <- sapply(list.files("./vect/vectorise_raster_grid"),function(f){
    gsub(".gpkg","",f)
  })
  ids <- ids[order(as.numeric(ids))]
  print(paste(length(ids)," grid elements to cut from smoothened raster",sep=""))
  
  library(parallel)
  # n.cores <- detectCores()-(detectCores()/4)
  n.cores <- detectCores()
  # n.cores <- 18
  unlink("./logs/vectorise_raster_set.txt")
  unlink("./temp/split/",recursive=T)
  dir.create("./temp/split/")
  # dir.create("./rasters/smoothen2/split/")
  rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024*1.8/n.cores
  cl <- create_cluster_clump_(n.cores,"vectorise_raster_set")
  parLapplyLB(cl,ids,function(id,vectorise_raster_grid,rast,rasterinmemory){
  # lapply(smoothen_raster_grid$id,function(id, grid, rast){
    rasterOptions(maxmemory=rasterinmemory)
    unlink(paste("./rasters/smoothen2/split/",id,".tif",sep=""))
    unlink(paste("./temp/split/",id,".tif",sep=""))
    gc <- st_read(paste("./vect/vectorise_raster_grid/",id,".gpkg",sep=""))
    # buf <- st_buffer(vectorise_raster_grid[vectorise_raster_grid$id==id,], 10)
    crop <- crop(rast,gc,filename=paste("./temp/split/",id,".tif",sep=""))
    mask <- mask(crop,gc,filename=paste("./rasters/smoothen2/split/",id,".tif",sep=""))
    unlink(crop@file@name)
    # crop <- mask(crop(rast,buf),buf)
    # writeRaster(crop,paste("./rasters/smoothen/split/",id,".tif",sep=""))
    rm(crop)
    rm(mask)
    gc()
    unlink(paste("./vect/vectorise_raster_grid/",id,".gpkg",sep=""))
  },vectorise_raster_grid,rast,rasterinmemory)
  stopCluster(cl)
  rm(cl)
  unlink("./logs/vectorise_raster_set.txt")
  gc()
  # rm(limit)
  # rm(smoothen_raster_grid)
  rm(rast)
  gc()
  
  ids_done <- sapply(list.files("./rasters/smoothen2/split",pattern=".tif$"),function(x){
    as.numeric(sub(".tif","",x))
  })
  vectorise_raster_grid <- vectorise_raster_grid[!vectorise_raster_grid$id%in%ids_done,]
  if(nrow(vectorise_raster_grid)>0){
    vectorise_raster_ini()
  }
  
  unlink("./vect/vectorise_raster_grid",recursive=T)
  rm(vectorise_raster_grid)
  rm(ids_done)
  vectorise_raster_current <- "set"
  save(vectorise_raster_current,file="./temp/vectorise_raster_current.RData")
  vectorise_raster_ini()
}

vectorise_raster_do_ <- function(file,
                                 crs=NULL,
                                 savedir="./vect/vectorised/split/ori/",
                                 resolution=NULL){
  rast <- raster(paste("./rasters/smoothen2/split/",
                       file,sep=""))
  values(rast) <- 2*values(rast)
  unlink(paste(savedir,as.numeric(sub(".tif","",file)),".rds",sep=""))
  print(as.numeric(sub(".tif","",file)))
  # rast <- rasters_list[[i]]
  rast <- rast(rast)
  vect <- as.polygons(rast, dissolve=T, trunc=F)
  rm(rast)
  gc()
  vect <- disagg(vect)
  # vect <- as(vect, "Spatial")
  vect <- st_as_sf(vect)
  if(!"layer"%in%colnames(vect)){
    if(paste("X",sub(".tif","",file),sep="")%in%colnames(vect)){
      vect$layer <- as.data.frame(vect)[,paste("X",sub(".tif","",file),sep="")]
      vect[,paste("X",sub(".tif","",file),sep="")] <- NULL
    }else{
      vect$layer <- as.data.frame(vect)[,colnames(vect)[colnames(vect)!="geometry"]]
      vect[,colnames(vect)[colnames(vect)!="geometry"]] <- NULL
    }
  }
  if(!is.null(crs)){
    vect <- st_transform(vect,crs)    
  }
  vect$layer <- vect$layer/2
  # if(is.null(resolution)){
  #   load("./temp/resolution.RData")
  # }
  # if(min(as.numeric(st_area(vect)))>=4*resolution){
  #   saveRDS(vect,paste(sub("/ori/","/simplified/",savedir),
  #                      file,
  #                      sep=""))
  # }
  saveRDS(vect,paste(savedir,as.numeric(sub(".tif","",file)),".rds",sep=""))
  unlink(paste("./rasters/smoothen2/split/",
               file,sep=""))
  rm(vect)
  gc()
  return(T)
}
