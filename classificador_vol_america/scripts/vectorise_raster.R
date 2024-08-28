library(terra)
library(raster)
# library(rgdal)
library(plyr)
source("./classificador_vol_america/scripts/clump_vector.R")

vectorise_raster_ini <- function(){
  # stop("vectorise_ini")
  # print("vectorise_raster_ini")
  print(paste(Sys.time()," vectorise_raster_ini"))
  if(!file.exists("./classificador_vol_america/temp/vectorise_raster_current.RData")){
    vectorise_raster_current <- "ini"
    save(vectorise_raster_current,file="./classificador_vol_america/temp/vectorise_raster_current.RData")
    vectorise_raster_ini()
  }else{
    load("./classificador_vol_america/temp/vectorise_raster_current.RData")
    if(vectorise_raster_current=="ini"){
      vectorise_raster_set()
    }else if(vectorise_raster_current=="set"){
      library(rstudioapi)
      restartSession(command=source("./classificador_vol_america/scripts/vectorise_raster_do.R"))
    }else if(vectorise_raster_current=="vectorised"){
      library(rstudioapi)
      # stop("tosimplify")
      restartSession(command=source("./classificador_vol_america/scripts/vectorise_raster_simplify.R"))
    }else if(vectorise_raster_current=="simplified"){
    #   library(rstudioapi)
    #   restartSession(command=source("./classificador_vol_america/scripts/vectorise_rasters_merge.R"))
    # }else if(vectorise_raster_current=="merged"){
    #   library(rstudioapi)
    #   restartSession(command=source("./classificador_vol_america/scripts/vectorise_raster_merge_secondstep.R"))
    # }else if(vectorise_raster_current=="merged_secondstep"){
      current_global <- "vectorised"
      save(current_global,file="./classificador_vol_america/temp/current_global.RData")
      unlink("./classificador_vol_america/temp/vectorise_raster_current.RData")
      restartSession(command=source("./classificador_vol_america/scripts/continue.R"))
      # continue()
    }
  }
}

vectorise_raster_set <- function(rast){
  # print("vectorise_raster_set")
  print(paste(Sys.time()," vectorise_raster_set"))
  rast <- raster("./classificador_vol_america/rasters/smoothen2/merged.tif")
  limit <- st_read("./classificador_vol_america/vect/limit.gpkg")
  # area <- as.numeric(st_area(limit))
  #TODO: save as "vectorise_raster_grid
  load("./classificador_vol_america/temp/resolution.RData")
  source("./classificador_vol_america/scripts/clump_vector.R")
  load("./classificador_vol_america/temp/smoothen_raster_2_grid.RData")
  
  # grid <- create_grid(limit,round(sqrt(area)/(resolution*2000)),round(sqrt(area)/(resolution*2000)))
  # grid <- create_grid(limit,round(sqrt(area)/(sqrt(resolution)*2000)),round(sqrt(area)/(sqrt(resolution)*2000)))
  
  dir.create("./classificador_vol_america/rasters/smoothen2/split", showWarnings = FALSE)
  # ids_done <- sapply(list.files("./classificador_vol_america/vect/vectorised/split/ori",pattern=".rds$"),function(x){
  # # ids_done <- sapply(list.files("./classificador_vol_america/rasters/smoothen2/split",pattern=".tif$"),function(x){
  #   # as.numeric(sub(".tif","",x))
  #   as.numeric(sub(".rds","",x))
  # })

  # ids_done <- sapply(list.files("./classificador_vol_america/vect/vectorised/split/ori",pattern=".rds$"),function(x){
  #   as.numeric(sub(".rds","",x))
  # })
  
  
  
  library(parallel)
  
  n.cores <- detectCores()
  free.mem <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024
  if(ceiling(sqrt(free.mem)/(252/6))<=n.cores){
    n.cores<-ceiling(sqrt(free.mem)/(252/6))
  }
  
  sapply(list.files("./classificador_vol_america/rasters/smoothen2/split",pattern=".tif$")[order(sapply(list.files("./classificador_vol_america/rasters/smoothen2/split",pattern=".tif$"),function(x){
    file.info(paste("./classificador_vol_america/rasters/smoothen2/split/",x,sep=""))$mtime
  }),decreasing=T)][1:n.cores],function(x){
    unlink(paste("./classificador_vol_america/rasters/smoothen2/split/",x,sep=""))
  })
  sapply(list.files("./classificador_vol_america/rasters/smoothen2/split",pattern=".tif$"),function(x){
    if(file.info(paste("./classificador_vol_america/rasters/smoothen2/split/",x,sep=""))$size==0){
      unlink(paste("./classificador_vol_america/rasters/smoothen2/split/",x,sep=""))
    }
  })
  
  ids_done <- sapply(list.files("./classificador_vol_america/rasters/smoothen2/split",pattern=".tif$"),function(x){
    # as.numeric(sub(".tif","",x))
    as.numeric(sub(".tif","",x))
  })
  smoothen_raster_2_grid <- smoothen_raster_2_grid[!smoothen_raster_2_grid$id%in%ids_done,]
  
  print(paste(nrow(smoothen_raster_2_grid)," grid elements to cut from smoothened raster",sep=""))
  
  unlink("./classificador_vol_america/logs/vectorise_raster_set.txt")
  cl <- create_cluster_clump_(n.cores,"vectorise_raster_set")
  parLapplyLB(cl,smoothen_raster_2_grid$id,function(id, grid, rast){
  # lapply(smoothen_raster_grid$id,function(id, grid, rast){
    unlink(paste("./classificador_vol_america/rasters/smoothen2/split/",id,".tif",sep=""))
    gd <- smoothen_raster_2_grid[smoothen_raster_2_grid$id==id,]
    print(gd$id)
    # buf <- st_buffer(smoothen_raster_grid[smoothen_raster_grid$id==id,], 10)
    crop <- crop(rast,gd)
    mask <- mask(crop,gd)
    unlink(crop@file@name)
    unlink(sub(".grd",".gri",crop@file@name))
    rm(crop)
    writeRaster(mask,paste("./classificador_vol_america/rasters/smoothen2/split/",id,".tif",sep=""))
    # crop
    unlink(mask@file@name)
    unlink(sub(".grd",".gri",mask@file@name))
    rm(mask)
  },smoothen_raster_2_grid,rast)
  stopCluster(cl)
  rm(cl)
  unlink("./classificador_vol_america/logs/vectorise_raster_set.txt")
  # rm(limit)
  # rm(smoothen_raster_grid)
  rm(rast)
  gc()
  
  ids_done <- sapply(list.files("./classificador_vol_america/rasters/smoothen2/split",pattern=".tif$"),function(x){
    as.numeric(sub(".tif","",x))
  })
  smoothen_raster_2_grid <- smoothen_raster_2_grid[!smoothen_raster_2_grid$id%in%ids_done,]
  if(nrow(smoothen_raster_2_grid)>0){
    vectorise_raster_ini()
  }
  rm(smoothen_raster_2_grid)
  rm(ids_done)
  vectorise_raster_current <- "set"
  save(vectorise_raster_current,file="./classificador_vol_america/temp/vectorise_raster_current.RData")
  vectorise_raster_ini()
}

vectorise_raster_do_ <- function(file,
                                 crs=NULL,
                                 savedir="./classificador_vol_america/vect/vectorised/split/ori/",
                                 resolution=NULL){
  rast <- raster(paste("./classificador_vol_america/rasters/smoothen2/split/",
                       file,sep=""))
  values(rast) <- 2*values(rast)
  unlink(paste(savedir,as.numeric(sub(".tif","",file)),".rds",sep=""))
  print(as.numeric(sub(".tif","",file)))
  # rast <- rasters_list[[i]]
  rast <- rast(rast)
  # vect <- as.polygons(rast, dissolve=T, trunc=F)
  vect <- as.polygons(rast)
  rm(rast)
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
  #   load("./classificador_vol_america/temp/resolution.RData")
  # }
  # if(min(as.numeric(st_area(vect)))>=4*resolution){
  #   saveRDS(vect,paste(sub("/ori/","/simplified/",savedir),
  #                      file,
  #                      sep=""))
  # }
  saveRDS(vect,paste(savedir,as.numeric(sub(".tif","",file)),".rds",sep=""))
  unlink(paste("./classificador_vol_america/rasters/smoothen2/split/",
               file,sep=""))
  rm(vect)
  gc()
}
