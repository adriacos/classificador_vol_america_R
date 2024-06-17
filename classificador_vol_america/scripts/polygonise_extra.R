library(parallel)
library(supercells)
library(stars)
library(plyr)
library(stringr)
library(terra)
library(exactextractr)
library(raster)
library(parallel)

ini_extra <- function(){
  smoothen_raster_extra()
  vectorise_extra()
  
}

simplify_extra <- function(){
  source("./classificador_vol_america/scripts/clump_vector.R")
  dir.create("./classificador_vol_america/entrenament extra/vector/simplified",showWarnings=F)
  files <- list.files("./classificador_vol_america/entrenament extra/vector",pattern=".gpkg$")
  lapply(files,function(file){
    id <- gsub(".gpkg","",file)
    print(id)
    vect <- st_read(paste("./classificador_vol_america/entrenament extra/vector/",file,sep=""))
    rast <- raster(paste("./classificador_vol_america/entrenament extra/rasters/",id,".tif",sep=""))
    vect <- add_colums_clump(vect)
    resolution <- prod(res(rast))
    save(resolution,file="./classificador_vol_america/temp/resolution.RData")
    minres <- 500
    save(minres,file="./classificador_vol_america/temp/minres.RData")
    
    vect$quad_ori_1km <- 1
    vect$quad_id_1km <- 1
    vect <- as.data.table(vect)
    
    dir.create("./classificador_vol_america/vect/temp/",showWarnings=F)
    dir.create("./classificador_vol_america/vect/temp/clumped/",showWarnings=F)
    dir.create(paste("./classificador_vol_america/vect/temp/clumped/",id,sep=""),showWarnings=F)
    clump_vector_simplify(vect,rast,quad_id=id,file_id=id,km=1,log=T,prepare=T,parallelextract=T)
    vect <- st_as_sf(readRDS(paste("./classificador_vol_america/vect/temp/clumped/",id,"/",id,".rds",sep="")))
  
    unlink(paste("./classificador_vol_america/vect/temp/clumped/",id,"/",id,".rds",sep=""))
    unlink(paste("./classificador_vol_america/entrenament extra/vector/simplified/",id,".gpkg",sep=""))
    st_write(vect,paste("./classificador_vol_america/entrenament extra/vector/simplified/",id,".gpkg",sep=""))
    return(T)
  })
  return(T)
}

clump_extra <- function(){
  source("./classificador_vol_america/scripts/clump_vector.R")
  dir.create("./classificador_vol_america/entrenament extra/vector/clumped",showWarnings=F)
  files <- list.files("./classificador_vol_america/entrenament extra/vector/simplified",pattern=".gpkg$")
  lapply(files,function(file){
    id <- gsub(".gpkg","",file)
    print(id)
    vect <- st_read(paste("./classificador_vol_america/entrenament extra/vector/simplified/",file,sep=""))
    rast <- raster(paste("./classificador_vol_america/entrenament extra/rasters/",id,".tif",sep=""))
    vect <- add_colums_clump(vect)
    # resolution <- prod(res(rast))
    load("./classificador_vol_america/temp/resolution.RData")
    # minres <- 500
    load("./classificador_vol_america/temp/minres.RData")
    
    vect$quad_ori_1km <- 1
    vect$quad_id_1km <- 1
    
    dir.create("./classificador_vol_america/vect/temp/",showWarnings=F)
    dir.create("./classificador_vol_america/vect/temp/clumped/",showWarnings=F)
    dir.create(paste("./classificador_vol_america/vect/temp/clumped/",id,sep=""),showWarnings=F)
    # clump_vector_simplify(vect,rast,quad_id=id,file_id=id,km=1,log=T,prepare=T,parallelextract=T)
    
    clump_vector(vect,rast,quad_id=id,file_id=id,
                             arealimit=NULL,method="closest",
                             log=T,prepare=T,km=1,
                             parallelextract=T,forceddnextract=T)
      
    vect <- st_as_sf(readRDS(paste("./classificador_vol_america/vect/temp/clumped/",id,"/",id,".rds",sep="")))
    
    unlink(paste("./classificador_vol_america/entrenament extra/vector/simplified/",id,".gpkg",sep=""))
    st_write(vect,paste("./classificador_vol_america/entrenament extra/vector/simplified/",id,".gpkg",sep=""))
    return(T)
  })
  return(T)
}

smoothen_raster_extra <- function(){
  source("./classificador_vol_america/scripts/smoothen_raster_extra.R")
  
  files <- list.files("./classificador_vol_america/entrenament extra/rasters",pattern=".tif$")
  res <- lapply(files, function(f){
    smoothen_raster_extra_do(f,1)
  })
  
  files <- list.files("./classificador_vol_america/entrenament extra/rasters",pattern="_s1.tif$")
  res <- lapply(files, function(f){
    smoothen_raster_extra_do(f,2)
  })
  
}

vectorise_extra <- function(){
  files <- list.files("./classificador_vol_america/entrenament extra/rasters",pattern="_s2.tif$")
  # crs <- st_crs(st_read("./classificador_vol_america/vect/limit.gpkg"))
  polygons <- st_read("./classificador_vol_america/entrenament extra/poligons_entrenament_extra.gpkg")
  lapply(files,function(file,polygons){
    rast <- raster(paste("./classificador_vol_america/entrenament extra/rasters/",
                         file,sep=""))
    id <- as.numeric(sub("1_s2.tif","",file))
    crs <- crs(rast)
    values(rast) <- 2*values(rast)
    unlink(paste("./classificador_vol_america/entrenament extra/vector/",id,".rds",sep=""))
    print(as.numeric(sub("_s2.tif","",file)))
    # rast <- rasters_list[[i]]
    rast <- mask(crop(rast,polygons),polygons)
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
    # vect <- st_intersection(vect,polygons)
    
    
    st_write(vect,paste("./classificador_vol_america/entrenament extra/vector/",id,".gpkg",sep=""))
    # saveRDS(vect,paste("./classificador_vol_america/entrenament extra/vector/",as.numeric(sub("_s2.tif","",file)),".rds",sep=""))
    rm(vect)
    gc()
    TRUE
  },polygons)
  
  
}

smoothen_raster_extra_do <- function(file,n=1){
  source("./classificador_vol_america/scripts/clump_vector.R")
  rast <- raster(paste("./classificador_vol_america/entrenament extra/rasters/",file,sep=""))
  
  id <- sub("_s2","",sub("_s1","",sub(" ","",sub(" ","",sub(".tif","",basename(rast@file@name))))))
  print(id)
  if(length(unique(values(rast)))==1&is.na(unique(values(rast))[1])){
    unlink(paste("./classificador_vol_america/rasters/original/split/",id,".tif",sep=""))
    writeRaster(rast,paste("./classificador_vol_america/entrenament extra/rasters/",id,"_s,",n,".tif",sep=""))
    return(T)
  }
  
  g <- rast
  values(g) <- rep(0,length(values(rast)))
  g <- rast(g)
  g <- as.polygons(g, dissolve=T, trunc=F)
  g <- disagg(g)
  # vect <- as(vect, "Spatial")
  g<- st_as_sf(g)
  
  rast <- rast(rast)
  area <- as.numeric(pi*st_area(g)/4)
  rm(g)
  if(ceiling(9*area/500)<3){
    writeRaster(rast,paste("./classificador_vol_america/entrenament extra/rasters/",id,"_s,",n,".tif",sep=""))
    return(T)
  }
  if(n==1){
    rast_sc = supercells(rast, k = ceiling(9*area/500), compactness = 0.5)
  }else{
    rast_sc = supercells(rast, k = ceiling(9/((n-1)*9)*area/500), compactness = 0.5)
  }
  rm(area)
  ex <- exact_extract(rast, rast_sc, "mean", progress=F)
  rast_sc$value <- ex
  rm(ex)
  # pretty=T
 
   # tryCatch(
   #      {
          # print("pretty=T")
          rast_sc <- st_rasterize(rast_sc[,"value"],st_as_stars(pretty=T,st_bbox(rast_sc),nx=nrow(rast),ny=ncol(rast)))
        # },
        # error=function() {
          # print("pretty=F")
          # rast_sc <- st_rasterize(rast_sc[,"value"],st_as_stars(pretty=F,st_bbox(rast_sc),nx=nrow(rast),ny=ncol(rast)))
        # }
      # )
  rast_sc <- rast(rast_sc)
  rast_sc <- raster(rast_sc)
  rast_sc <- resample(rast_sc,raster(rast))
  values(rast_sc) <- round_any(values(rast_sc), 0.5)
  writeRaster(rast_sc,paste("./classificador_vol_america/entrenament extra/rasters/",id,"_s",n,".tif",sep=""))
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