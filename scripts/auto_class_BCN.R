library(exactextractr)
library(raster)
library(sf)
library(parallel)


auto_class_BCN <- function(vect){
  dir.create("./classificador_vol_america/vect/class_auto",showWarnings=F)
  # rast <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/diba/CSA56.tif")
  rast <- raster("./maps/diba.tif")
  
  if(file.exists("./classificador_vol_america/vect/class_auto/all_class_90.rds")){
    if(!file.exists("./classificador_vol_america/vect/class_auto/all.rds")){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_class_90.rds")
      saveRDS(vect_dt,"./classificador_vol_america/vect/class_auto/all.rds")
      unlink("./classificador_vol_america/vect/class_auto/all_class_90.rds")
    }else{
      if(file.info("./classificador_vol_america/vect/class_auto/all.rds")$mtime>file.info("./classificador_vol_america/vect/class_auto/all_class_90.rds")$mtime){
        vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_class_90.rds")
        saveRDS(vect_dt,"./classificador_vol_america/vect/class_auto/all.rds")
        unlink("./classificador_vol_america/vect/class_auto/all_class_90.rds")
      }
    }
  }else if(file.exists("./classificador_vol_america/vect/class_auto/all_class_66.rds")){
    if(!file.exists("./classificador_vol_america/vect/class_auto/all.rds")){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_class_66.rds")
      saveRDS(vect_dt,"./classificador_vol_america/vect/class_auto/all.rds")
      unlink("./classificador_vol_america/vect/class_auto/all_class_66.rds")
    }else{
      if(file.info("./classificador_vol_america/vect/class_auto/all.rds")$mtime>file.info("./classificador_vol_america/vect/class_auto/all_class_66.rds")$mtime){
        vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_class_66.rds")
        saveRDS(vect_dt,"./classificador_vol_america/vect/class_auto/all.rds")
        unlink("./classificador_vol_america/vect/class_auto/all_class_66.rds")
      }
    }
  }else if(file.exists("./classificador_vol_america/vect/class_auto/all_class_00.rds")){
    if(!file.exists("./classificador_vol_america/vect/class_auto/all.rds")){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_class_00.rds")
      saveRDS(vect_dt,"./classificador_vol_america/vect/class_auto/all.rds")
      unlink("./classificador_vol_america/vect/class_auto/all_class_00.rds")
    }else{
      if(file.info("./classificador_vol_america/vect/class_auto/all.rds")$mtime>file.info("./classificador_vol_america/vect/class_auto/all_class_00.rds")$mtime){
        vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_class_00.rds")
        saveRDS(vect_dt,"./classificador_vol_america/vect/class_auto/all.rds")
        unlink("./classificador_vol_america/vect/class_auto/all_class_00.rds")
      }
    }
  } 
  
  if(file.exists("./classificador_vol_america/vect/class_auto/all.rds")){
    vect_dt <- readRDS("./classificador_vol_america/vect/class_auto/all.rds")
  }else{
    vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all.rds")
  }
  if(!"class_auto_90"%in%colnames(vect_dt)){
    vect <- st_as_sf(vect_dt)
    rm(vect_dt)
    gc()
    
    vect$class_auto_90 <- getclass(vect,rast,0.9)
    vect_dt <- as.data.table(vect)
    rm(vect)
    gc()
    
    saveRDS(vect_dt,"./classificador_vol_america/vect/class_auto/all_class_90.rds")
    unlink("./classificador_vol_america/vect/class_auto/all.rds")
    saveRDS(vect_dt,"./classificador_vol_america/vect/class_auto/all.rds")
    unlink("./classificador_vol_america/vect/class_auto/all_class_90.rds")
  }
  if(!"class_auto_66"%in%colnames(vect_dt)){
    vect <- st_as_sf(vect_dt)
    rm(vect_dt)
    gc()
    
    vect$class_auto_66 <- getclass(vect,rast,0.6)
    vect_dt <- as.data.table(vect)
    rm(vect)
    gc()
    
    saveRDS(vect_dt,"./classificador_vol_america/vect/class_auto/all_class_66.rds")
    unlink("./classificador_vol_america/vect/class_auto/all.rds")
    saveRDS(vect_dt,"./classificador_vol_america/vect/class_auto/all.rds")
    unlink("./classificador_vol_america/vect/class_auto/all_class_66.rds")
  }
  if(!"class_auto_00"%in%colnames(vect_dt)){
    vect <- st_as_sf(vect_dt)
    rm(vect_dt)
    gc()
    
    vect$class_auto_00 <- getclass(vect,rast,0)
    vect_dt <- as.data.table(vect)
    rm(vect)
    gc()
    
    saveRDS(vect_dt,"./classificador_vol_america/vect/class_auto/all_class_00.rds")
    unlink("./classificador_vol_america/vect/class_auto/all.rds")
    saveRDS(vect_dt,"./classificador_vol_america/vect/class_auto/all.rds")
    unlink("./classificador_vol_america/vect/class_auto/all_class_00.rds")
  }
  return(vect_dt)
}



getclass <- function(vect,rast,threshold){
  source("./classificador_vol_america/scripts/clump_vector.R")
  if(!"id"%in%colnames(vect)){
    vect$id <- 1:nrow(vect)
  }
  ids <- vect$id
  if(!"area"%in%colnames(vect)){
    vect$area <- st_area(st_as_sf(vect))
  }
  area <- vect$area
  n.cores <- detectCores()
  n.cores <- 2
  
  ids <- ids[order(area,decreasing=T)]
  ids <- split(ids,ceiling(seq_along(ids)/n.cores))
  for(ii in 1:length(ids)){
    if(ii%%2!=0){
      ids[[ii]] <- rev(ids[[ii]])
    }
  }
  ids <- lapply((1:n.cores),function(n.core,ids){sapply(ids,function(ii,n.core){
    ii[n.core]
  },n.core)},ids)
  ids <- lapply(ids,function(ii){
    ii[!is.na(ii)]
  })
  ids <- ids[sapply(ids,length)>0]
  if(length(ids)<n.cores){
    n.cores <- length(ids)
  }  
  
  vect_split <- ids
  for(ii in 1:length(vect_split)){
    vect_split[[ii]] <- vect[vect$id%in%vect_split[[ii]],]
  }
  ids <- vect$id
  rm(vect)
  # rm(ids)
  gc()
  
  
  unlink("./classificador_vol_america/logs/getclass_auto_class_BCN.txt")
  # clust <- create_cluster_clump_(n.cores,"getclass_auto_class_BCN")
  
  clust <- makeCluster(n.cores, outfile=paste("./classificador_vol_america/logs/","getclass_auto_class_BCN",".txt",sep=""))
  clusterExport(clust,list("vect_split"),envir=environment())
  clusterEvalQ(clust,list(library(sf),library(raster)))
  if(threshold>0){
    print("threshold")
    class <- unlist(parLapplyLB(clust,vect_split,function(vv,rast,threshold){
      setNames(sapply(1:nrow(vv),function(irow,vv,rast,threshold){
        print(irow)
        v <- vv[irow,]
        r <- crop(rast,v)
        r <- mask(r,v)
        tab <- table(values(r))
        if(max(tab)/sum(tab)>=threshold){
          c <- as.numeric(names(which.max(tab)))
          if(length(c)==0){
            return(NA)
          }else{
            return(c)
          }
        }else{
          return(NA)
        }
      },vv,rast,threshold),vv$id)
    },rast,threshold))
  }else{
    print("no threshold")
    class <- unlist(parLapplyLB(clust,vect_split,function(vv,rast,threshold){
      setNames(sapply(1:nrow(vv),function(irow,vv,rast,threshold){
        print(irow)
        v <- vv[irow,]
        r <- crop(rast,v)
        r <- mask(r,v)
        tab <- table(values(r))
        c <- as.numeric(names(which.max(tab)))
        if(length(c)==0){
          return(NA)
        }else{
          return(c)
        }
      },vv,rast,threshold),vv$id)
    },rast,threshold))
  }
  stopCluster(clust)
  rm(clust)
  rm(vect_split)
  rm(rast)
  gc()
  unlink("./classificador_vol_america/logs/getclass_auto_class_BCN.txt")
  return(class[as.character(ids)])
}

