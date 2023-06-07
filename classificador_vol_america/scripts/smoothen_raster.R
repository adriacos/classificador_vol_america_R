
library(raster)
library(SpaDES)
library(rgdal)
library(parallel)
library(supercells)
library(stars)

smoothen_raster <- function(id){
  print(paste("smoothen_raster", id, Sys.time(),sep="-"))

  dir <- paste("./classificador_vol_america/rasters/smoothen/",id, sep="")
  
  # i_st <- 1
  # rast <- raster(paste("./classificador_vol_america/rasters/exported/", id, ".tif", sep=""))
  rast <- raster(paste("./classificador_vol_america/rasters/pnoa/", "BCN_1km", ".tif", sep=""))
  # max_val <- max(values(rast))
  max_val <- 255
  # values(rast) <- 10*values(rast)/max(values(rast))
  
  limit <- st_read("./classificador_vol_america/rasters/pnoa/LIMADM_PROVINCIA_BCN_1km.gpkg")
  grid <- create_grid(limit,30,30)
  
  # grid <- st_as_sf(st_make_grid(limit,n=c(30,30)))#cellsize = c(3000,3000)))
  # grid$id <- 1:nrow(grid)
  # grid$ori <- 0
  # grid[grid$id%%4==0,"ori"] <- 0
  # grid[grid$id%%4==1,"ori"] <- 1
  # grid[grid$id%%4==2,"ori"] <- 2
  # grid[grid$id%%4==3,"ori"] <- 3
  # grid <- st_intersection(grid, limit)
  # st_write(grid, "./classificador_vol_america/rasters/pnoa/LIMADM_PROVINCIA_BCN_1km_grid_3x3km.gpkg",overwrite=T)
  
  # if(dir.exists(dir)){
  #   list <- list.files(dir, pattern = "\\.tif$")
  #   if(length(list)>0){
  #     i_st <-length(list) + 1 
  #     rast <- raster(paste(dir, list[length(list)],sep="/"))  
  #   }
  # }else{
  #   dir.create(dir)
  # }
 
  n.cores <- detectCores()
  n.cores <- 6
  # if(n.cores==12){
  #   raster_split <- splitRaster(rast, 6,4, buffer=c(2,2))
  # }else if (n.cores==20){
  #   n.cores <- 12
    # raster_split <- splitRaster(rast, 30,30, buffer=c(10,10))
  raster_split <- lapply(grid$id,function(id, grid, rast){
    print(grid[grid$id==id,]$id)
    buf <- st_buffer(grid[grid$id==id,], 10)
    crop <- mask(crop(rast,buf),buf)
    writeRaster(crop,paste("./classificador_vol_america/rasters/pnoa/split/",id,".tif",sep=""))
  },grid,rast)
    # raster_split <- splitRaster(rast, 5,4, buffer=c(2,2))
  # }else{
  #   raster_split <- splitRaster(rast, 5,5, buffer=c(2,2))
  # }
  # raster_split.locations <- sapply(raster_split,function(x){x@file@name})
  # for(i in 1:length(raster_split)){
  #   writeRaster(raster_split[[i]],paste("./classificador_vol_america/rasters/pnoa/split/supercells/",i,".tif"))
  # }

  # raster_split <- lapply(paste("./classificador_vol_america/rasters/pnoa/split/",list.files("./classificador_vol_america/rasters/pnoa/split/",pattern="\\.tif$"),sep=""),raster)
  names(raster_split) <- lapply(raster_split,function(r){
    sub(" ","",sub(" ","",sub(".tif","",basename(r@file@name))))
  })
  ids_done <- sub(".tif","",list.files("./classificador_vol_america/rasters/pnoa/split/supercells",pattern="\\.tif$"))
  raster_split <- raster_split[!names(raster_split)%in%ids_done]
  
  # raster_split <- list(raster("./classificador_vol_america/rasters/pnoa/split/ 823 .tif"),
  #                 raster("./classificador_vol_america/rasters/pnoa/split/ 824 .tif"))
  # names(raster_split) <- lapply(raster_split,function(r){
  #   sub(" ","",sub(" ","",sub(".tif","",basename(r@file@name))))
  # })
  
  clust <- makeCluster(n.cores,outfile=".supercells.txt")
  clusterExport(clust, c("raster_split","smoothen_raster_supercells"), envir = environment())
  clusterEvalQ(clust, list(library(raster),library(supercells),library(terra),library(stars),library(exactextractr)))
  
  # for(i in i_st:6){
    # print(paste("start ", i, Sys.time()))
    raster_split <- parLapplyLB(clust, raster_split,smoothen_raster_supercells,max_val)
    stopCluster(clust)
    
    lapply(raster_split,smoothen_raster_supercells,max_val)
    # raster_split <- parLapply(clust, raster_split,smoothen_raster_, seed=i)
    # rast <- mergeRaster(raster_split)
    
    # rast <- mosaic(raster_split,fun=mean)
    
    # writeRaster(rast,paste(dir, "/", id, "_smth_", i, ".tif", sep=""), overwrite=TRUE)
    # print(paste("end ", i, Sys.time()))
  # }
  
  rm(clust)
  rm(raster_split)
  
  merge_rasters_mosaic()
  
  # raster_split <- lapply(paste("./classificador_vol_america/rasters/pnoa/split/supercells/",list.files("./classificador_vol_america/rasters/pnoa/split/supercells/",pattern="\\.tif$"),sep=""),raster)
  # names(raster_split) <- lapply(raster_split,function(r){
  #   sub(" ","",sub(" ","",sub(".tif","",basename(r@file@name))))
  # })
    
  # rast <- rast*10
  # #values(rast) <- round_any(values(rast), 0.5)
  res <- writeRaster(rast,paste("./classificador_vol_america/rasters/smoothen/",id,"_smth.tif", sep=""), overwrite=TRUE)
  # res <- writeRaster(rast,paste("./classificador_vol_america/rasters/smoothen/bkp/",id,"_smth.tif", sep=""), overwrite=TRUE)
  save_id_smoothen(id)
  if(exists("res")){
    unlink(dir, recursive = TRUE)
    file.remove(paste("./classificador_vol_america/rasters/exported/", id, ".tif", sep=""))
  }
  rm(dir)
  rm(res)
  rast
}

merge_rasters_mosaic <- function(){
  
  dir <- "./classificador_vol_america/rasters/pnoa/split/supercells/"
  
 
  files <- list.files(dir, pattern = "\\.tif$")
  rasters <- sapply(files,function(f){
    return(raster(paste(dir,f,sep="")))  
  })
  names(rasters) <- sub(" ","",sub(" ","",sub(".tif","",names(rasters))))
  rasters <- rasters[order(as.numeric(names(rasters)))]

  files_done <- list.files(paste(dir, "temp/",sep=""))
  ids_done <- sapply(files_done,function(x){
    string_split(x,"_")[[1]]
  })
  ids_todo <- names(rasters)
  ids_todo <- ids_todo[!ids_todo%in%ids_done]
  rasters <- rasters[as.numeric(names(rasters))%in%ids_todo]
  # ids <- ids_done
  
  # n.cores <- 2
  # rasters_spl <- list(rasters[1:floor(length(rasters)/2)],rasters[ceiling(length(rasters)/2):length(rasters)])
  # cl <- makeCluster(n.cores,outfile="smoothen_raster_merge.txt")
  # clusterExport(cl,list("rasters_spl"))
  # clusterEvalQ(cl,list(library(raster)))
  # parLapply(cl,rasters_spl,function(rasters){
    # ids_todo <- as.numeric(names(rasters))
  
  rasters <- list(raster("./classificador_vol_america/rasters/pnoa/split/supercells/823.tif"),
                  raster("./classificador_vol_america/rasters/pnoa/split/supercells/824.tif"))
  
    ids <- as.numeric(names(rasters[1]))
    merged <- rasters[[1]]
    for(i in 1:length(rasters)){
      print(i)
      r2 <- rasters[[i]]
      ids <- append(ids, as.numeric(names(rasters[i])))
      # newname <- paste(paste(ids, collapse="_"),".tif",sep="")
      if(length(ids)==1){
        newname <- paste(ids[1],".tif",sep="")
      }else{
        newname <- paste(ids[1],"_",tail(ids,1),".tif",sep="")
      }
     
      print(newname)
      
      if(i==2){
        oldname <- newname
      }
      
      template <- projectRaster(from = r2, to= merged, alignOnly=TRUE)
      print("template")
      aligned <- projectRaster(from=r2, to=template)
      alignedlocation <- aligned@file@name
      print("aligned")
      rm(template)
      gc()
      
      print(oldname)
      oldlocation <- merged@file@name
      merged <- mosaic(merged, aligned,fun=mean)
      newlocation <- merged@file@name
      if(i!=2&oldlocation!=""&oldlocation!=newlocation){
        unlink(oldlocation)
        unlink(sub(".grd",".gri",oldlocation))
        print("deleted old temp file")
      }
      print("merged")
      if(alignedlocation!=""){
        unlink(alignedlocation)
        unlink(sub(".grd",".gri",alignedlocation))
      }
      rm(aligned)
      gc()
      if(i%%10==0){
        writeRaster(merged, paste("./classificador_vol_america/rasters/pnoa/split/supercells/temp/",newname,sep=""),overwrite=T)
        print("saved") 
        # rm(merged)
        unlink(paste("./classificador_vol_america/rasters/pnoa/split/supercells/temp/",oldname,sep=""))
        print("deleted")
        oldname <- newname
        # gc()
      }
    }
  # })
  # stopCluster(cl)
  
    writeRaster(merged, paste("./classificador_vol_america/rasters/pnoa/split/supercells/","merged.tif",sep=""),overwrite=T)
    unlink(newlocation)
    unlink(sub(".grd",".gri",newlocation))
    rm(merged)
    gc()
  
  
}

smoothen_raster_supercells <- function(rast,max_val){
  id <- sub(" ","",sub(" ","",sub(".tif","",basename(rast@file@name))))
  print(id)
  if(length(unique(values(rast)))==1&is.na(unique(values(rast))[1])){
    return(rast)
  }
  rast <- rast(rast)
  # area <- expanse(rast)
  area <- 9610000
  rast_sc = supercells(rast, k = 4*area/500, compactness = 0.5)
  rm(area)
  ex <- exact_extract(rast, rast_sc, "mean")
  rast_sc$value <- ex
  rm(ex)
  rast_sc <- st_rasterize(rast_sc[,"value"])
  rast_sc <- rast(rast_sc)
  rast_sc <- raster(rast_sc)
  values(rast_sc) <- 10*values(rast_sc)/max_val 
  writeRaster(rast_sc,paste("./classificador_vol_america/rasters/pnoa/split/supercells/",id,".tif",sep=""))
  rast_sc
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





calc_TPI_by_polygons <- function(vect, rast){
  if(!"id" %in% (names(vect))){
    vect$id <- as.numeric(row.names(vect))
    if(vect[1,]$id==0){
      vect$id <- vect$id +1
    }
  }
  ids <- vect$id
  
  n.cores <- detectCores()
  
  clust <- makeCluster(n.cores, outfile="log_smoothen.txt")
  clusterExport(clust, c("ids", "vect","rast", "calc_TPI_by_polygons_", "calc_TPI_by_polygon", "calc_TPI_raster"), envir =  environment())
  clusterEvalQ(clust, library(raster))
  c <- parSapply(clust, ids, calc_TPI_by_polygons_, vect, rast)
  stopCluster(clust)
  
  c
}

calc_TPI_by_polygons_ <- function(id, vect, rast){
  print(id)
  calc_TPI_by_polygon(vect[vect$id==id,], rast)
}

calc_TPI_by_polygon <- function(polygon, rast){
  r <- crop(rast, polygon)
  r <- mask(r, polygon)
  r <- calc_TPI_raster(r)
  mean(values(r), na.rm=T)
}

calc_TPI_raster <- function(rast){
  cells <- cellFromRow(rast, c(1:nrow(rast)))
  c <- c()
  for (x in 1:length(cells)){
    if(is.na(rast[x])){
      rast[x] <- NA
    }
    adj <- adjacent(rast, x, 8, include=FALSE)
    c <- append(c, abs(mean(rast[adj[,2]][!is.na(rast[adj[,2]])], na.rm=T)-rast[x]))
  }
  values(rast) <- c
  rast
}


calc_TRI_by_polygons <- function(vect, rast){
  if(!"id" %in% (names(vect))){
    vect$id <- as.numeric(row.names(vect))
    if(vect[1,]$id==0){
      vect$id <- vect$id +1
    }
  }
  ids <- vect$id
  
  n.cores <- detectCores()
  
  clust <- makeCluster(n.cores, outfile="log_smoothen.txt")
  clusterExport(clust, c("ids", "vect","rast", "calc_TRI_by_polygons_", "calc_TRI_by_polygon", "calc_TRI_raster"), envir =  environment())
  clusterEvalQ(clust, library(raster))
  c <- parSapply(clust, ids, calc_TRI_by_polygons_, vect, rast)
  stopCluster(clust)
  c
}

calc_TRI_by_polygons_ <- function(id, vect, rast){
  print(id)
  calc_TRI_by_polygon(vect[vect$id==id,], rast)
}

calc_TRI_by_polygon <- function(polygon, rast){
  r <- crop(rast, polygon)
  r <- mask(r, polygon)
  r <- calc_TRI_raster(r)
  mean(values(r), na.rm=T)
}

calc_TRI_raster <- function(rast){
  cells <- cellFromRow(rast, c(1:nrow(rast)))
  c <- c()
  for (x in 1:length(cells)){
    if(is.na(rast[x])){
      rast[x] <- NA
    }
    adj <- adjacent(rast, x, 8, include=FALSE)
    c <- append(c, mean(abs(rast[adj[,2]][!is.na(rast[adj[,2]])]-rast[x]), na.rm=T))
  }
  values(rast) <- c
  rast
}



calc_roughness_by_polygons <- function(vect, rast){
  if(!"id" %in% (names(vect))){
    vect$id <- as.numeric(row.names(vect))
    if(vect[1,]$id==0){
      vect$id <- vect$id +1
    }
  }
  ids <- vect$id
  
  n.cores <- detectCores()
  
  clust <- makeCluster(n.cores, outfile="log_smoothen.txt")
  clusterExport(clust, c("ids", "vect","rast", "calc_roughness_by_polygons_", "calc_roughness_by_polygon", "calc_roughness_raster"), envir =  environment())
  clusterEvalQ(clust, library(raster))
  c <- parSapply(clust, ids, calc_roughness_by_polygons_, vect, rast)
  stopCluster(clust)
  c
}

calc_roughness_by_polygons_ <- function(id, vect, rast){
  print(id)
  calc_roughness_by_polygon(vect[vect$id==id,], rast)
}

calc_roughness_by_polygon <- function(polygon, rast){
  r <- crop(rast, polygon)
  r <- mask(r, polygon)
  r <- calc_roughness_raster(r)
  mean(values(r), na.rm=T)
}

calc_roughness_raster <- function(rast){
  cells <- cellFromRow(rast, c(1:nrow(rast)))
  c <- c()
  for (x in 1:length(cells)){
    if(is.na(rast[x])){
      rast[x] <- NA
    }
    adj <- adjacent(rast, x, 8, include=TRUE)
    max <- max(rast[adj[,2]][!is.na(rast[adj[,2]])], na.rm=T)
    min <- min(rast[adj[,2]][!is.na(rast[adj[,2]])], na.rm=T)
    c <- append(c, max-min)
  }
  values(rast) <- c
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