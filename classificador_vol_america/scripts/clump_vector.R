source("./classificador_vol_america/scripts/project.R")
source("./classificador_vol_america/scripts/calc_metrics.R")
source("./classificador_vol_america/scripts/read_data.R")
source("./classificador_vol_america/scripts/merge.R")

library(rgdal)
library(rgeos)
library(stringr)
library(maptools)
library(exactextractr)
library(raster)
library(terra)
library(smoothr)
#library(sf)

clump_vector <- function(id){
  
  print("clump_vector")
  dir <- "./classificador_vol_america/vect/clumped/"
  
  vect <- readOGR(paste("./classificador_vol_america/vect/vectorised/", id, ".shp", sep=""))
  rast <- raster(paste("./classificador_vol_america/rasters/exported/", id, ".tif", sep=""))
  
  #treure àrea
  v <- vect(vect)
  vect$area <- expanse(v)
  rm(v)
  
  
  vect$id <- as.numeric(row.names(vect))
  if(vect[1,]$id==0){
    vect$id <- vect$id +1
  }
  
  ex <- exact_extract(rast, vect, "stdev")
  vect$sd <- ex
  vect[is.na(vect$sd),"sd"] <- 0
  
  #vect$tpi <- calc_TPI(rast, vect)
  
  #treure DN
  ex <- exact_extract(rast, vect, "mean")
  vect$DN <- ex*10
  vect[is.na(vect$DN),"DN"] <- 0.5
  rm(rast)
  gc()
  
  
  time <- Sys.time()
  neighbours <- gTouches(vect, returnDense=FALSE, byid=TRUE, )
  neighbours <- sapply(neighbours,paste,collapse=",")
  print(as.numeric(difftime(Sys.time(),time,units="secs")))
  
  vect$neighbors <- neighbours
  rm(neighbours)
  gc()
  
  vect$DN <- as.numeric(vect$DN)
  vect$area <- abs(vect$area)
  
  vect$toignore <- FALSE
  vect$npl <- 1
  vect$plare <- vect$area
  # yes <- c()
  # no200 <- c()
  # no500 <- c()
  # no3000 <- c()
  # no6000 <- c()
  # no9000 <- c()
  # no12000 <- c()
  # no15000 <- c()
  # 
  
  c <- 1
  time <- Sys.time()
  first500 <- FALSE
  first3000 <- FALSE
  first6000 <- FALSE
  first9000 <- FALSE
  first12000 <- FALSE
  first15000 <- FALSE
  repeat{
    
    vect.min <- vect[vect$toignore==FALSE,]
   
    vect.min <- vect.min[which.min(vect.min$area),]
   
    if(length(vect.min)==0){
      print("FINISH")
      break()
    }
    
    if(vect.min$area >= 500){
      if(first500==F){
        vect$toignore <- F
        first500=T
      }
    }else if(vect.min$area >= 3000){
      if(first3000==F){
        vect$toignore <- F
        first3000=T
      }
    }else if(vect.min$area >= 6000){
      if(first6000==F){
        vect$toignore <- F
        first6000=T
      }
    }else if(vect.min$area >= 9000){
      if(first9000==F){
        vect$toignore <- F
        first9000=T
      }
    }else if(vect.min$area >= 12000){
      if(first12000==F){
        vect$toignore <- F
        first12000=T
      }
    }else if(vect.min$area >= 15000){
      if(first15000==F){
        vect$toignore <- F
        first15000=T
      }
    }
    vect.min.neighbors <- vect[vect$id %in% str_split(vect.min$neighbors, ",")[[1]],]
    vect.min.neighbors <- vect.min.neighbors[vect.min.neighbors$id!=vect.min$id,]
    
    vect.min.neighbors.min <- vect.min.neighbors[
      which.min((abs(vect.min.neighbors$DN-vect.min$DN))^2*abs(vect.min.neighbors$sd-vect.min$sd))
      ,]
  
    if(first500 ==F && vect.min$area >= 200 && vect.min$area < 500 && abs(vect.min.neighbors.min$DN-vect.min$DN)>2){
      # no200 <- append(no200, (abs(vect.min.neighbors$DN-vect.min$DN))^2*abs(vect.min.neighbors$sd-vect.min$sd))
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    }else if(vect.min$area >= 500 && vect.min$area < 3000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>1.7){
      # no500 <- append(no500, (abs(vect.min.neighbors$DN-vect.min$DN))^2*abs(vect.min.neighbors$sd-vect.min$sd))
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    }else if(vect.min$area >= 3000 && vect.min$area < 6000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>1.4){
      # no3000 <- append(no3000, (abs(vect.min.neighbors$DN-vect.min$DN))^2*abs(vect.min.neighbors$sd-vect.min$sd))
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    }else if(vect.min$area >= 6000 && vect.min$area < 9000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>1.1){
      # no6000 <- append(no6000, (abs(vect.min.neighbors$DN-vect.min$DN))^2*abs(vect.min.neighbors$sd-vect.min$sd))
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    } else if(vect.min$area >= 9000 && vect.min$area < 12000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>0.8){
      # no9000 <- append(no9000, (abs(vect.min.neighbors$DN-vect.min$DN))^2*abs(vect.min.neighbors$sd-vect.min$sd))
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    } else if(vect.min$area >= 12000 && vect.min$area < 15000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>0.5){
      # no12000 <- append(no12000, (abs(vect.min.neighbors$DN-vect.min$DN))^2*abs(vect.min.neighbors$sd-vect.min$sd))
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    } else if(vect.min$area >= 15000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>0.2){
      # no15000 <- append(no15000, (abs(vect.min.neighbors$DN-vect.min$DN))^2*abs(vect.min.neighbors$sd-vect.min$sd))
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    }
    
    #yes <- append(yes, (abs(vect.min.neighbors$DN-vect.min$DN))^2*abs(vect.min.neighbors$sd-vect.min$sd))
    
    if(nrow(vect.min) > 1){
      print("ALARM! vect.min > 1")
      stop()
    }
    if(nrow(vect.min.neighbors.min) > 1){
      print("ALARM! vect.min.neighbors.min > 1")
      stop()
    }
    
    ids <- c(toString(vect.min$id), toString(vect.min.neighbors.min$id))
    
    if(length(ids) > 2){
      print("ALARM! ids > 2")
      stop()
    }
    
    vect.toagg <- vect[vect$id %in% ids,]
    
    vect.df <- as(vect.toagg,"data.frame")
    
    vect.union <- unionSpatialPolygons(vect.toagg, c(row.names(vect.df)[1],row.names(vect.df)[1]))
    
    neighbors <- paste(vect.df$neighbors,collapse=",")
    neighbors <- unique(str_split(neighbors, ","))[[1]]
    neighbors <- neighbors[!(neighbors %in% ids)]
    neighbors <- paste(neighbors, collapse = ",")
    area <- sum(vect.df$area)
    DN <- sum(vect.df$DN*(vect.df$area/area))
    sd <- sum(vect.df$sd*(vect.df$area/area))
    npl <-  sum(vect.df$npl)
    plare <- paste(str_split(vect.df[1,]$plare, ",")[[1]], str_split(vect.df[2,]$plare, ",")[[1]], collapse=",")
    #tpi <- sum(vect.df$tpi*(vect.df$area/area))
    
    vect.df <- vect.df[1,]
    vect.df$DN <- DN
    vect.df$neighbors <- neighbors
    vect.df$area <- area
    vect.df$id <- ids[1]
    vect.df$sd <- sd
    vect.df$npl <- npl
    vect.df$plare <- plare
    
    #vect.df$tpi <- tpi
    
    neighbors <- NULL
    area <- NULL
    DN <- NULL
    sd <- NULL
    #tpi <- NULL
    
    neighbors <- str_split(vect$neighbors, ",")
    neighbors <- lapply(neighbors, function(nb){nb[nb==ids[2]]<-ids[1]; nb})
    neighbors <- sapply(neighbors, paste, collapse=",")
    vect$neighbors <- neighbors
    neighbors <- NULL

    vect.shp.agg <- SpatialPolygonsDataFrame(vect.union, vect.df)
    
    vect.rest <- vect[!(vect$id %in% ids),]
    
    vect <- rbind(vect.rest, vect.shp.agg)
    
    vect.rest <- NULL
    vect.shp.agg <- NULL
    vect.df.agg <- NULL
    vect.df <- NULL
    vect.union <- NULL
    vect.toagg <- NULL
    vect.join <- NULL
    vect.min.neighbors.min <- NULL
    vect.min.neighbors <- NULL
    vect.min <- NULL
    c <- c+1
    if(c%%50==0){
      print(paste(id, as.numeric(difftime(Sys.time(),time,units="secs")), c, nrow(vect), sep="-"))
      time <- Sys.time()
    }
  }
  
  # print(paste("YEs:", mean(yes, na.rm=T)))
  # print(paste("NO200:", min(no200, na.rm=T)))
  # print(paste("NO500:", min(no500, na.rm=T)))
  # print(paste("NO3000:", min(no3000, na.rm=T)))
  # print(paste("NO6000:", min(no6000, na.rm=T)))
  # print(paste("NO9000:", min(no9000, na.rm=T)))
  # print(paste("NO12000:", min(no12000, na.rm=T)))
  # print(paste("NO15000:", min(no15000, na.rm=T)))
  
  
  rast <- raster(paste("./classificador_vol_america/rasters/exported/", id, ".tif", sep=""))
  #vect$tpi <- calc_TPI_by_polygons(vect, rast) 
  
  ex <- exact_extract(rast, vect, "stdev")
  vect$sd <- ex
  vect[is.na(vect$sd),"sd"] <- 0
  
  rm(rast)
  gc()
  
  vect <- cut_clumped_by_extent(vect, id, 2)
  
  vect.towrite <- vect[,c("npl", "plare")]
  writeOGR(vect.towrite, dir, paste(id, "_clmp", sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)  
  rm(vect.towrite)
  
  
  
  rm(vect)
  rm(vect.df)
  rm(vect.df.agg)
  rm(vect.join)
  rm(vect.min)
  rm(vect.min.neighbors)
  rm(vect.min.neighbors.min)
  
  save_id_done(id)
  file.remove(paste("./classificador_vol_america/vect/vectorised/", id, ".shp", sep=""))
  file.remove(paste("./classificador_vol_america/vect/vectorised/", id, ".dbf", sep=""))
  file.remove(paste("./classificador_vol_america/vect/vectorised/", id, ".shx", sep=""))
  file.remove(paste("./classificador_vol_america/vect/vectorised/", id, ".prj", sep=""))
  file.remove(paste("./classificador_vol_america/rasters/exported/", id, ".tif", sep=""))
  
  gc()
}

cut_clumped_by_extent <- function(vect, id, buffer=0){
  v <- vect(vect)

  ext <- reproject_EPSG_4258_vect(get_quad_vect(id))
  
  if(buffer>0){
    ext <- buffer(ext, width=buffer, dissolve=T)
  }
  
  ext <- vect(ext)
  
  v <- crop(v, ext)
  vect <- as(v, "Spatial")
  vect
}




clump_vector_global <- function(){
  
  print("clump_vector")
  # dir <- "./classificador_vol_america/vect/clumped/"
  
  #do both in parallel
  vect <- merge_clumped()
  ids <- get_clumped_ids()
  rast <- merge_rasters(read_ortofotos(ids))
  
  
  #treure àrea
  v <- vect(vect)
  vect$area <- expanse(v)
  rm(v)
  
  
  vect$id <- 1:length(vect)
  if(vect[1,]$id==0){
    vect$id <- vect$id +1
  }
  
  ex <- exact_extract(rast, vect, "stdev")
  vect$sd <- ex
  vect[is.na(vect$sd),"sd"] <- 0

  #vect$tpi <- calc_TPI(rast, vect)

  #treure DN
  ex <- exact_extract(rast, vect, "mean")
  vect$DN <- ex*10
  vect[is.na(vect$DN),"DN"] <- 0.5
  rm(rast)
  gc()
  
  
  # #gTouches was not returning all touching vectors, I don't know why
  # time <- Sys.time()
  # 
  # iii <- 1:length(vect)
  # vect$neighbors <- ""
  # vect$border <- F
  # find_neighbors<- function(i, vect){
  #   print(i)
  #   v <- vect[i,]
  #   vect.n_quad <- vect[vect$ori %in% str_split(v$n_quad, ",")[[1]],]
  #   for(ii in 1:nrow(vect.n_quad)){
  #     if (v$id==vect.n_quad[ii,]$id){
  #       next()
  #     }
  #     if(gIntersects(v, vect.n_quad[ii,], byid = F)==T){
  #       print("a")
  #       if(v$neighbors == ""){
  #         v$neighbors <- vect.n_quad[ii,]$id
  #       }else{
  #         v$neighbors <- paste(v$neighbors, vect.n_quad[ii,]$id, sep=",")
  #       }
  #       if(v$ori != vect.n_quad[ii,]$ori){
  #         v$border <- T
  #         #vect[ii,"border"] <- T
  #       }
  #     }
  #   }
  #   return(v)
  # }
  # 
  # # cluster <- makeCluster(detectCores(), outfile="log_clump.txt")
  # # clusterExport(cluster, list("iii", "find_neighbors", "vect"))
  # # clusterEvalQ(cluster, list(library(sp), library(rgeos), library(stringr)))
  # # neighbors <- parSapply(cluster, iii, find_neighbors, vect)
  # # stopCluster(cluster)
  # # 
  # vect <- do.call(rbid, sapply(iii, find_neighbors, vect))
  # 
  # 
  # # vect$neighbors <- ""
  # # vect$border <- F
  # # for(i in 1:nrow(vect)){
  # #   print(i)
  # #   vect.n_quad <- vect[vect$ori %in% str_split(vect[i,]$n_quad, ",")[[1]],]
  # #   for(ii in 1:nrow(vect.n_quad)){
  # #     if (vect[i,]$id==vect.n_quad[ii,]$id){
  # #       next()
  # #     }
  # #     if(gIntersects(vect[i,], vect.n_quad[ii,], byid = F)==T){
  # #       print("a")
  # #       if(vect[i,]$neighbors == ""){
  # #         vect[i,"neighbors"] <- vect.n_quad[ii,]$id
  # #       }else{
  # #         vect[i,"neighbors"] <- paste(vect[i,]$neighbors, vect.n_quad[ii,]$id, sep=",")
  # #       }
  # #       if(vect[i,]$ori != vect.n_quad[ii,]$ori){
  # #         vect[i,"border"] <- T
  # #         #vect[ii,"border"] <- T
  # #       }
  # #     }
  # #   }
  # # }
  # print(as.numeric(difftime(Sys.time(),time,units="secs")))
  
   time <- Sys.time()
   neighbours <- gTouches(gBuffer(vect, byid=TRUE, width=0), returnDense=FALSE, byid=TRUE, )
   neighbours <- sapply(neighbours,paste,collapse=",")
   print(as.numeric(difftime(Sys.time(),time,units="secs")))
   
   vect$neighbors <- neighbours
   rm(neighbours)
   gc()
   
  vect$border <- F
  for(i in 1:length(vect)){
    print(i)
    nbs <- str_split(vect[i,]$neighbors, ",")[[1]]
    if(length(nbs)==1&& nbs==""){
      next
    }
    for(ii in 1:length(nbs)){
      if(vect[i,]$ori != vect[vect$id==nbs[ii],]$ori){
        print("b")
        vect[i,"border"] <- T
        break()
      }
    }
  }
   
  
  vect$DN <- as.numeric(vect$DN)
  vect$area <- abs(vect$area)
  
  vect$toignore <- FALSE
  
  c <- 1
  time <- Sys.time()
  first500 <- FALSE
  first3000 <- FALSE
  first6000 <- FALSE
  first9000 <- FALSE
  first12000 <- FALSE
  first15000 <- FALSE
  
  repeat{
    
    vect.min <- vect[vect$toignore==FALSE,]
    vect.min <- vect.min[vect.min$border==TRUE,]
    vect.min <- vect.min[which.min(vect.min$area),]
    
    
    if(length(vect.min)==0){
      print("FINISH")
      break()
    }

    if(vect.min$area >= 500){
      if(first500==F){
        vect$toignore <- F
        first500=T
      }
    }else if(vect.min$area >= 3000){
      if(first3000==F){
        vect$toignore <- F
        first3000=T
      }
    }else if(vect.min$area >= 6000){
      if(first6000==F){
        vect$toignore <- F
        first6000=T
      }
    }else if(vect.min$area >= 9000){
      if(first9000==F){
        vect$toignore <- F
        first9000=T
      }
    }else if(vect.min$area >= 12000){
      if(first12000==F){
        vect$toignore <- F
        first12000=T
      }
    }else if(vect.min$area >= 15000){
      if(first15000==F){
        vect$toignore <- F
        first15000=T
      }
    }
    vect.min.neighbors <- vect[vect$id %in% str_split(vect.min$neighbors, ",")[[1]],]
    vect.min.neighbors <- vect.min.neighbors[vect.min.neighbors$id!=vect.min$id,]
    vect.min.neighbors <- vect.min.neighbors[vect.min.neighbors$border==T,]
    
    
    if(length(vect.min.neighbors) == 0){
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    }
    
    ignore <- F
    for(i in 1:length(vect.min.neighbors)){

      # if(length(str_split(vect.min$ori, ",")[[1]]) == 1 && length(str_split(vect.min.neighbors[i,]$ori, ",")[[1]]) == 1){
      #   if(str_split(vect.min.neighbors[i,]$ori, ",")[[1]] == str_split(vect.min$ori, ",")[[1]]){
      #     ignore <- T
      #     next()
      #   }
      # }
      
      if(all(str_split(vect.min.neighbors[i,]$ori, ",")[[1]] %in% str_split(vect.min$ori, ",")[[1]])){
        ignore <- T
        next()
      }

    
      if(vect.min$area < 500 && abs(vect.min.neighbors[i,]$DN-vect.min$DN)>2.2){
        if(i>=length(vect.min.neighbors[i,])){
          vect[vect$id==vect.min$id,"toignore"] <- T
          ignore <- T
          break()
        }
        next()
      }else if(vect.min$area < 500 && abs(vect.min.neighbors[i,]$DN-vect.min$DN)>2.2){
        if(i>=length(vect.min.neighbors[i,])){
          vect[vect$id==vect.min$id,"toignore"] <- T
          ignore <- T
          next()
        }
        next()
      }else if(vect.min$area < 500 && abs(vect.min.neighbors[i,]$DN-vect.min$DN)>2.2){
        if(i>=length(vect.min.neighbors[i,])){
          vect[vect$id==vect.min$id,"toignore"] <- T
          ignore <- T
          next()
        }
        next()
      }else if(vect.min$area >= 6000 && vect.min$area < 9000 && abs(vect.min.neighbors[i,]$DN-vect.min$DN)>1.3){
        if(i>=length(vect.min.neighbors[i,])){
          vect[vect$id==vect.min$id,"toignore"] <- T
          ignore <- T
          next()
        }
        next()
      } else if(vect.min$area >= 9000 && vect.min$area < 12000 && abs(vect.min.neighbors[i,]$DN-vect.min$DN)>0.9){
        if(i>=length(vect.min.neighbors[i,])){
          vect[vect$id==vect.min$id,"toignore"] <- T
          ignore <- T
          next()
        }
        next()
      } else if(vect.min$area >= 12000 && vect.min$area < 15000 && abs(vect.min.neighbors[i,]$DN-vect.min$DN)>0.7){
        if(i>=length(vect.min.neighbors[i,])){
          vect[vect$id==vect.min$id,"toignore"] <- T
          ignore <- T
          next()
        }
        next()
      } else if(vect.min$area >= 15000 && abs(vect.min.neighbors[i,]$DN-vect.min$DN)>0.4){
        if(i>=length(vect.min.neighbors[i,])){
          vect[vect$id==vect.min$id,"toignore"] <- T
          ignore <- T
          next()
        }
        next()
      } else{
        vect.min.neighbors.min <- vect.min.neighbors[i,]
        ignore <- F
        break()
      }
    }
    
    if(ignore==T){
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    }

    
    if(nrow(vect.min) > 1){
      print("ALARM! vect.min > 1")
      stop()
    }
    if(nrow(vect.min.neighbors.min) > 1){
      print("ALARM! vect.min.neighbors.min > 1")
      stop()
    }
    
    ids <- c(toString(vect.min$id), toString(vect.min.neighbors.min$id))
    
    if(length(ids) > 2){
      print("ALARM! ids > 2")
      stop()
    }
    
    vect.toagg <- vect[vect$id %in% ids,]
    
    vect.df <- as(vect.toagg,"data.frame")
    
    vect.union <- unionSpatialPolygons(vect.toagg, c(row.names(vect.df)[1],row.names(vect.df)[1]))
    
    neighbors <- paste(vect.df$neighbors,collapse=",")
    neighbors <- unique(str_split(neighbors, ","))[[1]]
    neighbors <- neighbors[!(neighbors %in% ids)]
    neighbors <- paste(neighbors, collapse = ",")
    area <- sum(vect.df$area)
    DN <- sum(vect.df$DN*(vect.df$area/area))
    sd <- sum(vect.df$sd*(vect.df$area/area))
    ori <- paste(unique(vect.df$ori), collapse=",")
    
    #tpi <- sum(vect.df$tpi*(vect.df$area/area))
    
    vect.df <- vect.df[1,]
    vect.df$DN <- DN
    vect.df$neighbors <- neighbors
    vect.df$area <- area
    vect.df$id <- ids[1]
    vect.df$sd <- sd
    vect.df$ori <- ori
    #vect.df$tpi <- tpi
    
    neighbors <- NULL
    area <- NULL
    DN <- NULL
    sd <- NULL
    #tpi <- NULL
    
    neighbors <- str_split(vect$neighbors, ",")
    neighbors <- lapply(neighbors, function(nb){nb[nb==ids[2]]<-ids[1]; nb})
    neighbors <- sapply(neighbors, paste, collapse=",")
    vect$neighbors <- neighbors
    neighbors <- NULL
    
    vect.shp.agg <- SpatialPolygonsDataFrame(vect.union, vect.df)
    
    vect.rest <- vect[!(vect$id %in% ids),]
    
    vect <- rbind(vect.rest, vect.shp.agg)
    
    vect.rest <- NULL
    vect.shp.agg <- NULL
    vect.df.agg <- NULL
    vect.df <- NULL
    vect.union <- NULL
    vect.toagg <- NULL
    vect.join <- NULL
    vect.min.neighbors.min <- NULL
    vect.min.neighbors <- NULL
    vect.min <- NULL
    c <- c+1
    if(c%%50==0){
      print(paste(as.numeric(difftime(Sys.time(),time,units="secs")), c, nrow(vect), sep="-"))
      time <- Sys.time()
    }
  }
  
  # rast <- raster(paste("./classificador_vol_america/rasters/exported/", id, ".tif", sep=""))
  #vect$tpi <- calc_TPI_by_polygons(vect, rast) 
  
  # ex <- exact_extract(rast, vect, "stdev")
  # vect$sd <- ex
  # vect[is.na(vect$sd),"sd"] <- 0
  
  # rm(rast)
  # gc()
  # v <- cut_clumped_by_extent(vect, id)
  # v.ids <- sapply(v@polygons, function(x) x@ID)
  # v.ids <- sapply(strsplit(v.ids, " "), function(x) x[1])
  # v.ids <- data.frame(v.ids)
  # row.names(v.ids) <- v.ids[,1]
  # vct <- SpatialPolygonsDataFrame(v, v.ids)
  
  # vect <- cut_clumped_by_extent(vect, id)
  
  vect.towrite <- vect
  #vect.towrite$area <- round(vect.towrite$area,2)
  # vect.towrite <- vect.towrite[,c()]
  # vect.towrite$fid <- vect$id
  #vect.towrite <- vect.towrite[,-3]
  writeOGR(vect.towrite, "./classificador_vol_america/vect/global", "global_clmp", driver = "ESRI Shapefile", overwrite_layer = TRUE)  
  rm(vect.towrite)
  
  
  
  rm(vect)
  rm(vect.df)
  rm(vect.df.agg)
  rm(vect.join)
  rm(vect.min)
  rm(vect.min.neighbors)
  rm(vect.min.neighbors.min)
  
  # save_id_done(id)
  # file.remove(paste("./classificador_vol_america/vect/vectorised/", id, ".shp", sep=""))
  # file.remove(paste("./classificador_vol_america/vect/vectorised/", id, ".dbf", sep=""))
  # file.remove(paste("./classificador_vol_america/vect/vectorised/", id, ".shx", sep=""))
  # file.remove(paste("./classificador_vol_america/vect/vectorised/", id, ".prj", sep=""))
  # file.remove(paste("./classificador_vol_america/rasters/exported/", id, ".tif", sep=""))
  
  gc()
}

