source("./classificador_vol_america/scripts/project.R")



library(rgdal)
library(rgeos)
library(stringr)
library(maptools)
library(exactextractr)
library(raster)
library(terra)

clump_vector <- function(name){
  
  #name <- "vect_baga15"
  
  print("clump_vector")
  dir <- "./classificador_vol_america/vect/clumped/"
  # dir_bkp <- paste(dir, name, "/", sep="")
  # dir.create(dir_bkp)
  
  vect <- readOGR(paste("./classificador_vol_america/vect/vectorised/", name, ".shp", sep=""))
  rast <- raster(paste("./classificador_vol_america/rasters/", name, ".tif", sep=""))
  vect <- project_EPSG_25831_vect(vect)
  rast <- project_EPSG_25831_rast(rast)
  
  
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
  #treure DN
  ex <- exact_extract(rast, vect, "mean")
  vect$DN <- ex
  #vect[is.na(vect$DN),"DN"] <- 0
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
        # vect.towrite <- vect
        # vect.towrite$area <- round(vect.towrite$area,2)
        # writeOGR(vect.towrite, dir_bkp, paste(name, "_clmp_500_bkp", sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)
        # rm(vect.towrite)
        first500=T
      }
    }else if(vect.min$area >= 3000){
      if(first3000==F){
        vect$toignore <- F
        # vect.towrite <- vect
        # vect.towrite$area <- round(vect.towrite$area,2)
        # writeOGR(vect.towrite, dir_bkp, paste(name, "_clmp_3000_bkp", sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)
        # rm(vect.towrite)
        first3000=T
      }
    }else if(vect.min$area >= 6000){
      if(first6000==F){
        vect$toignore <- F
        # vect.towrite <- vect
        # vect.towrite$area <- round(vect.towrite$area,2)
        # writeOGR(vect.towrite, dir_bkp, paste(name, "_clmp_6000_bkp", sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)
        # rm(vect.towrite)
        first6000=T
      }
    }else if(vect.min$area >= 9000){
      if(first9000==F){
        vect$toignore <- F
        # vect.towrite <- vect
        # vect.towrite$area <- round(vect.towrite$area,2)
        # writeOGR(vect.towrite, dir_bkp, paste(name, "_clmp_9000_bkp", sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)
        # rm(vect.towrite)
        first9000=T
      }
    }else if(vect.min$area >= 12000){
      if(first12000==F){
        vect$toignore <- F
        # vect.towrite <- vect
        # vect.towrite$area <- round(vect.towrite$area,2)
        # writeOGR(vect.towrite, dir_bkp, paste(name, "_clmp_12000_bkp", sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)
        # rm(vect.towrite)
        first12000=T
      }
    }else if(vect.min$area >= 15000){
      if(first15000==F){
        vect$toignore <- F
        # vect.towrite <- vect
        # vect.towrite$area <- round(vect.towrite$area,2)
        # writeOGR(vect.towrite, dir_bkp, paste(name, "_clmp_12000_bkp", sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)
        # rm(vect.towrite)
        first15000=T
      }
    }
    vect.min.neighbors <- vect[vect$id %in% str_split(vect.min$neighbors, ",")[[1]],]
    vect.min.neighbors <- vect.min.neighbors[vect.min.neighbors$id!=vect.min$id,]
    
    vect.min.neighbors.min <- vect.min.neighbors[
      which.min((abs(vect.min.neighbors$DN-vect.min$DN))^2*abs(vect.min.neighbors$sd-vect.min$sd))
      ,]
  
    if(first500 ==F && vect.min$area >= 200 && vect.min$area < 500 && abs(vect.min.neighbors.min$DN-vect.min$DN)>2){
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    }else if(vect.min$area >= 500 && vect.min$area < 3000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>1.7){
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    }else if(vect.min$area >= 3000 && vect.min$area < 6000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>1.4){
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    }else if(vect.min$area >= 6000 && vect.min$area < 9000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>1.1){
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    } else if(vect.min$area >= 9000 && vect.min$area < 12000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>0.8){
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    } else if(vect.min$area >= 12000 && vect.min$area < 15000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>0.5){
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    } else if(vect.min$area >= 15000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>0.2){
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
    
    vect.df <- vect.df[1,]
    vect.df$DN <- DN
    vect.df$neighbors <- neighbors
    vect.df$area <- area
    vect.df$id <- ids[1]
    vect.df$sd <- sd
    
    neighbors <- NULL
    area <- NULL
    DN <- NULL
    sd <- NULL
    
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
  
  # neighbours <- gTouches(vect, returnDense=FALSE, byid=TRUE, )
  # neighbours <- sapply(neighbours,paste,collapse=",")
  # vect$neighbors <- neighbours
  
  vect.towrite <- vect
  #vect.towrite$area <- round(vect.towrite$area,2)
  vect.towrite <- vect.towrite[,c()]
  vect.towrite$fid <- vect$id
  #vect.towrite <- vect.towrite[,-3]
  writeOGR(vect.towrite, dir, paste(name, "_clmp", sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)  
  rm(vect.towrite)

  rm(vect)
  rm(vect.df)
  rm(vect.df.agg)
  rm(vect.join)
  rm(vect.min)
  rm(vect.min.neighbors)
  rm(vect.min.neighbors.min)
  
  
  gc()
}

