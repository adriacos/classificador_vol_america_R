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

clump_vector <- function(vect, rast_global, arealimit=NULL){
  
  print("clump_vector")
  dir <- "./classificador_vol_america/vect/clumped/"
  
  rast <- crop(rast_global, vect)
  
  # vect <- readOGR(paste("./classificador_vol_america/vect/vectorised/", id, ".shp", sep=""))
  # rast <- raster(paste("./classificador_vol_america/rasters/exported/", id, ".tif", sep=""))
  id <- vect$quad_id[1]
  print(id)
  v <- vect(vect)
  vect$area <- expanse(v)
  rm(v)
  
  vect$id <- 1:nrow(vect)
  # vect$id <- as.numeric(row.names(vect))
  # if(vect[1,]$id==0){
  #   vect$id <- vect$id +1
  # }
  
  
  ex <- exact_extract(rast, vect, "stdev")
  vect$sd <- ex
  vect[is.na(vect$sd),"sd"] <- 0
  
  #treure DN
  ex <- exact_extract(rast, vect, "mean")
  vect$DN <- ex*10
  rm(ex)
  vect[is.na(vect$DN),"DN"] <- 5
  rm(rast)
  gc()
  
  time <- Sys.time()
  neighbours <- gTouches(vect, returnDense=FALSE, byid=TRUE)
  neighbours <- sapply(neighbours,paste,collapse=",")
  print(as.numeric(difftime(Sys.time(),time,units="secs")))
  vect$neighbors <- neighbours
  rm(neighbours)
  gc()
  
  vect$DN <- as.numeric(vect$DN)
  vect$area <- abs(vect$area)
  
  vect$toignore <- FALSE
  vect$npl <- 1
  
  # if(!("plare" %in% colnames(vect))){
  #   vect$plare <- round(vect$area,0)
  # }
  if(!("pldn" %in% colnames(vect))){
    vect$pldn <- vect$DN
  }
  if(!("plmaxare" %in% colnames(vect))){
    vect$plmaxare <- vect$area
  }
  
  
  
  if(nrow(vect)==1){
    return(vect)
  }
  
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
    
    if(!is.null(arealimit)&&vect.min$area>=arealimit){
      print("AREA LIMIT REACHED")
      return(vect)
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
    
    if(length(vect.min.neighbors.min)==0){
      print("No vect.min.neighbours found")
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    }
    
    if(first15000==F && (vect.min$area >= 200 && vect.min$area < 500 && abs(vect.min.neighbors.min$DN-vect.min$DN)>1.9||
       abs(vect.min.neighbors.min$sd-vect.min$sd)>0.0680371)
       ){
      # no200 <- append(no200, (abs(vect.min.neighbors$DN-vect.min$DN))^2*abs(vect.min.neighbors$sd-vect.min$sd))
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    }else if(vect.min$area >= 500 && (vect.min$area < 3000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>1.6||
             abs(vect.min.neighbors.min$sd-vect.min$sd)> 0.0572944)
    ){
      # no500 <- append(no500, (abs(vect.min.neighbors$DN-vect.min$DN))^2*abs(vect.min.neighbors$sd-vect.min$sd))
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    }else if(vect.min$area >= 3000 && (vect.min$area < 6000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>1.3||
             abs(vect.min.neighbors.min$sd-vect.min$sd)>0.0465517)
    ){
      # no3000 <- append(no3000, (abs(vect.min.neighbors$DN-vect.min$DN))^2*abs(vect.min.neighbors$sd-vect.min$sd))
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    }else if(vect.min$area >= 6000 && (vect.min$area < 9000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>1||
             abs(vect.min.neighbors.min$sd-vect.min$sd)>0.035809)
    ){
      # no6000 <- append(no6000, (abs(vect.min.neighbors$DN-vect.min$DN))^2*abs(vect.min.neighbors$sd-vect.min$sd))
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    } else if(vect.min$area >= 9000 && (vect.min$area < 12000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>0.7||
              abs(vect.min.neighbors.min$sd-vect.min$sd)>0.0250663)
    ){
      # no9000 <- append(no9000, (abs(vect.min.neighbors$DN-vect.min$DN))^2*abs(vect.min.neighbors$sd-vect.min$sd))
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    } else if(vect.min$area >= 12000 && (vect.min$area < 15000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>0.4||
              abs(vect.min.neighbors.min$sd-vect.min$sd)>0.0143236)
    ){
      # no12000 <- append(no12000, (abs(vect.min.neighbors$DN-vect.min$DN))^2*abs(vect.min.neighbors$sd-vect.min$sd))
      vect[vect$id==vect.min$id,"toignore"] <- T
      next()
    } else if(vect.min$area >= 15000 && (abs(vect.min.neighbors.min$DN-vect.min$DN)>0.1||
              abs(vect.min.neighbors.min$sd-vect.min$sd)>0.0035809)
    ){
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
    
    # plare <- paste(vect.df[1,]$plare, vect.df[2,]$plare, sep=",")
    pldn <- paste(vect.df[1,]$pldn, vect.df[2,]$pldn, sep=",")
    plmaxare <- max(vect.df$area)
    #tpi <- sum(vect.df$tpi*(vect.df$area/area))
    
    vect.df <- vect.df[1,]
    vect.df$DN <- DN
    vect.df$neighbors <- neighbors
    vect.df$area <- area
    vect.df$id <- ids[1]
    vect.df$sd <- sd
    vect.df$npl <- npl
    #vect.df$plare <- plare
    vect.df$pldn <- pldn
    vect.df$plmaxare <- plmaxare
    
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
    
    c <- c+1
    if(c%%50==0){
      print(paste(id, as.numeric(difftime(Sys.time(),time,units="secs")), c, vect.min$area, nrow(vect), sep="-"))
      time <- Sys.time()
    }
    vect.min <- NULL
  }
  
  # vect.towrite <- vect[,c("npl", "plare")]
  # writeOGR(vect.towrite, dir, paste("global_", id, "_clmp", sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)  
  # rm(vect.towrite)
  
  rm(vect.df)
  rm(vect.df.agg)
  rm(vect.join)
  rm(vect.min)
  rm(vect.min.neighbors)
  rm(vect.min.neighbors.min)
  # 
  # save_id_done(id)
  # file.remove(paste("./classificador_vol_america/vect/vectorised/", id, ".shp", sep=""))
  # file.remove(paste("./classificador_vol_america/vect/vectorised/", id, ".dbf", sep=""))
  # file.remove(paste("./classificador_vol_america/vect/vectorised/", id, ".shx", sep=""))
  # file.remove(paste("./classificador_vol_america/vect/vectorised/", id, ".prj", sep=""))
  # file.remove(paste("./classificador_vol_america/rasters/exported/", id, ".tif", sep=""))
  
  gc()
  return(vect[,c("fid", "DN", "quad_id", "quad_ori", "id", "area", "sd", "npl", "pldn", "plmaxare")])
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
  #ids <- c(12261, 12262, 12341, 12342)
  ids <- get_clumped_ids()
  
  library(future)
  plan(multisession)
  
  vect_f <- future({merge_vectors_3(ids)})
  rast_f <- future({merge_rasters(read_ortofotos(ids))})
  
  vect <- value(vect_f)
  rast <- value(rast_f)
  
  #treure àrea
  v <- vect(vect)
  vect$area <- expanse(v)
  rm(v)
  
  ex <- exact_extract(rast, vect, "stdev")
  vect$sd <- ex
  vect[is.na(vect$sd),"sd"] <- 0
  
  #treure DN
  ex <- exact_extract(rast, vect, "mean")
  vect$DN <- ex*10
  vect[is.na(vect$DN),"DN"] <- 0.5
  rm(rast)
  gc()
  
  vect$DN <- as.numeric(vect$DN)
  vect$area <- abs(vect$area)
  
  vect$toignore <- FALSE
  
  bkp <- vect
  
  #vect <- reproject_EPSG_25831_vect(vect)
  
  c <- 1
  time <- Sys.time()
  first500 <- FALSE
  first3000 <- FALSE
  first6000 <- FALSE
  first9000 <- FALSE
  first12000 <- FALSE
  first15000 <- FALSE
  
  #cnt <- 0
  
  repeat{
    
    vect.min <- vect[vect$toignore==FALSE,]
    vect.min <- vect.min[vect.min$border==TRUE,]
    vect.min <- vect.min[which.min(vect.min$area),]
    
    
    if(length(vect.min)==0){
      # if(cnt==0){
      #   print("fin")
      #   cnt <- 1
      #   vect$toignore <- FALSE
      #   next()
      # }else {
      print("FINISH")
      break()
      #}
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
    
    vect.min.neighbors$rm=F
    for(i in 1:length(vect.min.neighbors)){
      
      # if(length(str_split(vect.min$ori, ",")[[1]]) == 1 && length(str_split(vect.min.neighbors[i,]$ori, ",")[[1]]) == 1){
      #   if(str_split(vect.min.neighbors[i,]$ori, ",")[[1]] == str_split(vect.min$ori, ",")[[1]]){
      #     vect.min.neighbors[i,"rm"]<-T
      #     #print("uuuu")
      #     next()
      #   }
      # }
      
      # #if(cnt==0){
      #   #if(vect.min.neighbors[i,]$ori==vect.min$ori){
      #   if(all(str_split(vect.min.neighbors[i,]$ori, ",")[[1]] %in% str_split(vect.min$ori, ",")[[1]])){
      #     vect.min.neighbors[i,"rm"]<-T
      #     next()
      #   }
      # #}
      
      if(vect.min$area < 500 && abs(vect.min.neighbors[i,]$DN-vect.min$DN)>2.3){
        if(i>=length(vect.min.neighbors[i,])){
          vect.min.neighbors[i,"rm"]<-T
          next()
        }
        next()
      }else if(vect.min$area >= 500 && vect.min$area < 3000 && abs(vect.min.neighbors[i,]$DN-vect.min$DN)>2.0){
        if(i>=length(vect.min.neighbors[i,])){
          vect.min.neighbors[i,"rm"]<-T
          next()
        }
        next()
      }else if(vect.min$area >= 3000 && vect.min$area < 6000 && abs(vect.min.neighbors[i,]$DN-vect.min$DN)>1.7){
        if(i>=length(vect.min.neighbors[i,])){
          vect.min.neighbors[i,"rm"]<-T
          next()
        }
        next()
      }else if(vect.min$area >= 6000 && vect.min$area < 9000 && abs(vect.min.neighbors[i,]$DN-vect.min$DN)>1.4){
        if(i>=length(vect.min.neighbors[i,])){
          vect.min.neighbors[i,"rm"]<-T
          next()
        }
        next()
      } else if(vect.min$area >= 9000 && vect.min$area < 12000 && abs(vect.min.neighbors[i,]$DN-vect.min$DN)>1.1){
        if(i>=length(vect.min.neighbors[i,])){
          vect.min.neighbors[i,"rm"]<-T
          next()
        }
        next()
      } else if(vect.min$area >= 12000 && vect.min$area < 15000 && abs(vect.min.neighbors[i,]$DN-vect.min$DN)>0.8){
        if(i>=length(vect.min.neighbors[i,])){
          vect.min.neighbors[i,"rm"]<-T
          next()
        }
        next()
      } else if(vect.min$area >= 15000 && abs(vect.min.neighbors[i,]$DN-vect.min$DN)>0.5){
        if(i>=length(vect.min.neighbors[i,])){
          vect.min.neighbors[i,"rm"]<-T
          next()
        }
        next()
      } else{
        vect.min.neighbors[i,"rm"]<-F
        next()
      }
    }
    
    vect.min.neighbors <- vect.min.neighbors[vect.min.neighbors$rm==F,]
    vect.min.neighbors.min <- vect.min.neighbors[which.min(vect.min.neighbors$DN-vect.min$DN),]
    
    if(nrow(vect.min.neighbors.min)==0){
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
    
    #ori <- vect.df[which.max(vect.df$area),]$ori
    ori <- paste(unique(vect.df$ori), collapse=",")
    
    vect.df <- vect.df[1,]
    vect.df$DN <- DN
    vect.df$neighbors <- neighbors
    vect.df$area <- area
    vect.df$id <- ids[1]
    vect.df$sd <- sd
    vect.df$ori <- ori
    
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
  
  # sf <- st_as_sf(vect)
  # sf <- st_cast(sf, "POLYGON")
  # sf <- st_make_valid(sf)
  # library(nngeo)
  # sf <- st_remove_holes(sf, max_area = 0)
  # # sf$id <- 1:length(sf)
  # # nbrs <- st_touches(sf)
  # vects <- as(sf, "Spatial")
  
  vect.towrite <- vect
  writeOGR(vect.towrite, "./classificador_vol_america/vect/global", "global_clmp_no_recl", driver = "ESRI Shapefile", overwrite_layer = TRUE)  
  rm(vect.towrite)
  
  gc()
}

clump_vector_combine <- function(){
  
  vects <- reproject_EPSG_25831_vect(readOGR("./classificador_vol_america/vect/vectorised/global_qgis.shp"))
  vects <- reproject_EPSG_4258_vect(vects)
  
  vects$quad_id <- NA
  vects$quad_ori <- NA
  vects$fid <- 1: nrow(vects)
  vects$npl <- NA
  #vects$plare <- NA
  vects$pldn <- NA
  vects$plmaxare <- NA
  vects <- vects[,c("fid","quad_ori","quad_id","npl", "pldn", "plmaxare")]
  
  for (ikm in c(1,2,3,5,7,10,20,30)){
    if(file.exists(paste("./classificador_vol_america/vect/clumped/global_clmp_",ikm,"km.csv"))){
      vects <- readOGR(paste("./classificador_vol_america/vect/clumped/global_clmp_",ikm,"km.shp", sep=""))
      vects_data <- read.csv(paste("./classificador_vol_america/vect/clumped/global_clmp_",ikm,"km.csv"))
      vects <- merge(vects,vects_data,by="fid")
      next()
    }
    
    if(ikm==1){
      quads <- reproject_EPSG_4258_vect(get_quad_vect())
    }else if(ikm==2){
      quads <- reproject_EPSG_4258_vect(get_quad_vect_2km())
    }else if(ikm==3){
      quads <- reproject_EPSG_4258_vect(get_quad_vect_3km())
    }else if(ikm==5){
      quads <- reproject_EPSG_4258_vect(get_quad_vect_5km())
    }else if(ikm==7){
      quads <- reproject_EPSG_4258_vect(get_quad_vect_7km())
    }else if(ikm==10){
      quads <- reproject_EPSG_4258_vect(get_quad_vect_10km())
    }else if(ikm==20){
      quads <- reproject_EPSG_4258_vect(get_quad_vect_20km())
    }else if(ikm==30){
      quads <- reproject_EPSG_4258_vect(get_quad_vect_30km())
    }
    
    print(paste(ikm,"km"),sep="")
    for(i in 1:length(unique(quads$ori))){
      print(i)
      ori <- unique(quads$ori)[i]
      quads_ori <- quads[quads$ori==ori,]
      m <- gIntersects(vects, quads_ori, byid=T)
      for(ii in 1:nrow(m)){
        #print(ii)
        ori <- quads_ori[ii,]$ori
        id <- quads_ori[ii,]$id
        v <- which(m[ii,]==T)
        vects[v,"quad_ori"] <- ori
        vects[v,"quad_id"] <- id
      }
      rm(m)
      rm(quads_ori)
      vects_ori <- vects[!(is.na(vects$quad_ori))&vects$quad_ori==ori,]
      vects_ori_list <- split(vects_ori, vects_ori$quad_id)
      rm(vects_ori)
      vects_rest <- vects[is.na(vects$quad_ori)|vects$quad_ori!=ori,]
      rm(vects)
      rast <- raster("./classificador_vol_america/rasters/all/global.tif")
      gc()
      
      #lapply(vects_ori_list, function(x){print(nrow(x))})
      
      print("cl")
      cl <- makeCluster(12, outfile="log_clump_global.txt")
      clusterExport(cl, list("clump_vector"))
      clusterEvalQ(cl, list(library(maptools), library(rgeos), library(stringr), library(terra), library(raster), library(exactextractr)))
      #vects_ori_list <- lapply(vects_ori_list, clump_vector, rast, 200)
      vects_ori_list <- parLapplyLB(cl, vects_ori_list, clump_vector, rast, 100*ikm)
      stopCluster(cl)
      vects_ori <- do.call(rbind, vects_ori_list)
      vects_ori <- vects_ori[,c("fid", "quad_id", "quad_ori", "npl", "pldn", "plmaxare")]
      rm(vects_ori_list)
      vects <- rbind(vects_rest, vects_ori)
      rm(vects_ori)
      rm(cl)
      gc()
      # writeOGR(vects, "./classificador_vol_america/vect/clumped", paste("global_clmp_1km_",i,"_",ori, sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)
    }
    writeOGR(vects[,"fid"], "./classificador_vol_america/vect/clumped", paste("global_clmp_",ikm,"km", sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)
    save.csv(as.data.frame(vects),paste("./classificador_vol_america/vect/clumped/global_clmp_",ikm,"km.csv"))  
  }
  
  
  # quads <- reproject_EPSG_4258_vect(get_quad_vect())
  # print("1km")
  # for(i in 1:length(unique(quads$ori))){
  #   print(i)
  #   ori <- unique(quads$ori)[i]
  #   quads_ori <- quads[quads$ori==ori,]
  #   m <- gIntersects(vects, quads_ori, byid=T)
  #   for(ii in 1:nrow(m)){
  #     #print(ii)
  #     ori <- quads_ori[ii,]$ori
  #     id <- quads_ori[ii,]$id
  #     v <- which(m[ii,]==T)
  #     vects[v,"quad_ori"] <- ori
  #     vects[v,"quad_id"] <- id
  #   }
  #   rm(m)
  #   rm(quads_ori)
  #   vects_ori <- vects[!(is.na(vects$quad_ori))&vects$quad_ori==ori,]
  #   vects_ori_list <- split(vects_ori, vects_ori$quad_id)
  #   rm(vects_ori)
  #   vects_rest <- vects[is.na(vects$quad_ori)|vects$quad_ori!=ori,]
  #   rm(vects)
  #   rast <- raster("./classificador_vol_america/rasters/all/global.tif")
  #   gc()
  #   
  #   #lapply(vects_ori_list, function(x){print(nrow(x))})
  #   
  #   print("cl")
  #   cl <- makeCluster(12, outfile="log_clump_global.txt")
  #   clusterExport(cl, list("clump_vector"))
  #   clusterEvalQ(cl, list(library(maptools), library(rgeos), library(stringr), library(terra), library(raster), library(exactextractr)))
  #   #vects_ori_list <- lapply(vects_ori_list, clump_vector, rast, 200)
  #   vects_ori_list <- parLapplyLB(cl, vects_ori_list, clump_vector, rast, 100)
  #   stopCluster(cl)
  #   vects_ori <- do.call(rbind, vects_ori_list)
  #   vects_ori <- vects_ori[,c("fid", "quad_id", "quad_ori", "npl", "pldn", "plmaxare")]
  #   rm(vects_ori_list)
  #   vects <- rbind(vects_rest, vects_ori)
  #   rm(vects_ori)
  #   rm(cl)
  #   gc()
  #   writeOGR(vects, "./classificador_vol_america/vect/clumped", paste("global_clmp_1km_",i,"_",ori, sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)
  # }
  # 
  # quads <- reproject_EPSG_4258_vect(get_quad_vect_3km())
  # print("3km")
  # vects$quad_id <- NA
  # vects$quad_ori <- NA
  # for(i in 1:length(unique(quads$ori))){
  #   print(i)
  #   ori <- unique(quads$ori)[i]
  #   quads_ori <- quads[quads$ori==ori,]
  #   m <- gIntersects(vects, quads_ori, byid=T)
  #   for(ii in 1:nrow(m)){
  #     ori <- quads_ori[ii,]$ori
  #     id <- quads_ori[ii,]$id
  #     v <- which(m[ii,]==T)
  #     vects[v,"quad_ori"] <- ori
  #     vects[v,"quad_id"] <- id
  #   }
  #   rm(m)
  #   rm(quads_ori)
  #   vects_ori <- vects[!(is.na(vects$quad_ori))&vects$quad_ori==ori,]
  #   vects_ori_list <- split(vects_ori, vects_ori$quad_id)
  #   rm(vects_ori)
  #   vects_rest <- vects[is.na(vects$quad_ori)|vects$quad_ori!=ori,]
  #   rm(vects)
  #   rast <- raster("./classificador_vol_america/rasters/all/global.tif")
  #   gc()
  #   
  #   #lapply(vects_ori_list, function(x){print(nrow(x))})
  #   
  #   print("cl")
  #   cl <- makeCluster(12, outfile="log_clump_global.txt")
  #   clusterExport(cl, list("clump_vector"))
  #   clusterEvalQ(cl, list(library(maptools), library(rgeos), library(stringr), library(terra), library(raster), library(exactextractr)))
  #   vects_ori_list <- parLapplyLB(cl, vects_ori_list, clump_vector, rast, 300)
  #   stopCluster(cl)
  #   vects_ori <- do.call(rbind, vects_ori_list)
  #   vects_ori <- vects_ori[,c("fid", "quad_id", "quad_ori", "npl", "pldn", "plmaxare")]
  #   rm(vects_ori_list)
  #   vects <- rbind(vects_rest, vects_ori)
  #   rm(vects_ori)
  #   rm(cl)
  #   gc()
  #   writeOGR(vects, "./classificador_vol_america/vect/clumped", paste("global_clmp_3km_",i,"_",ori, sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)
  # }
  # 
  # quads <- reproject_EPSG_4258_vect(get_quad_vect_5km())
  # print("5km")
  # vects$quad_id <- NA
  # vects$quad_ori <- NA
  # for(i in 1:length(unique(quads$ori))){
  #   print(i)
  #   ori <- unique(quads$ori)[i]
  #   quads_ori <- quads[quads$ori==ori,]
  #   m <- gIntersects(vects, quads_ori, byid=T)
  #   for(ii in 1:nrow(m)){
  #     ori <- quads_ori[ii,]$ori
  #     id <- quads_ori[ii,]$id
  #     v <- which(m[ii,]==T)
  #     vects[v,"quad_ori"] <- ori
  #     vects[v,"quad_id"] <- id
  #   }
  #   rm(m)
  #   rm(quads_ori)
  #   vects_ori <- vects[!(is.na(vects$quad_ori))&vects$quad_ori==ori,]
  #   vects_ori_list <- split(vects_ori, vects_ori$quad_id)
  #   rm(vects_ori)
  #   vects_rest <- vects[is.na(vects$quad_ori)|vects$quad_ori!=ori,]
  #   rm(vects)
  #   rast <- raster("./classificador_vol_america/rasters/all/global.tif")
  #   gc()
  #   
  #   #lapply(vects_ori_list, function(x){print(nrow(x))})
  #   
  #   print("cl")
  #   cl <- makeCluster(12, outfile="log_clump_global.txt")
  #   clusterExport(cl, list("clump_vector"))
  #   clusterEvalQ(cl, list(library(maptools), library(rgeos), library(stringr), library(terra), library(raster), library(exactextractr)))
  #   vects_ori_list <- parLapplyLB(cl, vects_ori_list, clump_vector, rast, 500)
  #   stopCluster(cl)
  #   vects_ori <- do.call(rbind, vects_ori_list)
  #   vects_ori <- vects_ori[,c("fid", "quad_id", "quad_ori", "npl", "pldn", "plmaxare")]
  #   rm(vects_ori_list)
  #   vects <- rbind(vects_rest, vects_ori)
  #   rm(vects_ori)
  #   rm(cl)
  #   gc()
  #   writeOGR(vects, "./classificador_vol_america/vect/clumped", paste("global_clmp_5km_",i,"_",ori, sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)
  # }
  # 
  # quads <- reproject_EPSG_4258_vect(get_quad_vect_10km())
  # print("10km")
  # vects$quad_id <- NA
  # vects$quad_ori <- NA
  # for(i in 1:length(unique(quads$ori))){
  #   print(i)
  #   ori <- unique(quads$ori)[i]
  #   quads_ori <- quads[quads$ori==ori,]
  #   m <- gIntersects(vects, quads_ori, byid=T)
  #   for(ii in 1:nrow(m)){
  #     ori <- quads_ori[ii,]$ori
  #     id <- quads_ori[ii,]$id
  #     v <- which(m[ii,]==T)
  #     vects[v,"quad_ori"] <- ori
  #     vects[v,"quad_id"] <- id
  #   }
  #   rm(m)
  #   rm(quads_ori)
  #   vects_ori <- vects[!(is.na(vects$quad_ori))&vects$quad_ori==ori,]
  #   vects_ori_list <- split(vects_ori, vects_ori$quad_id)
  #   rm(vects_ori)
  #   vects_rest <- vects[is.na(vects$quad_ori)|vects$quad_ori!=ori,]
  #   rm(vects)
  #   rast <- raster("./classificador_vol_america/rasters/all/global.tif")
  #   gc()
  #   
  #   #lapply(vects_ori_list, function(x){print(nrow(x))})
  #   
  #   print("cl")
  #   cl <- makeCluster(12, outfile="log_clump_global.txt")
  #   clusterExport(cl, list("clump_vector"))
  #   clusterEvalQ(cl, list(library(maptools), library(rgeos), library(stringr), library(terra), library(raster), library(exactextractr)))
  #   vects_ori_list <- parLapplyLB(cl, vects_ori_list, clump_vector, rast, 1000)
  #   stopCluster(cl)
  #   vects_ori <- do.call(rbind, vects_ori_list)
  #   vects_ori <- vects_ori[,c("fid", "quad_id", "quad_ori", "npl", "pldn", "plmaxare")]
  #   rm(vects_ori_list)
  #   vects <- rbind(vects_rest, vects_ori)
  #   rm(vects_ori)
  #   rm(cl)
  #   gc()
  #   writeOGR(vects, "./classificador_vol_america/vect/clumped", paste("global_clmp_10km_",i,"_",ori, sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)
  # }
  # 
  # quads <- reproject_EPSG_4258_vect(get_quad_vect_20km())
  # print("20km")
  # vects$quad_id <- NA
  # vects$quad_ori <- NA
  # for(i in 1:length(unique(quads$ori))){
  #   print(i)
  #   ori <- unique(quads$ori)[i]
  #   quads_ori <- quads[quads$ori==ori,]
  #   m <- gIntersects(vects, quads_ori, byid=T)
  #   for(ii in 1:nrow(m)){
  #     ori <- quads_ori[ii,]$ori
  #     id <- quads_ori[ii,]$id
  #     v <- which(m[ii,]==T)
  #     vects[v,"quad_ori"] <- ori
  #     vects[v,"quad_id"] <- id
  #   }
  #   rm(m)
  #   rm(quads_ori)
  #   vects_ori <- vects[!(is.na(vects$quad_ori))&vects$quad_ori==ori,]
  #   vects_ori_list <- split(vects_ori, vects_ori$quad_id)
  #   rm(vects_ori)
  #   vects_rest <- vects[is.na(vects$quad_ori)|vects$quad_ori!=ori,]
  #   rm(vects)
  #   rast <- raster("./classificador_vol_america/rasters/all/global.tif")
  #   gc()
  #   
  #   #lapply(vects_ori_list, function(x){print(nrow(x))})
  #   
  #   print("cl")
  #   cl <- makeCluster(12, outfile="log_clump_global.txt")
  #   clusterExport(cl, list("clump_vector"))
  #   clusterEvalQ(cl, list(library(maptools), library(rgeos), library(stringr), library(terra), library(raster), library(exactextractr)))
  #   vects_ori_list <- parLapplyLB(cl, vects_ori_list, clump_vector, rast, 2000)
  #   stopCluster(cl)
  #   vects_ori <- do.call(rbind, vects_ori_list)
  #   vects_ori <- vects_ori[,c("fid", "quad_id", "quad_ori", "npl", "pldn", "plmaxare")]
  #   rm(vects_ori_list)
  #   vects <- rbind(vects_rest, vects_ori)
  #   rm(vects_ori)
  #   rm(cl)
  #   gc()
  #   writeOGR(vects, "./classificador_vol_america/vect/clumped", paste("global_clmp_20km_",i,"_",ori, sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)
  # }
  #afegir 2km, 7km, 30km i fer una funció a part on li passis el n.
  
  vects <- clump_vector(vects, rast)
  vects <- clump_vector(vects, rast)
  vects <- vects[,c("fid", "npl", "pldn", "plmaxare")]
  
  plare <- str_split(vects$plare, ",")
  vects$plare_mn <- sapply(lapply(plare, as.numeric), mean, na.rm=T)
  vects$plare_max <- sapply(lapply(plare, as.numeric), max, na.rm=T)
  vects$plare_min <- sapply(lapply(plare, as.numeric), min, na.rm=T)
  vects$plare_mdn <- sapply(lapply(plare, as.numeric), median, na.rm=T)
  vects$plare_sd<- sapply(lapply(plare, as.numeric), sd, na.rm=T)
  vects$plare_cv<- vects$plare_sd/vects$plare_mn
  vects$plare_maxdif<- vects$max-vects$min
  vects$plare_p10 <- sapply(lapply(plare, as.numeric), quantile, 0.10, na.rm=T)
  vects$plare_p25 <- sapply(lapply(plare, as.numeric), quantile, 0.25, na.rm=T)
  vects$plare_p75 <- sapply(lapply(plare, as.numeric), quantile, 0.75, na.rm=T)
  vects$plare_p90 <- sapply(lapply(plare, as.numeric), quantile, 0.90, na.rm=T)
  vects$plare<-NULL
  
  # abans de guardar-lo, calcular: sd, cv, mitjana, mediana, primer quartil, tercer quartil, max i min de les plare
  writeOGR(vects, "./classificador_vol_america/vect/clumped", "global_clmp", driver = "ESRI Shapefile", overwrite_layer = TRUE)
}

