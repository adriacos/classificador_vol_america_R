library(raster)
library(parallel)
library(sf)

calc_metrics_ini <- function(){
  source("./classificador_vol_america/scripts/calc_metrics.R")
  print(paste(Sys.time(),"calc_metrics_ini"))
  if(!file.exists("./classificador_vol_america/temp/calc_metrics_current.RData")){
    calc_metrics_current <- "ini"
    save(calc_metrics_current,file="./classificador_vol_america/temp/calc_metrics_current.RData")
    rm(calc_metrics_current)
    calc_metrics_ini()
  }else{
    load("./classificador_vol_america/temp/calc_metrics_current.RData")
    stop()
    if(calc_metrics_current=="ini"){
      calc_metrics_set()
    }else if(calc_metrics_current=="set"){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all.rds")
      calc_value(vect_dt)
    }else if(calc_metrics_current=="value"){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all.rds")
      calc_neighbors(vect_dt)
    }else if(calc_metrics_current=="neighbors"){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all.rds")
      calc_clima(vect_dt)
    }else if(calc_metrics_current=="climate"){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all.rds")
      calc_topographic(vect_dt)
    }else if(calc_metrics_current=="topographic"){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all.rds")
      calc_shape(vect_dt)
    }else if(calc_metrics_current=="shape"){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all.rds")
      calc_texture(vect_dt)
    }else if(calc_metrics_current=="texture"){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all.rds")
      calc_neighbor_metrics(vect_dt)
    }else if(calc_metrics_current=="neighbors_metrics"){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all.rds")
      calc_neighbor_metrics_2(vect_dt)
    }else if(calc_metrics_current=="neighbors_metrics_2"){
      stop("all metrics calculated")
    }
  }
  
  # colnames(vect_dt)
  # colnames(vect_dt_m)
  # # vect_dt_m[,append("id",colnames(vect_dt_m)[!colnames(vect_dt_m)%in%colnames(vect_dt)])]
  # vect_dt <- merge(vect_dt,vect_dt_m[,append("id",colnames(vect_dt_m)[!colnames(vect_dt_m)%in%colnames(vect_dt)])])
}

calc_metrics_set <- function(){
  print(paste(Sys.time(),"calc_metrics_set"))
  dir.create("./classificador_vol_america/vect/metrics")
  vect_dt <- readRDS("./classificador_vol_america/vect/clumped/all.rds")
  
  vect_dt <- id <- 1:nrow(vect_dt)
  
  saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
  unlink("./classificador_vol_america/vect/set/all.rds")
  
  calc_metrics_current <- "set"
  save(calc_metrics_current,file="./classificador_vol_america/temp/calc_metrics_current.RData")
  rm(list=ls())
  gc()
  
  library(rstudioapi)
  restartSession(command=source("./classificador_vol_america/scripts/continue.R"))
}

calc_topographic <- function(vect_dt){
  if(file.exists("./classificador_vol_america/vect/metrics/all_topo.rds")){
    if(!file.exists("./classificador_vol_america/vect/metrics/all.rds")){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_topo.rds")
      saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
      unlink("./classificador_vol_america/vect/metrics/all_topo.rds")
    }else{
      if(file.info("./classificador_vol_america/vect/metrics/all.rds")$mtime>file.info("./classificador_vol_america/vect/metrics/all_topo.rds")$mtime){
        vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_topo.rds")
        saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
        unlink("./classificador_vol_america/vect/metrics/all_topo.rds")
      }
    }
  }
  if(!"elevation"%in%colnames(vect_dt)){
    vect <- st_as_sf(vect_dt)
    rm(vect_dt)
    gc()
    # library(raster)
    elev <- raster("./classificador_vol_america/maps/elevacions_CAT.tif")
    source("./classificador_vol_america/scripts/clump_vector.R")
    vect <- extract_metrics_parallel(vect,elev,quad_id="all",file_id="topo",var="elevation",operations=c("mean"),log=F)
    rm(elev)
    gc()
    pend <- raster("./classificador_vol_america/maps/pendent_CAT.tif")
    vect <- extract_metrics_parallel(vect,pend,quad_id="all",file_id="topo",var="slope",operations=c("mean"),log=F)
    rm(pend)
    gc()
    shade <- raster("./classificador_vol_america/maps/ombrejat_CAT.tif")
    vect <- extract_metrics_parallel(vect,shade,quad_id="all",file_id="topo",var="shade",operations=c("mean"),log=F)
    rm(shade)
    gc()
    twi <- raster("./classificador_vol_america/maps/TWI_osona.tif")
    vect <- extract_metrics_parallel(vect,twi,quad_id="all",file_id="topo",var="twi",operations=c("mean"),log=F)
    rm(twi)
    gc()
    
    colnames(vect)[colnames(vect)=="elevationmean"] <- "elevation"
    colnames(vect)[colnames(vect)=="slopemean"] <- "slope"
    colnames(vect)[colnames(vect)=="shademean"] <- "shade"
    colnames(vect)[colnames(vect)=="twimean"] <- "twi"
    
    source("./classificador_vol_america/scripts/clump_vector.R")
    n.cores <- detectCores()
    vects_split <- split(vect, factor(sort(rank(row.names(vect))%%n.cores)))
    unlink("./classificador_vol_america/logs/calc_long.txt")
    clust <- create_cluster_clump_(n.cores,"calc_long")
    vect$lng <- unlist(parLapplyLB(clust,vects_split,function(vv){
      sapply(1:nrow(vv),function(irow,vv){
        print(irow)
        v <- vv[irow,]
        st_bbox(v)["xmin"]
      },vv)
    }))
    stopCluster(clust)
    rm(clust)
    gc()
    unlink("./classificador_vol_america/logs/calc_long.txt")
    unlink("./classificador_vol_america/logs/calc_lat.txt")
    clust <- create_cluster_clump_(n.cores,"calc_lat")
    vect$lat <- unlist(parLapplyLB(clust,vects_split,function(vv){
      sapply(1:nrow(vv),function(irow,vv){
        print(irow)
        v <- vv[irow,]
        st_bbox(v)["xmin"]
      },vv)
    }))
    stopCluster(clust)
    rm(clust)
    gc()
    unlink("./classificador_vol_america/logs/calc_lat.txt")
    rm(vects_split)
    gc()
    
    vect_dt <- as.data.frame(vect)
    rm(vect)
    gc()
    
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all_topo.rds")
    unlink("./classificador_vol_america/vect/metrics/all.rds")
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
    unlink("./classificador_vol_america/vect/metrics/all_topo.rds")
  }
  
  calc_metrics_current <- "topographic"
  save(calc_metrics_current,file="./classificador_vol_america/temp/calc_metrics_current.RData")
  rm(list=ls())
  gc()
  
  library(rstudioapi)
  restartSession(command=source("./classificador_vol_america/scripts/continue.R"))
}

calc_clima <- function(vect_dt){
  if(file.exists("./classificador_vol_america/vect/metrics/all_climate.rds")){
    if(!file.exists("./classificador_vol_america/vect/metrics/all.rds")){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_climate.rds")
      saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
      unlink("./classificador_vol_america/vect/metrics/all_climate.rds")
    }else{
      if(file.info("./classificador_vol_america/vect/metrics/all.rds")$mtime>file.info("./classificador_vol_america/vect/metrics/all_climate.rds")$mtime){
        vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_climate.rds")
        saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
        unlink("./classificador_vol_america/vect/metrics/all_climate.rds")
      }
    }
  }
  
  if(!"meantemperature"%in%colnames(vect_dt)){
    vect <- st_as_sf(vect_dt)
    rm(vect_dt)
    gc()
    
    library(raster)
    clima.mean_temp <- raster("./classificador_vol_america/maps/CLIMA_ATLES9120_TMCANUAL.geotiff")
    source("./classificador_vol_america/scripts/clump_vector.R")
    vect <- extract_metrics_parallel(vect,clima.mean_temp,quad_id="all",file_id="metrics",var="meantemperature",operations=c("mean"),log=F)
    rm(clima.mean_temp)
    gc()
    
    # clima.amp_term <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_AMPTERMI/ATMOSFERA_ATLES6190_AMPTERMI_5mx5m.tif")
    clima.amp_term <- raster("./classificador_vol_america/maps/CLIMA_ATLES9120_AMPLITUDTERM.geotiff.tif")
    vect <- extract_metrics_parallel(vect,clima.amp_term,quad_id="all",file_id="metrics",var="temperaturerange",operations=c("mean"),log=F)
    rm(clima.amp_term)
    gc()
    
    # clima.mean_prec <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_PPTANUAL/ATMOSFERA_ATLES6190_PPTANUAL_5mx5m.tif")
    clima.mean_prec <- raster("./classificador_vol_america/maps/CLIMA_ATLES9120_PPTANUAL.geotiff")
    vect <- extract_metrics_parallel(vect,clima.mean_prec,quad_id="all",file_id="metrics",var="meanprecipitation",operations=c("mean"),log=F)
    rm(clima.mean_prec)
    gc()
    
    clima.mean.temp.wint <- raster("./classificador_vol_america/maps/CLIMA_ATLES9120_TMCGENER.geotiff")
    vect <- extract_metrics_parallel(vect,clima.mean.temp.wint,quad_id="all",file_id="metrics",var="meanwintertemperature",operations=c("mean"),log=F)
    rm(clima.mean.temp.wint)
    gc()
    
    colnames(vect)[colnames(vect)=="meantemperaturemean"] <- "meantemperature"
    colnames(vect)[colnames(vect)=="meanwintertemperaturemean"] <- "meanwintertemperature"
    colnames(vect)[colnames(vect)=="temperaturerangemean"] <- "temperaturerange"
    colnames(vect)[colnames(vect)=="meanprecipitationmean"] <- "meanprecipitation"
    
    vect_dt <- as.data.frame(vect)
    rm(vect)
    gc()
    
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all_climate.rds")
    unlink("./classificador_vol_america/vect/metrics/all.rds")
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
    unlink("./classificador_vol_america/vect/metrics/all_climate.rds")
  }
  
  
  calc_metrics_current <- "climate"
  save(calc_metrics_current,file="./classificador_vol_america/temp/calc_metrics_current.RData")
  rm(list=ls())
  gc()
  
  library(rstudioapi)
  restartSession(command=source("./classificador_vol_america/scripts/continue.R"))
}

calc_value <- function(vect_dt){
  if(file.exists("./classificador_vol_america/vect/metrics/all_value.rds")){
    if(!file.exists("./classificador_vol_america/vect/metrics/all.rds")){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_value.rds")
      saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
      unlink("./classificador_vol_america/vect/metrics/all_value.rds")
    }else{
      if(file.info("./classificador_vol_america/vect/metrics/all.rds")$mtime>file.info("./classificador_vol_america/vect/metrics/all_value.rds")$mtime){
        vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_value.rds")
        saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
        unlink("./classificador_vol_america/vect/metrics/all_value.rds")
      }
    }
  }
  if(!"median"%in%colnames(vect_dt)){
    rast <- raster(paste("./classificador_vol_america/rasters/original/original_normalised.tif", sep=""))
    vect <- st_as_sf(vect_dt)
    rm(vect_dt)
    gc()
    
    source("./classificador_vol_america/scripts/clump_vector.R")
    vect <- as.data.frame(extract_metrics_parallel(vect,rast,quad_id="all",file_id="metrics",var="value",operations=c("mean","stdev","median"),log=F))
    rm(rast)
    
    colnames(vect)[colnames(vect)=="valuemedian"] <- "median"
    
    vect$dv <- vect$DN-vect$median
    vect_dt <- as.data.frame(vect)
    rm(vect)
    gc()
    
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all_value.rds")
    unlink("./classificador_vol_america/vect/metrics/all.rds")
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
    unlink("./classificador_vol_america/vect/metrics/all_value.rds")
  }
  
  calc_metrics_current <- "value"
  save(calc_metrics_current,file="./classificador_vol_america/temp/calc_metrics_current.RData")
  rm(list=ls())
  gc()
  
  library(rstudioapi)
  restartSession(command=source("./classificador_vol_america/scripts/continue.R"))
}

calc_neighbors <- function(vect_dt){
  if(file.exists("./classificador_vol_america/vect/metrics/all_neighbors.rds")){
    if(!file.exists("./classificador_vol_america/vect/metrics/all.rds")){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_neighbors.rds")
      saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
      unlink("./classificador_vol_america/vect/metrics/all_neighbors.rds")
    }else{
      if(file.info("./classificador_vol_america/vect/metrics/all.rds")$mtime>file.info("./classificador_vol_america/vect/metrics/all_neighbors.rds")$mtime){
        vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_neighbors.rds")
        saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
        unlink("./classificador_vol_america/vect/metrics/all_neighbors.rds")
      }
    }
  }
  
  if(!"neighbors"%in%colnames(vect_dt)){
    source("./classificador_vol_america/scripts/clump_vector.R")
    vect_dt <- as.data.frame(extract_neighbors_parallel(st_as_sf(vect_dt),quad_id="all",file_id="all",log=F))
    
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all_neighbors.rds")
    unlink("./classificador_vol_america/vect/metrics/all.rds")
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics//all.rds")
    unlink("./classificador_vol_america/vect/metrics/all_neighbors.rds")
  } 
  
  calc_metrics_current <- "neighbors"
  save(calc_metrics_current,file="./classificador_vol_america/temp/calc_metrics_current.RData")
  rm(list=ls())
  gc()
  
  library(rstudioapi)
  restartSession(command=source("./classificador_vol_america/scripts/continue.R"))
}

calc_shape <- function(vect_dt){
  if(file.exists("./classificador_vol_america/vect/metrics/all_shape.rds")){
    if(!file.exists("./classificador_vol_america/vect/metrics/all.rds")){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_shape.rds")
      saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
      unlink("./classificador_vol_america/vect/metrics/all_shape.rds")
    }else{
      if(file.info("./classificador_vol_america/vect/metrics/all.rds")$mtime>file.info("./classificador_vol_america/vect/metrics/all_shape.rds")$mtime){
        vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_shape.rds")
        saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
        unlink("./classificador_vol_america/vect/metrics/all_shape.rds")
      }
    }
  } 
  
  if(!"longness"%in%colnames(vect_dt)){
    vect <- st_as_sf(vect_dt)
    vect$area <- as.numeric(st_area(vect))
    
    library(lwgeom)
    vect$tmp <- ceiling((1:nrow(vect))/(nrow(vect)/4))
    vect_split <- split(vect,vect$tmp)
    vect$perimeter <- unlist(lapply(vect_split,function(vv){
      as.numeric(st_perimeter_lwgeom(vv))
    }))
    vect$tmp <- NULL
    # vect$perimeter <- as.numeric(st_perimeter(vect))
    vect$shape <- (2*pi*sqrt(vect$area))/vect$perimeter
    
    vect$squaredness <- calc_squaredness(vect)
    vect$longness <- calc_longness(vect)
    
    vect_dt <- as.data.frame(vect)
    rm(vect)
    gc()
    
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all_shape.rds")
    unlink("./classificador_vol_america/vect/metrics/all.rds")
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
    unlink("./classificador_vol_america/vect/metrics/all_shape.rds")
  }
  
  
  calc_metrics_current <- "shape"
  save(calc_metrics_current,file="./classificador_vol_america/temp/calc_metrics_current.RData")
  rm(list=ls())
  gc()
  
  library(rstudioapi)
  restartSession(command=source("./classificador_vol_america/scripts/continue.R"))
  
}

calc_texture <- function(vect_dt){
  
  if(file.exists("./classificador_vol_america/vect/metrics/all_texture_focal.rds")){
    if(!file.exists("./classificador_vol_america/vect/metrics/all.rds")){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_texture_focal.rds")
      saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
      unlink("./classificador_vol_america/vect/metrics/all_texture_focal.rds")
    }else{
      if(file.info("./classificador_vol_america/vect/metrics/all.rds")$mtime>file.info("./classificador_vol_america/vect/metrics/all_texture_focal.rds")$mtime){
        vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_texture_focal.rds")
        saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
        unlink("./classificador_vol_america/vect/metrics/all_texture_focal.rds")
      }
    }
  }else if(file.exists("./classificador_vol_america/vect/metrics/all_texture_tpi.rds")){
    if(!file.exists("./classificador_vol_america/vect/metrics/all.rds")){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_texture_tpi.rds")
      saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
      unlink("./classificador_vol_america/vect/metrics/all_texture_tpi.rds")
    }else{
      if(file.info("./classificador_vol_america/vect/metrics/all.rds")$mtime>file.info("./classificador_vol_america/vect/metrics/all_texture_tpi.rds")$mtime){
        vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_texture_tpi.rds")
        saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
        unlink("./classificador_vol_america/vect/metrics/all_texture_tpi.rds")
      }
    }
  }else if(file.exists("./classificador_vol_america/vect/metrics/all_texture.rds")){
    if(!file.exists("./classificador_vol_america/vect/metrics/all.rds")){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_texture.rds")
      saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
      unlink("./classificador_vol_america/vect/metrics/all_texture.rds")
    }else{
      if(file.info("./classificador_vol_america/vect/metrics/all.rds")$mtime>file.info("./classificador_vol_america/vect/metrics/all_texture.rds")$mtime){
        vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_texture.rds")
        saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
        unlink("./classificador_vol_america/vect/metrics/all_texture.rds")
      }
    }
  }
  
  if(!"sa_1"%in%colnames(vect_dt)){
    vect <- st_as_sf(vect_dt)
    rm(vect_dt)
    gc()
    
    rast <- raster(paste("./classificador_vol_america/rasters/original/original_normalised.tif", sep=""))
    # vect$tpi <- calc_TPI_by_polygons(rast, vect)
    
    library(geodiv)
    vect <- calc_focal_metrics_by_polygons(vect,rast,1)
    vect <- calc_focal_metrics_by_polygons(vect,rast,2)
    vect <- calc_focal_metrics_by_polygons(vect,rast,3)
    
    vect_dt <- as.data.frame(vect)
    rm(vect)
    gc()
    
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all_texture_focal.rds")
    unlink("./classificador_vol_america/vect/metrics/all.rds")
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
    unlink("./classificador_vol_america/vect/metrics/all_texture_focal.rds")
  }
  if(!"tpi_1"%in%colnames(vect_dt)){
    vect <- st_as_sf(vect_dt)
    rm(vect_dt)
    gc()
    
    vect <- calc_TPI_by_polygons(vect,rast,1)
    vect <- calc_TPI_by_polygons(vect,rast,2)
    vect <- calc_TPI_by_polygons(vect,rast,3)
    
    vect_dt <- as.data.frame(vect)
    rm(vect)
    gc()
    
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all_texture_tpi.rds")
    unlink("./classificador_vol_america/vect/metrics/all.rds")
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
    unlink("./classificador_vol_america/vect/metrics/all_texture_tpi.rds")
  }
  
  if(!"denspl"%in%colnames(vect_dt)){
    vect_dt$denspl <- vect_dt$npl/(vect_dt$area/10000)
    vect_dt$plaredv <- sapply(str_split(vect_dt$plare,","),function(plares){
      mean(as.numeric(plares))-median(as.numeric(plares))
    })
    vect_dt$plaresd <- sapply(str_split(vect_dt$plare,","),function(plares){
      sd(as.numeric(plares))
    })
    vect_dt$plaremn <- sapply(str_split(vect_dt$plare,","),function(plares){
      mean(as.numeric(plares))
    })
    
    vect_dt$pldndv <- sapply(str_split(vect_dt$pldn,","),function(pldns){
      mean(as.numeric(pldns))-median(as.numeric(pldns))
    })
    vect_dt$pldnsd <- sapply(str_split(vect_dt$pldn,","),function(pldns){
      sd(as.numeric(pldns))
    })
    vect_dt$pldnmn <- sapply(str_split(vect_dt$pldn,","),function(pldns){
      mean(as.numeric(pldns))
    })
    
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all_texture.rds")
    unlink("./classificador_vol_america/vect/metrics/all.rds")
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
    unlink("./classificador_vol_america/vect/metrics/all_texture.rds")
  }
  
  calc_metrics_current <- "texture"
  save(calc_metrics_current,file="./classificador_vol_america/temp/calc_metrics_current.RData")
  rm(list=ls())
  gc()
  
  library(rstudioapi)
  restartSession(command=source("./classificador_vol_america/scripts/continue.R"))
}

calc_neighbor_metrics <- function(vect_dt){
  if(file.exists("./classificador_vol_america/vect/metrics/all_neighbors_metrics.rds")){
    if(!file.exists("./classificador_vol_america/vect/metrics/all.rds")){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_neighbors_metrics.rds")
      saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
      unlink("./classificador_vol_america/vect/metrics/all_neighbors_metrics.rds")
    }else{
      if(file.info("./classificador_vol_america/vect/metrics/all.rds")$mtime>file.info("./classificador_vol_america/vect/metrics/all_neighbors_metrics.rds")$mtime){
        vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_neighbors_metrics.rds")
        saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
        unlink("./classificador_vol_america/vect/metrics/all_neighbors_metrics.rds")
      }
    }
  }
  
  if(!"nmndifDN"%in%colnames(vect_dt)){
    vect_dt$tmp <- ceiling((1:nrow(vect_dt))/(nrow(vect_dt)/detectCores()))
    vect_spl <- split(vect_dt,vect_dt$tmp)
    unlink("./classificador_vol_america/logs/calc_neighbor_metrics.txt")
    library(parallel)
    source("./classificador_vol_america/scripts/clump_vector.R")
    cl <- create_cluster_clump_(detectCores(),"calc_neighbor_metrics")
    vect_dt <- parLapplyLB(cl,vect_spl,function(vv,vect_dt){
      do.call(rbind,lapply(1:nrow(vv),function(irow,vv,vect_dt){
        if(irow%%500==0){
          print(irow)        
        }
        calc_neighbor_metrics_(vv[irow,],vect_dt)
      },vv,vect_dt))
    },vect_dt)
    stopCluster(cl)
    rm(cl)
    unlink("./classificador_vol_america/logs/calc_neighbor_metrics.txt")
    rm(vect_spl)
    vect_dt$tmp <- NULL
    gc()
    vect_dt <- do.call(rbind,vect_dt)
    
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all_neighbors_metrics.rds")
    unlink("./classificador_vol_america/vect/metrics/all.rds")
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
    unlink("./classificador_vol_america/vect/metrics/all_neighbors_metrics.rds")
  }
  calc_metrics_current <- "neighbors_metrics"
  save(calc_metrics_current,file="./classificador_vol_america/temp/calc_metrics_current.RData")
  rm(list=ls())
  gc()
  
  library(rstudioapi)
  restartSession(command=source("./classificador_vol_america/scripts/continue.R"))
}
calc_neighbor_metrics_ <- function(v,vect_dt){
  # print(id)
  # v <- vect_dt[id==id,]
  # metrics <- c("twi")
  metrics <- c("DN","sd","median","dv",
               "area","perimeter","shape","sqr","longness",
               "slope",
               "sa_1","ssk_1","smean_1","tpi_1","vrm_1","rie_1","adjsd_1","sapa_1","bpi_1",
               "sa_2","ssk_2","smean_2","tpi_2","vrm_2","rie_2","adjsd_2","sapa_2","bpi_2",
               "sa_3","ssk_3","smean_3","tpi_3","vrm_3","rie_3","adjsd_3","sapa_3","bpi_3",
               "twi",
               "denspl","plaredv","pldndv","plaresd","pldnsd","plaremn","pldnmn")
  metrics <- metrics[metrics%in%colnames(v)]
  
  for(metric in metrics){
    if(v$neighbors==""){
      # v[,paste("n","mn",metric,sep="")] <- NA
      v[,paste("n","sd",metric,sep="")] <- NA
      # v[,paste("n","dif","mn",metric,sep="")] <- NA
      v[,paste("n","mn","dif",metric,sep="")] <- NA
    }else{
      # v[,paste("n","mn",metric,sep="")] <- mean(vect[vect_dt$id %in% str_split(v$neighbors, ",")[[1]],metric],na.rm=T)
      v[,paste("n","sd",metric,sep="")] <- sd(vect_dt[vect_dt$id %in% str_split(v$neighbors, ",")[[1]],metric],na.rm=T)  
      # v[,paste("n","dif","mn",metric,sep="")] <- v[,metric]-v[,paste("n","mn",metric,sep="")]
      v[,paste("n","mn","dif",metric,sep="")] <- mean(v[,metric]-vect_dt[vect_dt$id %in% str_split(v$neighbors, ",")[[1]],metric],na.rm=T)
    }
  }
  return(v)
}
calc_neighbor_metrics_2 <- function(vect_dt){
  if(file.exists("./classificador_vol_america/vect/metrics/all_neighbors_metrics_2.rds")){
    if(!file.exists("./classificador_vol_america/vect/metrics/all.rds")){
      vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_neighbors_metrics_2.rds")
      saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
      unlink("./classificador_vol_america/vect/metrics/all_neighbors_metrics_2.rds")
    }else{
      if(file.info("./classificador_vol_america/vect/metrics/all.rds")$mtime>file.info("./classificador_vol_america/vect/metrics/all_neighbors_metrics_2.rds")$mtime){
        vect_dt <- readRDS("./classificador_vol_america/vect/metrics/all_neighbors_metrics_2.rds")
        saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
        unlink("./classificador_vol_america/vect/metrics/all_neighbors_metrics_2.rds")
      }
    }
  }
  
  if(!"nmndifDN_2"%in%colnames(vect_dt)){
    vect_dt$tmp <- ceiling((1:nrow(vect_dt))/(nrow(vect_dt)/detectCores()))
    vect_spl <- split(vect_dt,vect_dt$tmp)
    unlink("./classificador_vol_america/logs/calc_neighbor_metrics_2.txt")
    library(parallel)
    source("./classificador_vol_america/scripts/clump_vector.R")
    cl <- create_cluster_clump_(detectCores(),"calc_neighbor_metrics_2")
    vect_dt <- parLapplyLB(cl,vect_spl,function(vv,vect_dt){
      do.call(rbind,lapply(1:nrow(vv),function(irow,vv,vect_dt){
        if(irow%%500==0){
          print(irow)        
        }
        calc_neighbor_metrics_2_(vv[irow,],vect_dt)
      },vv,vect_dt))
    },vect_dt)
    stopCluster(cl)
    rm(cl)
    unlink("./classificador_vol_america/logs/calc_neighbor_metrics_2.txt")
    rm(vect_spl)
    vect_dt$tmp <- NULL
    gc()
    vect_dt <- do.call(rbind,vect_dt)
    
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all_neighbors_metrics_2.rds")
    unlink("./classificador_vol_america/vect/metrics/all.rds")
    saveRDS(vect_dt,"./classificador_vol_america/vect/metrics/all.rds")
    unlink("./classificador_vol_america/vect/metrics/all_neighbors_metrics_2.rds")
  }
  calc_metrics_current <- "neighbors_metrics_2"
  save(calc_metrics_current,file="./classificador_vol_america/temp/calc_metrics_current.RData")
  rm(list=ls())
  gc()
  
  library(rstudioapi)
  restartSession(command=source("./classificador_vol_america/scripts/continue.R"))
}
calc_neighbor_metrics_2_ <- function(v,vect_dt){
  # print(id)
  # v <- vect_dt[id==id,]
  metrics <- c("DN","sd","median","dv",
               "area","perimeter","shape","sqr","longness",
               "slope",
               "sa_1","ssk_1","smean_1","tpi_1","vrm_1","rie_1","adjsd_1","sapa_1","bpi_1",
               "sa_2","ssk_2","smean_2","tpi_2","vrm_2","rie_2","adjsd_2","sapa_2","bpi_2",
               "sa_3","ssk_3","smean_3","tpi_3","vrm_3","rie_3","adjsd_3","sapa_3","bpi_3",
               "twi",
               "denspl","plaredv","pldndv","plaresd","pldnsd","plaremn","pldnmn")
  
  metrics <- metrics[metrics%in%colnames(v)]
  
  for(metric in metrics){
    if(v$neighbors==""){
      # v[,paste("n","mn",metric,"_2",sep="")] <- NA
      v[,paste("n","sd",metric,"_2",sep="")] <- NA
      # v[,paste("n","dif","mn",metric,"_2",sep="")] <- NA
      v[,paste("n","mn","dif",metric,"_2",sep="")] <- NA
    }else{
      
      neighbrs <- vect_dt[vect_dt$id %in% unique(unlist(lapply(vect_dt[vect_dt$id %in% str_split(v$neighbors, ",")[[1]],]$neighbors,function(neighbors){
        str_split(neighbors,",")[[1]]
      }))),]
      # v[,paste("n","mn",metric,sep="")] <- mean(vect[vect_dt$id %in% str_split(v$neighbors, ",")[[1]],metric],na.rm=T)
      v[,paste("n","sd",metric,"_2",sep="")] <- sd(neighbrs[,metric],na.rm=T)  
      # v[,paste("n","dif","mn",metric,sep="")] <- v[,metric]-v[,paste("n","mn",metric,sep="")]
      v[,paste("n","mn","dif",metric,"_2",sep="")] <- mean(v[,metric]-neighbrs[,metric],na.rm=T)
    }
  }
  return(v)
}

calc_squaredness <- function(vect){
  v <- vect
  #es podria fer que la toler??ncia depengu??s del per??metre, segons alguna f??rmula que hauria de provar
  library(parallel)
  v$tmp <- ceiling((1:nrow(v))/(nrow(v)/detectCores()))
  v_spl <- split(v,v$tmp)
  
  unlink("./classificador_vol_america/logs/calc_squaredness.txt")
  library(parallel)
  source("./classificador_vol_america/scripts/clump_vector.R")
  cl <- create_cluster_clump_(detectCores(),"calc_squaredness")
  smp <- do.call(rbind,parLapplyLB(cl,v_spl,function(vv){
    st_simplify(vv,preserveTopology=T,dTolerance=2)
  }))
  stopCluster(cl)
  rm(cl)
  rm(v_spl)
  v$tmp <- NULL
  gc()
  unlink("./classificador_vol_america/logs/calc_squaredness.txt")
  
  y <- st_cast(smp,"MULTILINESTRING")
  y$tmp <- ceiling((1:nrow(y))/(nrow(y)/detectCores()))
  y_spl <- split(y,y$tmp)
  
  unlink("./classificador_vol_america/logs/calc_squaredness.txt")
  cl <- create_cluster_clump_(detectCores(),"calc_squaredness")
  y <- parLapplyLB(cl,y_spl,function(yy){
    sapply(yy$id,function(id,yy){
      y <- yy[yy$id==id,]
      print(y$id)
      y_coords <- st_coordinates(y)
      vertices <- st_multipoint(y_coords[,c(1,2)]) %>% st_zm() %>% 
        st_geometry() %>% st_cast('POINT')
      rm(y_coords)
      return(length(vertices))
    },yy)
  })
  stopCluster(cl)
  rm(cl)
  unlink("./classificador_vol_america/logs/calc_squaredness.txt")
  gc()
  v$sides <- unlist(y)
  rm(y)
  sqr_p <- 1/(v$sides/log(v$perimeter))
  rm(v)
  gc()
  #sqr_p <- 1/(v$vtx/v$peri)
  sqr_p
}

calc_longness <- function(vect){
  calc_longest_line_within(vect)/vect$are
}

calc_longest_line_within <- function(vect_dt){
  vect <- st_as_sf(vect_dt)
  rm(vect_dt)
  gc()
  
  if(!"id"%in%colnames(vect)){
    vect$id <- 1:nrow(vect)
  }
  ids <- vect$id
  if(!"perimeter"%in%colnames(vect)){
    area <- st_area(st_as_sf(vect))
  }else{
    area <- vect$perimeter
  }
  n.cores <- detectCores()
  
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
  rm(area)
  gc()
  
  unlink("./classificador_vol_america/logs/calc_longest_line.txt")
  source("./classificador_vol_america/scripts/clump_vector.R")
  clust <- create_cluster_clump_(n.cores,"calc_longest_line")
  lngs_ln <- unlist(parLapplyLB(clust, vect_split, function(vcts){
    # lngs_ln_split <- lapply(vect_split, function(vcts){
    # sapply(1:50, function(i, vcts){
    setNames(sapply(1:nrow(vcts), function(i, vcts){
      if(i%%100==0){
        print(i)
      }
      calc_longest_line_within__(vcts[i,])
    },vcts),vcts$id)
  }))
  stopCluster(clust)
  rm(clust)
  unlink("./classificador_vol_america/logs/calc_longest_line.txt")
  
  return(lngs_ln[as.character(ids)])
}

calc_longest_line_within_ <- function(i, vects){
  print(length(vects))
  calc_longest_line_within__(vects[i,])
}

calc_longest_line_within__ <- function(v){
  # print(v$id)
  # v <- st_as_sf(v)
  v <- st_simplify(v,preserveTopology=T,dTolerance=2)
  vb <- st_buffer(v, -0.3, nQuadSegs=1)
  v_coords <- st_coordinates(vb)
  rm(vb)
  vertices <- st_multipoint(v_coords[,c(1,2)]) %>% st_zm() %>% 
    st_geometry() %>% st_cast('POINT')
  distances <- st_distance(vertices)
  
  
  order <- order(distances, decreasing=T)
  order <- cbind(row(distances)[order], col(distances)[order])
  order <- do.call(rbind, unique(lapply(1:nrow(order),function(i){
    sort(order[i,])
  })))
  dist <- 0
  for(rw in 1:nrow(order)){
    v1 <- vertices[order[rw,1]]
    v2 <- vertices[order[rw,2]]
    pair <- st_combine(c(v1, v2))
    line <- st_cast(pair, "LINESTRING") %>% st_set_crs(st_crs(v))
    if(length(st_crosses(v, line)[[1]])==0){
      dist <- distances[order[rw,1],order[rw,2]]
      break()
    }
  }
  return(dist) 
}

calc_focal_metrics_by_polygons <- function(vect,rast,k=1){
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
  rm(vect)
  rm(ids)
  gc()
  
  
  library(geodiv)
  unlink("./classificador_vol_america/logs/calc_focal_metrics_by_polygons.txt")
  clust <- create_cluster_clump_(n.cores,"calc_focal_metrics_by_polygons")
  # vect <- do.call(rbind,parLapplyLB(clust,ids,function(ii,vect,rast,k){
  tt <- parLapplyLB(clust,vect_split,function(rast,k){
    # vv <- vect[vect$id%in%ii,]
    do.call(rbind,lapply(1:nrow(vv),function(irow,vv,rast,k=1){
      print(irow)
      polygon <- vv[irow,]
      # buf <- st_buffer(polygon,k*prod(res(rast)))
      r <- crop(rast, polygon)
      r <- mask(r, polygon)
      metrics <- list("sa","ssk","smean")
      
      if(nrow(r)<=((k*2)+1)|ncol(r)<=((k*2)+1)){
        for(metric in metrics){
          polygon[,paste(metric,"_",k,sep="")] <- NA
        }
        return(polygon)
      }
      
      r <- focal_metrics(x=r,window=matrix(1,(k*2)+1,(k*2)+1),
                         metrics=metrics,progress=F)
      r <- lapply(r,function(rr){
        mean(values(rr)[!is.na(values(rr))&!is.infinite(values(rr))])
      })
      names(r) <- paste(names(r),"_",k,sep="")
      polygon <- cbind(polygon,r)
      polygon
    },vv,rast,k))
    # 'sa': average surface roughness
    # 'ssk': skewness
    # 'smean': mean peak height
  },rast,k)#)
  unlink("./classificador_vol_america/logs/calc_focal_metrics_by_polygons.txt")
  vect
}

calc_TPI_by_polygons <- function(vect,rast,k=1){
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
  rm(vect)
  rm(ids)
  rm(area)
  gc()
  
  
  # remotes::install_github("ailich/MultiscaleDTM")
  # install.packages("MultiscaleDTM")
  library(MultiscaleDTM)
  unlink("./classificador_vol_america/logs/calc_TPI_by_polygons.txt")
  clust <- create_cluster_clump_(n.cores,"calc_TPI_by_polygons")
  tt <- parLapplyLB(clust,vect_split,function(vv,rast,k){
    # vv <- vect[vect$id%in%ii,]
    # do.call(rbind,lapply(6550:7500,function(irow,vv,rast,k){
    do.call(rbind,lapply(1:nrow(vv),function(irow,vv,rast,k){
      print(irow)
      polygon <- vv[irow,]
      r <- crop(rast, polygon)
      r <- mask(r, polygon)
      
      if(nrow(r)<=((k*2)+1)|ncol(r)<=((k*2)+1)){
        polygon[,paste("tpi","_",k,sep="")] <- NA
        polygon[,paste("vrm","_",k,sep="")] <- NA
        polygon[,paste("rie","_",k,sep="")] <- NA
        polygon[,paste("adjsd","_",k,sep="")] <- NA
        polygon[,paste("sapa","_",k,sep="")] <- NA
        polygon[,paste("bpi","_",k,sep="")] <- NA
        return(polygon)
      }
      
      tpi <- values(TPI(r,w=c((k*2)+1,(k*2)+1),na.rm = T))
      polygon[,paste("tpi_",k,sep="")] <- mean(tpi[!is.na(tpi)])
      rm(tpi)
      vrm <- VRM(r,w=c((k*2)+1,(k*2)+1),na.rm=T)
      polygon[,paste("vrm_",k,sep="")] <- mean(vrm[!is.na(vrm)])
      rm(vrm)
      rie <- RIE(r,w=c((k*2)+1,(k*2)+1),na.rm=T)
      polygon[,paste("rie_",k,sep="")] <- mean(rie[!is.na(rie)])
      rm(rie)
      adjsd <- AdjSD(r,w=c((k*2)+1,(k*2)+1),na.rm=T)
      polygon[,paste("adjsd_",k,sep="")] <- mean(adjsd[!is.na(adjsd)])
      rm(adjsd)
      sapa <- SAPA(r,w=c((k*2)+1,(k*2)+1),na.rm=T)
      polygon[,paste("sapa_",k,sep="")] <- mean(sapa[!is.na(sapa)])
      rm(sapa)
      bpi <- BPI(r,w=c((k*2)+1,(k*2)+1),na.rm=T)
      polygon[,paste("bpi_",k,sep="")] <- mean(bpi[!is.na(bpi)])
      rm(bpi)
      polygon
    },vv,rast,k))
  },rast,k)
  stopCluster(clust)
  rm(clust)
  gc()
  unlink("./classificador_vol_america/logs/calc_TPI_by_polygons.txt")
  c
}
