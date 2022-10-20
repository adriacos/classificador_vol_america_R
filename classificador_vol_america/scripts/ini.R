
library(parallel)
library(raster)
source("./classificador_vol_america/scripts/read_data.R")
source("./classificador_vol_america/scripts/save_data.R")
source("./classificador_vol_america/scripts/create_ortofoto_leaflet.R")
source("./classificador_vol_america/scripts/export_rasters.R")
source("./classificador_vol_america/scripts/smoothen_raster.R")
source("./classificador_vol_america/scripts/clump_vector.R")
source("./classificador_vol_america/scripts/vectorise_raster.R")
source("./classificador_vol_america/scripts/calc_metrics.R")
source("./classificador_vol_america/scripts/create_export_ortofoto_rasters.R")
source("./classificador_vol_america/scripts/create_export_1956_class_diba_rasters.R")


ini <- function(){
  
  #maximum number of parallel clump_vector that can be done. Theoretically equal 
  #to the number of cores, but practically limited to 5 due to memory issues
  #cores <- detectCores()
  cores <- 5
  
  ids <- read_quad_ids()
  done <- get_done_ids() 
  while(length(done)<length(ids)){
    #auto_class_BCN_all()
    calc_metrics_all()
    clumped <- get_clumped_ids()
    if(length(clumped)>0){
      res <- clumped%%cores
      if(res==0){
        clump_vectors_all()
      }else{
        vectorise_rasters_all(cores-res)
        clumped <- get_clumped_ids()
        res <- clumped%%cores
        if(res==0){
          clump_vectors_all()
        }else{
          smoothen_rasters_all(cores-res)
          vectorise_rasters_all()
          clumped <- get_clumped_ids()
          res <- clumped%%cores
          if(res==0){
            clump_vectors_all()
          }else{
            save_ortofotos_to_rasters(cores-res)
            smoothen_rasters_all()
            vectorise_rasters_all()
          }
        }
      }
    }else{
      smoothen_rasters_all()
      vectorise_rasters_all()
    }
    save_ortofotos_to_rasters()
    done <- get_done_ids()
  }
}

save_1956_diba_to_rasters <- function(){
  ids <- read_quad_ids()
  vects <- get_quad_vect(ids)
  vects <- reproject_EPSG_4258_vect(vects)
  
  if(length(vects)!= length(ids)){
    print("ALARM - vects length != ids length")
    stop()
  }
  
  coordinates <- get_vectors_centroids_coords(vects)
  #coordinates <- get_EPSG_4258_vectors_centroids_lat_lng(vects)
  lats <- coordinates[,2]
  lngs <- coordinates[,1]
  rm(coordinates)
  rm(vects)
  gc()
  
  mapply(create_export_1956_class_diba_leaflet, ids, lats, lngs)
  
  rm(lat)
  rm(long)
  
  
}

save_ortofotos_to_rasters <- function(n=NULL){
  
  done <- get_done_ids() 
  ids <- read_quad_ids_not_exported(notin=done)
  if(!is.null(n)){
    ids <- ids[1:n]
  }else{
    ids <- ids[1:100]
  }
  rm(done)
  vects <- get_quad_vect(ids)
  vects <- reproject_EPSG_4258_vect(vects)
  
  if(length(vects)!= length(ids)){
    print("ALARM - vects length != ids length")
    stop()
  }
  
  coordinates <- get_vectors_centroids_coords(vects)
  #coordinates <- get_EPSG_4258_vectors_centroids_lat_lng(vects)
  lats <- coordinates[,2]
  lngs <- coordinates[,1]
  rm(coordinates)
  rm(vects)
  gc()
  
  mapply(create_export_ortofoto_raster, ids, lats, lngs)

  rm(lat)
  rm(long)
}

smoothen_rasters_all <- function(n=NULL){
  #ids <- get_raster_ids_done_not_smoothen()
  ids <- get_exported_ids()
  if(!is.null(n)){
    ids <- ids[1:n]
  }
  if(length(ids)==0){
    return(NULL)
  }
  sapply(ids, smoothen_raster)
}

vectorise_rasters_all <- function(n=NULL){
  #ids <- get_raster_ids_smoothen_done()
  ids <- get_smoothen_ids()
  if(!is.null(n)){
    ids <- ids[1:n]
  }
  if(length(ids)==0){
    return(NULL)
  }
  cl <- makeCluster(5, outfile="log.txt")
  clusterExport(cl, c("ids", "vectorise_save_smoothen_raster"), envir = environment())
  clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/vectorise_raster.R"), library(raster), library(terra), library(rgdal)))
  parLapply(cl, ids, vectorise_save_smoothen_raster)
  #vectorise_raster(rast)
  stopCluster(cl)
}

clump_vectors_all <- function(){
  #cores <- detectCores()
  cores <- 5
  #ids <- get_ids_smoothen_vectorised_not_clumped()
  ids <- get_vectorised_ids()
  if(length(ids)==0){
    return(NULL)
  }
  print(ids)
  ids <- split(ids, ceiling(seq_along(ids)/5))
  for(ids_5 in ids){
    cl <- makeCluster(5, outfile="log.txt")
    clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/clump_vector.R"), library(rgdal), library(rgeos), library(stringr), library(maptools)))
    clusterExport(cl, c("ids_5", "clump_vector"), envir = environment())
    clusterMap(cl, clump_vector, ids_5)
    stopCluster(cl)
  }
}

auto_class_BCN_all <- function(){
  cores <- 5
  ids <- get_metrics_ids()
  if(length(ids)==0){
    return(NULL)
  }
  vects <- get_metrics_vectors()
  if(length(ids)!=length(vectors)){
    print("auto_class_BCN_all - ids length different than vectors length")
    stop()
  }
  cl <- makeCluster(cores, outfile="ini_log.txt")
  clusterExport(cl, c("ids","vects"), envir = environment())
  clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/auto_class_BCN.R"), source("./classificador_vol_america/scripts/read_data.R"), library(exactextractr)))
  vects <- clusterMap(cl, auto_class_BCN, vects, ids)
  stopCluster(cl)
  save_id_done(ids)
}

calc_metrics_all <- function(){
  
  #ids <- get_vectors_clumped_file_ids_not_metrics()
  ids <- get_clumped_ids()
  if(length(ids)==0){
    return(NULL)
  }
  
  elev <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Elevacions/elevacions_CAT.tif")
  pend <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Elevacions/pendent_CAT.tif")
  clima.mean_temp <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_TMPANUAL/ATMOSFERA_ATLES6190_TMPANUAL_5mx5m.tif")
  clima.amp_term <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_AMPTERMI/ATMOSFERA_ATLES6190_AMPTERMI_5mx5m.tif")
  clima.mean_prec <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_PPTANUAL/ATMOSFERA_ATLES6190_PPTANUAL_5mx5m.tif")
  clima.reg_pluv <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_REGPLUVI/ATMOSFERA_ATLES6190_REGPLUVI_5mx5m.tif")

  print(ids)
  
  cl <- makeCluster(5, outfile="log.txt")
  clusterExport(cl, c("ids", "calc_metrics", "elev", "pend", "clima.mean_temp","clima.amp_term","clima.mean_prec","clima.reg_pluv"), envir = environment())
  clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/calc_metrics.R"), library(raster), library(terra), library(rgdal)))
  parLapply(cl, ids, calc_metrics, elev, pend, clima.mean_temp,clima.amp_term,clima.mean_prec,clima.reg_pluv)
  stopCluster(cl)
}