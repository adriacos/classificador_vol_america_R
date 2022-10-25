
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
    #calc_metrics_all()
    vectorised <- get_vectorised_ids()
    if(length(vectorised)>3*cores){
      clump_vectors_all()
    }
    smoothen_rasters_all()
    vectorise_rasters_all()
    vectorised <- get_vectorised_ids()
    if(length(vectorised)>3*cores){
      clump_vectors_all()
    }
    save_ortofotos_to_rasters()
    done <- get_done_ids()
  }
    
    # if(length(vectorised)>0){
    #   res <- length(vectorised)%%cores
    #   if(res==0){
    #     clump_vectors_all()
    #   }else{
    #     vectorise_rasters_all(cores-res)
    #     vectorised <- get_vectorised_ids()
    #     res <- length(vectorised)%%cores
    #     if(res==0){
    #       clump_vectors_all()
    #     }else{
    #       smoothen_rasters_all(cores-res)
    #       vectorise_rasters_all()
    #       vectorised <- get_vectorised_ids()
    #       res <- length(vectorised)%%cores
    #       if(res==0){
    #         clump_vectors_all()
    #       }else{
    #         save_ortofotos_to_rasters(cores-res)
    #         smoothen_rasters_all()
    #         vectorise_rasters_all()
    #       }
    #     }
    #   }
    # }else{
    #   smoothen <- get_smoothen_ids()
    #   if(length(smoothen)>0){
    #     res <- length(smoothen)%%cores
    #     if(res==0){
    #       vectorise_rasters_all()
    #       next()
    #     }
    #     else{
    #       smoothen_rasters_all(cores-res)
    #       vectorise_rasters_all()
    #       next()
    #     }
    #   }else{
    #     exported <- get_exported_ids()
    #     smoothen <- get_smoothen_ids()
    #     vectorised <- get_vectorised_ids()
    #     exported <- exported[!exported %in% smoothen]
    #     exported <- exported[!exported %in% vectorised]
    #     if(length(exported)>0){
    #       smoothen_rasters_all()
    #       vectorise_rasters_all()
    #       next()  
    #     }
    #   }
    # }
    # save_ortofotos_to_rasters()
    # done <- get_done_ids()
  # }
}

save_1956_diba_to_rasters <- function(){
  ids <- read_quad_ids()
  vects <- get_quad_vect(ids)
  vects <- reproject_EPSG_4258_vect(vects)
  vects <- buffer(vects, width=200, dissolve=T)
  
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
  print("save_ortofotos_to_rasters")
  done <- get_done_ids() 
  corrupted <- get_corrupted_ids() 
  
  ids <- read_quad_ids_not_exported(notin=append(done, corrupted))
  
  if(dir.exists("C:/Users/acosd/Desktop/CREAF/Mapes/Quadricula")){
   ids <- rev(ids)
  } 
  
  if(!is.null(n)){
    ids <- ids[1:n]
  }else{
    ids <- ids[1:15]
  }
  ids <- sort(ids)
  
  rm(done)
  vects <- get_quad_vect(ids)
  vects <- reproject_EPSG_4258_vect(vects)
  
  if(length(vects)!= length(ids)){
    print("ALARM - vects length != ids length")
    stop()
  }
  
  coordinates <- get_vectors_centroids_coords(vects)
  lats <- coordinates[,2]
  lngs <- coordinates[,1]
  rm(coordinates)
  rm(vects)
  gc()

  res <- mapply(create_export_ortofoto_raster, ids, lats, lngs)
  nulls <- length(sapply(res, is.null)[sapply(res, is.null)==T])
  if(nulls>0){
    save_ortofotos_to_rasters(nulls)
  }
  
  rm(lat)
  rm(long)
  gc()
}

try_export_corrupted <- function(){
  ids <- get_corrupted_ids() 
  vects <- get_quad_vect(ids)
  vects <- reproject_EPSG_4258_vect(vects)
  
  if(length(vects)!= length(ids)){
    print("ALARM - vects length != ids length")
    stop()
  }
  
  coordinates <- get_vectors_centroids_coords(vects)
  lats <- coordinates[,2]
  lngs <- coordinates[,1]
  rm(coordinates)
  rm(vects)
  gc()
  
  mapply(create_export_ortofoto_raster, ids, lats, lngs)
  
  rm(lat)
  rm(long)
  gc()
}

smoothen_rasters_all <- function(n=NULL){
  print("smoothen_rasters_all")
  #ids <- get_raster_ids_done_not_smoothen()
  ids <- get_exported_ids()
  smoothen <- get_smoothen_ids()
  vectorised <- get_vectorised_ids()
  ids <- ids[!ids %in% smoothen]
  ids <- ids[!ids %in% vectorised]
  if(length(ids)==0){
    return(NULL)
  }
  if(!is.null(n)){
    ids <- ids[1:n]
  }
 
  sapply(ids, smoothen_raster)
}

vectorise_rasters_all <- function(n=NULL){
  print("vectorise_rasters_all")
  #ids <- get_raster_ids_smoothen_done()
  ids <- get_smoothen_ids()
  if(length(ids)==0){
    return(NULL)
  }
  if(!is.null(n)){
    ids <- ids[1:n]
  }
  
  cl <- makeCluster(5, outfile="log_vectorise.txt")
  clusterExport(cl, c("ids", "vectorise_save_smoothen_raster"), envir = environment())
  clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/vectorise_raster.R"), library(raster), library(terra), library(rgdal)))
  parLapplyLB(cl, ids, vectorise_save_smoothen_raster)
  #vectorise_raster(rast)
  stopCluster(cl)
}

clump_vectors_all <- function(){
  print("clump_vectors_all")
  cores <- 5
  vects <- get_vectorised_vectors()
  vects <- vects[order(sapply(vects, nrow), decreasing=F)]
  ids <- names(vects)
  
  if(length(ids)==0){
    return(NULL)
  }
  print(ids)
    cl <- makeCluster(cores, outfile="log_clump.txt")
    clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/clump_vector.R"), library(rgdal), library(rgeos), library(stringr), library(maptools)))
    clusterExport(cl, c("ids", "clump_vector"), envir = environment())
    parLapplyLB(cl, ids, clump_vector)
    stopCluster(cl)
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
  print("calc_metrics_all")
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
  
  cl <- makeCluster(5, outfile="log_metrics.txt")
  clusterExport(cl, c("ids", "calc_metrics", "elev", "pend", "clima.mean_temp","clima.amp_term","clima.mean_prec","clima.reg_pluv"), envir = environment())
  clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/calc_metrics.R"), library(raster), library(terra), library(rgdal)))
  parLapplyLB(cl, ids, calc_metrics, elev, pend, clima.mean_temp,clima.amp_term,clima.mean_prec,clima.reg_pluv)
  stopCluster(cl)
}