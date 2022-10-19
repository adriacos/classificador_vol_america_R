
library(parallel)
library(raster)
source("./classificador_vol_america/scripts/read_data.R")
source("./classificador_vol_america/scripts/create_ortofoto_leaflet.R")
source("./classificador_vol_america/scripts/export_rasters.R")
source("./classificador_vol_america/scripts/smoothen_raster.R")
source("./classificador_vol_america/scripts/clump_vector.R")
source("./classificador_vol_america/scripts/vectorise_raster.R")
source("./classificador_vol_america/scripts/calc_metrics.R")
source("./classificador_vol_america/scripts/create_export_ortofoto_rasters.R")

ini <- function(){
  
  #maximum number of parallel clump_vector that can be done. Theoretically equal 
  #to the number of cores, but practically limited to 5 due to memory issues
  #cores <- detectCores()
  cores <- 5
  
  #done <- get_done_ids()
  clumped <- get_clumped_ids()
  if(length(clumped)>0){
    res <- clumped%%cores
    if(res==0){
      clump_vectors_all()
    }else{
      
    }
  }else{
    
  }
  
}

save_ortofotos_to_rasters <- function(){
  done <- get_done_ids()
  
  ids <- read_quad_ids_not_exported(notin=done)
  ids <- ids[1:100]
  rm(done)
  vects <- get_quad_vect(ids)
  
  coordinates <- get_EPSG_25831_vectors_centroids_lat_lng(vects)
  lat <- coordinates[,2]
  lng <- coordinates[,1]
  rm(coordinates)
  rm(vects)
  
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/create_map.R"), source("./classificador_vol_america/scripts/export_rasters.R"
                                        , source("./classificador_vol_america/scripts/create_export_ortofoto_rasters.R")
                                        , source("./classificador_vol_america/scripts/read_data.R"))))
  clusterExport(cl, "plots", envir = environment())
  clusterMap(cl, create_export_ortofoto_raster, ids, lat, lng)
  #mcmapply(create_export_map, plots$plot_id, plots$coords_latitude, plots$coords_longitude, plots$admin_municipality, plots$admin_province)
  stopCluster(cl)
  rm(lat)
  rm(long)
  
  # 
  # 
  # print("save_1956_rasters")
  # plots <- read_parcels()
  # cl <- makeCluster(detectCores())
  # clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/create_map.R"), source("./classificador_vol_america/scripts/export_rasters.R")))
  # clusterExport(cl, "plots", envir = environment())
  # clusterMap(cl, create_export_ortofoto_raster, plots$plot_id, plots$coords_latitude, plots$coords_longitude, plots$admin_municipality, plots$admin_province)
  # #mcmapply(create_export_map, plots$plot_id, plots$coords_latitude, plots$coords_longitude, plots$admin_municipality, plots$admin_province)
  # stopCluster(cl)
}

smoothen_rasters_all <- function(){
  #ids <- get_raster_ids_done_not_smoothen()
  ids <- get_exported_ids()
  sapply(ids, smoothen_raster)
}

vectorise_rasters_all <- function(){
  #ids <- get_raster_ids_smoothen_done()
  ids <- get_smoothen_ids()
  
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

calc_metrics_all <- function(){
  elev <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Elevacions/elevacions_CAT.tif")
  pend <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Elevacions/pendent_CAT.tif")
  clima.mean_temp <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_TMPANUAL/ATMOSFERA_ATLES6190_TMPANUAL_5mx5m.tif")
  clima.amp_term <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_AMPTERMI/ATMOSFERA_ATLES6190_AMPTERMI_5mx5m.tif")
  clima.mean_prec <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_PPTANUAL/ATMOSFERA_ATLES6190_PPTANUAL_5mx5m.tif")
  clima.reg_pluv <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_REGPLUVI/ATMOSFERA_ATLES6190_REGPLUVI_5mx5m.tif")
  
  #ids <- get_vectors_clumped_file_ids_not_metrics()
  ids <- get_clumped_ids()
  
  print(ids)
  
  cl <- makeCluster(5, outfile="log.txt")
  clusterExport(cl, c("ids", "calc_metrics", "elev", "pend", "clima.mean_temp","clima.amp_term","clima.mean_prec","clima.reg_pluv"), envir = environment())
  clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/calc_metrics.R"), library(raster), library(terra), library(rgdal)))
  parLapply(cl, ids, calc_metrics, elev, pend, clima.mean_temp,clima.amp_term,clima.mean_prec,clima.reg_pluv)
  stopCluster(cl)
}