
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


save_1956_ortofotos_to_rasters_all <- function(){
  print("save_1956_rasters")
  plots <- read_parcels()
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/create_map.R"), source("./classificador_vol_america/scripts/export_rasters.R")))
  clusterExport(cl, "plots", envir = environment())
  clusterMap(cl, create_export_ortofoto_raster, plots$plot_id, plots$coords_latitude, plots$coords_longitude, plots$admin_municipality, plots$admin_province)
  #mcmapply(create_export_map, plots$plot_id, plots$coords_latitude, plots$coords_longitude, plots$admin_municipality, plots$admin_province)
  stopCluster(cl)
}

smoothen_rasters_all <- function(){
  files <- get_raster_names_done_not_smoothen()
  sapply(files, smoothen_raster)
}

vectorise_rasters_all <- function(){
  names <- get_raster_names_smoothen_done()
  cl <- makeCluster(5, outfile="log.txt")
  clusterExport(cl, c("names", "vectorise_save_smoothen_raster"), envir = environment())
  clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/vectorise_raster.R"), library(raster), library(terra), library(rgdal)))
  parLapply(cl, names, vectorise_save_smoothen_raster)
  #vectorise_raster(rast)
  stopCluster(cl)
}

clump_vectors_all <- function(){
    names <- get_names_smoothen_vectorised_not_clumped()
  print(names)
  names <- split(names, ceiling(seq_along(names)/5))
  for(names_5 in names){
    cl <- makeCluster(5, outfile="log.txt")
    clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/clump_vector.R"), library(rgdal), library(rgeos), library(stringr), library(maptools)))
    clusterExport(cl, c("names_5", "clump_vector"), envir = environment())
    clusterMap(cl, clump_vector, names_5)
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
  
  names <- get_vectors_clumped_file_names_not_metrics()
  
  print(names)
  
  cl <- makeCluster(5, outfile="log.txt")
  clusterExport(cl, c("names", "calc_metrics", "elev", "pend", "clima.mean_temp","clima.amp_term","clima.mean_prec","clima.reg_pluv"), envir = environment())
  clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/calc_metrics.R"), library(raster), library(terra), library(rgdal)))
  parLapply(cl, names, calc_metrics, elev, pend, clima.mean_temp,clima.amp_term,clima.mean_prec,clima.reg_pluv)
  stopCluster(cl)
}