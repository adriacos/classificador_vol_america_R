load("./classificador_vol_america/temp/current_global.RData")
print(paste(Sys.time()," continue"))
if(current_global=="set"){
  source("./classificador_vol_america/scripts/smoothen_raster.R")
  smoothen_raster_ini()
}else if(current_global=="smoothen"){
  source("./classificador_vol_america/scripts/smoothen_raster_2.R")
  smoothen_raster_2_ini()
}else if(current_global=="smoothen2"){
  source("./classificador_vol_america/scripts/vectorise_raster.R")
  vectorise_raster_ini()
}else if(current_global=="vectorised"){
  source("./classificador_vol_america/scripts/clump_vector.R")
  clump_vector_ini()
}else if(current_global=="clumped"){
  source("./classificador_vol_america/scripts/calc_metrics.R")
  calc_metrics_ini()
  # clump_vector_ini()
}