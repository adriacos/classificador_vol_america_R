load("./temp/current_global.RData")
print(paste(Sys.time()," continue"))
if(current_global=="set"){
  source("./scripts/smoothen_raster.R")
  smoothen_raster_ini()
}else if(current_global=="smoothen"){
  source("./scripts/smoothen_raster_2.R")
  smoothen_raster_2_ini()
}else if(current_global=="smoothen2"){
  source("./scripts/vectorise_raster.R")
  vectorise_raster_ini()
}else if(current_global=="vectorised"){
  source("./scripts/clump_vector.R")
  clump_vector_ini()
}else if(current_global=="clumped"){
  source("./scripts/calc_metrics.R")
  calc_metrics_ini()
  # clump_vector_ini()
}