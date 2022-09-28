
library(parallel)
source("./classificador_vol_america/scripts/read_data.R")
source("./classificador_vol_america/scripts/create_map.R")
source("./classificador_vol_america/scripts/export_rasters.R")

save_1956_rasters <- function(){
  print("save_1956_rasters")
  parcels <- read_parcels()
  cl <- makeCluster(detectCores())
  mcmapply(create_export_map, parcels$plot_id, parcels$coords_latitude, parcels$coords_longitude, parcels$admin_municipality, parcels$admin_province)
  stopCluster(cl)
}

create_export_map <- function(plot_id, lat, long, municipality, province){
  print("create_export_map")
  print(plot_id)
  map <- create_map(lat, long)
  export_map(map, plot_id, municipality, province)
}