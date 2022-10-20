source("./classificador_vol_america/scripts/read_data.R")
source("./classificador_vol_america/scripts/create_1956_class_diba_leaflet.R")
source("./classificador_vol_america/scripts/export_rasters.R")
library(raster)

create_export_1956_class_diba_leaflet <- function(id, lat, lng){

  print("create_export_1956_class_diba_leaflet")
  print(id)
  map <- create_1956_class_diba_leaflet(lat, lng)
  rast <- export_map(map, id)
  rast <- project_EPSG_4258_rast(rast)
  # rast <- project_EPSG_25831_rast(rast)
  # rast <- reproject_EPSG_25831_rast(rast)
  rast <- cut_raster(rast, reproject_EPSG_4258_vect(get_quad_vect(id)))
  #rast <- cut_raster(rast, get_quad_vect(id))
  save_1956_diba_map(rast, id)
  rast
}

cut_raster <- function(rast, vect){
  rast <- crop(rast, vect)
  rast <- mask(rast, vect)
  rast
}