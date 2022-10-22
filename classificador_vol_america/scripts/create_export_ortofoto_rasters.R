
source("./classificador_vol_america/scripts/read_data.R")
source("./classificador_vol_america/scripts/create_ortofoto_leaflet.R")
source("./classificador_vol_america/scripts/export_rasters.R")
source("./classificador_vol_america/scripts/save_data.R")

library(raster)

create_export_ortofoto_raster <- function(id, lat, lng, vect){
  print("create_export_map")
  print(id)
  map <- create_ortofoto_leaflet(lat, lng)
  rast <- export_map(map, id)
  rast <- project_EPSG_4258_rast(rast)
  #rast <- reproject_EPSG_25831_rast(rast)
  rast <- cut_ortofoto_raster(rast, buffer(reproject_EPSG_4258_vect(get_quad_vect(id)), width=200, dissolve=T))
  #rsat <- cut_ortofoto_raster(rast, vect)
  save_map(rast, id)
  save_id_exported(id)
  rast
}

cut_ortofoto_raster <- function(rast, vect){
  rast <- crop(rast, vect)
  rast <- mask(rast, vect)
  rast
}