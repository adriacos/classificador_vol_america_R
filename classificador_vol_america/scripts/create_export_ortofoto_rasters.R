
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
  
  #check if there are holes
  if(max(table(values(rast))/length(rast)) > 0.10){
    print(paste("Map with id ", id, " was corrupted at extraction", sep=""))
    save_id_corrupted(id)
    return(NULL)
  }
  
  rast <- project_EPSG_4258_rast(rast)
  #rast <- reproject_EPSG_25831_rast(rast)
  
  
  ext <- reproject_EPSG_4258_vect(get_quad_vect(id))
  buf <- buffer(ext, width=150, dissolve=T)
  rast <- cut_ortofoto_raster(rast, buf)
  save_ortofoto_raster_buffer(rast, id)                            
  rast <- cut_ortofoto_raster(rast, ext)                            
  save_ortofoto_raster_tight(rast, id)
  
  #rsat <- cut_ortofoto_raster(rast, vect)
  #save_ortofoto_raster_buffer(rast, id)
  save_id_exported(id)
  rast
}

save_ortofoto_raster_tight <- function(rast, id){
  writeRaster(rast, paste("./classificador_vol_america/rasters/all/", id, ".tif", sep=""), format="GTiff", overwrite=TRUE)
}

save_ortofoto_raster_buffer <- function(rast, id){
  writeRaster(rast, paste("./classificador_vol_america/rasters/exported/", id, ".tif", sep=""), format="GTiff", overwrite=TRUE)
}


cut_ortofoto_raster <- function(rast, vect){
  rast <- crop(rast, vect)
  rast <- mask(rast, vect)
  rast
}