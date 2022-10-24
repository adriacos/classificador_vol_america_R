library(terra)
library(raster)
library(rgdal)
library(plyr)

vectorise_raster_by_url <- function(url){
  print(url)
  rast <- raster(url)
  vectorise_raster(rast)
}

vectorise_raster <- function(rast){
  #to tera::spatRaster
  values(rast) <- round_any(values(rast), 0.5)
  rast <- rast(rast)
  vect <- as.polygons(rast, dissolve=T, trunc=F)
  vect <- disagg(vect)
  #area <- expanse(vect)
  #to sp: spatialPolygonsDataFrame
  vect <- as(vect, "Spatial")
  #names(vect)[1] <- "DN"
  #vect$area <- area
  vect
}

vectorise_save_smoothen_raster <- function(id){
  print("vectorise_save_smoothen_raster")
  file <- paste("./classificador_vol_america/rasters/smoothen/", id, "_smth.tif", sep="")
  vect <- vectorise_raster_by_url(file)
  save_vectorised_raster(vect, id)
  file.remove(paste("./classificador_vol_america/rasters/smoothen/", id, "_smth.tif", sep=""))
  file.remove(paste("./classificador_vol_america/rasters/smoothen/", id, "_smth.tif.aux", sep=""))
} 

save_vectorised_raster <- function(vect, name){
  writeOGR(vect, "./classificador_vol_america/vect/vectorised", name, driver = "ESRI Shapefile", overwrite_layer = TRUE) 
}

vectorise_raster_ <- function(rast){
  
}
