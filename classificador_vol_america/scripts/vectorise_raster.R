library(terra)
library(raster)
library(rgdal)

vectorise_raster_by_url <- function(url){
  print(url)
  rast <- raster(url)
  vectorise_raster(rast)
}

vectorise_raster <- function(rast){
  #to tera::spatRaster
  rast <- rast(rat)
  vect <- as.polygons(rast, dissolve=T)
  vect <- disagg(vect)
  area <- expanse(vect)
  #to sp: spatialPolygonsDataFrame
  vect <- as(vect, "Spatial")
  names(vect)[1] <- "DN"
  vect$area <- area
  vect
}

vectorise_save_smoothen_raster <- function(name){
  file <- paste("./classificador_vol_america/rasters/smoothen/", name, "_smth.tif", sep="")
  vect <- vectorise_raster_by_url(file)
  save_vectorised_raster(vect, name)
} 

save_vectorised_raster <- function(vect, name){
  writeOGR(vect, "./classificador_vol_america/vect/vectorised", name, driver = "ESRI Shapefile", overwrite_layer = TRUE) 
}