library(supercells)
library(raster)
library(terra)
library(sf)
library(stars)
library(exactextractr)

vectorise_raster_supercells <- function(id){
  
  print(paste("vectorise_raster_supercells", id, Sys.time(),sep="-"))
  # dir <- paste("./classificador_vol_america/rasters/smoothen/",id, sep="")
  rast <- raster(paste("./classificador_vol_america/rasters/exported/", id, ".tif", sep=""))
  rast_sc <- rast(rast)
  rast_sc = supercells(rast_sc, k = 15625, compactness = 0.5)
  vect <- as(vect, "Spatial")
  
  
  
  writeOGR(vect, "./classificador_vol_america/rasters", paste(id,"_sc",sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE)
}

# rast <- raster("./classificador_vol_america/rasters/exported/5671.tif")

vectorise_raster_supercells_global <- function(){
  rast <- rast(raster)
  # area <- expanse(rast)
  area <- 9610000
  rast_sc = supercells(rast, k = 4*area/500, compactness = 0.5)
  rm(area)
  # plot(rast)
  # plot(st_geometry(rast_sc), add = TRUE, lwd = 0.2)
  ex <- exact_extract(rast, rast_sc, "mean")
  rast_sc$value <- ex
  rm(ex)
  rast_sc <- st_rasterize(rast_sc[,"value"])
  rast_sc <- rast(rast_sc)
  rast_sc <- raster(rast_sc)
  rast_sc
  # # writeRaster(rast_sc,"./classificador_vol_america/rasters/rast_sc.tif",overwrite=T)
  # r <- rast_sc
  # for(i in 1:6){
  #   print(paste("start ", i, Sys.time()))
  #   r <- smoothen_raster_(r)
  # }
  # values(r) <- values(r)*10
  # values(r) <- round_any(values(r), 0.5)
  # writeRaster(r,"./classificador_vol_america/rasters/test.tif",overwrite=T)
  
}


