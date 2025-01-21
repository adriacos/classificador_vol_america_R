library(raster)

files <- list.files("C:/Users/a.cos/OneDrive - CREAF/classificador/rasters/pnoa/LLE",pattern="\\.tif$")

lapply(files, function(f){
  rast <- raster(paste("C:/Users/a.cos/OneDrive - CREAF/classificador/rasters/pnoa/LLE/",f,sep=""))
  values(rast) <- values(rast)*10/255
  writeRaster(rast,paste("C:/Users/a.cos/OneDrive - CREAF/classificador/rasters/pnoa/LLE/r",f,sep=""))
})