library(raster)
library(sf)
# library(exactextractr)

files <- list.files("C:/Users/a.cos/OneDrive - CREAF/classificador/rasters/pnoa/LLE",pattern="\\.tif$",full.names=T)
files <- append(files,list.files("C:/Users/a.cos/OneDrive - CREAF/classificador/rasters/pnoa/TAR",pattern="\\.tif$",full.names=T))
files <- append(files,list.files("C:/Users/a.cos/OneDrive - CREAF/classificador/rasters/pnoa/GIR",pattern="\\.tif$",full.names=T))

polygons <- st_read("./classificador_vol_america/entrenament_extra/poligons_entrenament_extra.gpkg")
# st_write(polygons,"./classificador_vol_america/entrenament_extra/poligons_entrenament_extra_id.gpkg")
polygons$id <- 1:nrow(polygons)
lapply(1:nrow(polygons),function(r,polygons,files){
  p <- polygons[r,]
  crop <- lapply(files,function(f,p){
    rast <- raster(f)
    if(!is.null(intersect(extent(rast),p))){
      print(f)
      rast <- crop(rast,p)
      rast <- mask(rast,p)
      return(rast)
    }else{
      return(NULL)
    }
  },p)
  crop <- crop[!sapply(crop,is.null)]
  if(length(crop)>1){
      merged <- crop[[1]]
      for(i in 2:length(crop)){
        r2 <- crop[[i]]
        merged <- merge(merged, r2)
        rm(r2)
      }
      crop <- merged
      rm(merged)
  }else{
    crop <- crop[[1]]
  }
  values(crop) <- values(crop)*10/255
  writeRaster(crop,paste("./classificador_vol_america/entrenament_extra/",p$id,".tif",sep=""))
},polygons,files)

