

cut_ortofoto_by_quad <- function(){
  raster <- raster("./classificador_vol_america/rasters/pnoa/BCN_1km.tif")
  quads <- get_quad_vect_3km()
  ids <- sub(".tif","",list.files("./classificador_vol_america/rasters/pnoa/cuts"))
  for(iquad in 1:nrow(quads)){
    quad <- quads[iquad,]
    id <- quad$id
    if(id%in%ids){
      next()
    }
    print(id)
    quad <- buffer(quad, width=100, dissolve=T)
    rast <- crop(raster,quad)
    location1 <- rast@file@name
    rast <- mask(rast,quad)
    location <- rast@file@name
    writeRaster(rast,paste("./classificador_vol_america/rasters/pnoa/cuts/",id,".tif",sep=""))
    lapply(list.files(dirname(location)), function(file,dir){
      unlink(paste(dir,"/",file,sep=""))
    },dirname(location))
  }
}