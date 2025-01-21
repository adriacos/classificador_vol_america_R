library(sf)

data.parcela <- read.csv("./data_metr_parcela.csv")
data.parcela$X <- NULL


camins <- st_read("./camins_carreteres.gpkg")[,1]
st_crs(camins) <- 4326

gc()

# st_write(data.parcela.gis,"./gis/parceles.gpkg",append=F)
data.parcela.gis.buffer <- st_buffer(st_as_sf(data.parcela,
                                              coords=c("utm_x_fus30", "utm_y_fus30"),
                                              #coords=c("utm_x", "utm_y"),
                                              crs=25830),200)

camins <- st_transform(camins,st_crs(data.parcela.gis.buffer))
# data.parcela.gis.buffer <- st_transform(data.parcela.gis.buffer,st_crs(camins))

grid <- st_as_sf(st_make_grid(st_as_sfc(st_bbox(data.parcela.gis.buffer)),n=c(20,20)))
grid$id <- 1:nrow(grid)

data.parcela.gis <- do.call(rbind,lapply(grid$id,function(id,grid,camins,data.parcela.gis.buffer){
  g <- grid[grid$id==id,]
  print(g$id)
  data.parcela.gis.buffer_grid <- data.parcela.gis.buffer[st_intersects(data.parcela.gis.buffer,g) %>% lengths > 0,]
  if(nrow(data.parcela.gis.buffer_grid)==0){
    return(NULL)
  }else{
    stop()
  }
  # g <- st_buffer(g,400)
  # camins_grid <- camins[st_intersects(camins,g) %>% lengths > 0,]
  # if(nrow(camins_grid)==0){
  #   return(NULL)
  # }else{
  #   stop()
  # }
  
  interstection <- st_intersection(camins,data.parcela.gis.buffer_grid)
  
  data.parcela.gis.buffer_grid$camins <-  
  data.parcela.gis.buffer_grid
},grid,camins,data.parcela.gis.buffer))


intersection <- st_intersection(camins,data.parcela.gis.buffer)

lapply(intersect,print)


