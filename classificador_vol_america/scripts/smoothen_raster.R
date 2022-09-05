

smoothen <- function(rast){
  library(raster)
  library(SpaDES)
  raster_split <- splitRaster(rast, 10,10, buffer=c(2,2))
  for(i in 1:10){
    print(paste("start ", i, Sys.time()))
    raster_split <- lapply(raster_split,test2)
    rast <- mergeRaster(raster_split)
    writeRaster(rast,paste("classificador_vol_america/raster_recl_0.15_",i,".tif"), overwrite=TRUE)
    print(paste("end ", i, Sys.time()))
  }
  rast
  #raster_recl <- mergeRaster(raster_split)
  #greyscale <- grey(seq(0, 1, length = 256))
}

writeOGR(vect, ".", "vectorised", driver = "ESRI Shapefile")


vect <- sf::as_Spatial(
                sf::st_as_sf(
                    stars::st_as_stars(rast), as_points = FALSE, merge = FALSE, precision=1000))

vect <- st_as_stars(rast) %>% 
  st_as_sf(merge = TRUE) %>% # this is the raster to polygons part
  st_cast("MULTILINESTRING") # cast the polygons to polylines



test2 <- function(rast){
  test <- function(x, rast){
    adj <- adjacent(rast, x, 8, include=TRUE)
    m <- mean(rast[adj[,2]][rast[adj[,2]]>rast[x]-0.15&rast[adj[,2]]<rast[x]+0.15], na.rm=TRUE)
    m
  }
  
  cells <- cellFromRow(rast, c(1:nrow(rast)))
  values(rast) <- sapply(cells, test, rast=rast)
  rast
}


test <- function(x, rast){
  adj <- adjacent(rast, x, 8, include=TRUE)
  m <- mean(rast[adj[,2]][rast[adj[,2]]>rast[x]-0.15&rast[adj[,2]]<rast[x]+0.15], na.rm=TRUE)
  m
}


test3 <- function(){
  r <- rast
  r<- focal(r, w=matrix(1/9,nrow=3,ncol=3)) 
  r <- reclassify(r, reclass_m5)
  r<- focal(r, w=matrix(1/25,nrow=5,ncol=5)) 
  r <- reclassify(r, reclass_m5)
  r<- focal(r, w=matrix(1/25,nrow=5,ncol=5)) 
  r <- reclassify(r, reclass_m5)
  r<- focal(r, w=matrix(1/9,nrow=3,ncol=3)) 
  r <- reclassify(r, reclass_m5)
  
  plot(r, col=grey_scale(6))
}

