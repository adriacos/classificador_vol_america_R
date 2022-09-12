

smoothen <- function(rast){
  library(raster)
  library(SpaDES)library(rgdal)
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


library(rgdal)
vect <- readOGR(dsn = "./vect", layer = "vect_10_corr")
neighbours <- gTouches(vect, returnDense=FALSE, byid=TRUE)
neighbours <- sapply(neighbours,paste,collapse=",")
vect$neighbors <- neighbours
 


for(i in 1:nrow(vect)){
  print(vect[i,]$fid)
}

nrow(vect[c(1,2),])

test <- vect[(vect$fid==1|vect$fid==985),]

test2 <- aggregate(test,dissolve=T,areaWeighted=T,by=list(test$area,test$DN),mean)


    fid DN      area   intArea intValue dissolved
0     1  3  16675.65  16675.65        3         0
5     6  7  50026.98  50026.98        7         0
541 542  6  50027.08  50027.08        6         0
984 985  5 166757.36 166757.36        5         0

    Group.1 Group.2 fid DN      area   intArea intValue dissolved neighbors
1  16675.65       3   1 NA  16675.65  16675.65        3        NA        NA
2 166757.36       5 985 NA 166757.36 166757.36        5        NA        NA


library(stringr)

ids <- c(1,542)
test$dissolve <- 0
test$dissolve <- factor(test$dissolve, levels=c(0,1))
test[test$fid==ids,"dissolve"] <- 1

test.dissolve <- test$dissolve
test.union <- unionSpatialPolygons(test, test$dissolve)
test.df <- as(test,"data.frame")
neighbors <- paste(test.df[test$dissolve==1,]$neighbors,collapse=",")
neighbors <- unique(str_split(neighbors, ","))

test.df.agg <- aggregate(test.df[, 2:5], list(test.id), mean)




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

