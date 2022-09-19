

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
library(rgeos)
library(stringr)
library(maptools)
#library(foreach)
#library(doParallel)
#library(parallel)
library(future)

#cores <- detectCores()
#registerDoParallel(cores)
plan(cluster)

vect <- readOGR(dsn = "./vect", layer = "vect_10_corr")
#!
neighbours <- gTouches(vect, returnDense=FALSE, byid=TRUE)
neighbours <- sapply(neighbours,paste,collapse=",")
vect$neighbors <- neighbours
rm(neighbours)
vect$DN <- as.numeric(vect$DN)
vect$area <- abs(vect$area)
#vect$intArea <- abs(vect$intArea)

#ids <- 1:400
#test <- vect[(vect$fid %in% ids),]
test <- vect
#test$dissolve <- 0

c <- 1
time <- Sys.time()
repeat{
  time <- Sys.time()
  
  #!
  test.min <- test[which.min(test$area),]
  if(test.min$area >= 60000){
    break
  }
  test.min.neighbors <- test[test$fid %in% str_split(test.min$neighbors, ",")[[1]],]
  test.min.neighbors.min <- test.min.neighbors[which.min(test.min.neighbors$area),]
  
  #test$dissolve <- test$fid
  #test$dissolve <- factor(test$dissolve, levels=unique(test$fid))
  if(nrow(test.min) > 1){
    print("ALARM! test.min > 1")
    stop()
  }
  if(nrow(test.min.neighbors.min) > 1){
    print("ALARM! test.min.neighbors.min > 1")
    stop()
  }
  ids <- c(toString(test.min$fid), toString(test.min.neighbors.min$fid))
  if(length(ids) > 2){
    print("ALARM! ids > 2")
    stop()
  }
  
  #rest_f <- future({
  #  #!  
  #  test[!(test$fid %in% ids),]
  #})
  
  
  #test[test$fid %in% ids,"dissolve"] <- ids[1]
  #test.dissolve <- test$dissolve
  test.toagg <- test[test$fid %in% ids,]
  
  
  #test.toagg$dissolve <- 1
  test.df <- as(test.toagg,"data.frame")
  
  ##test.union <- unionSpatialPolygons(test, test$dissolve)  
  ##test.df.agg <- aggregate(test.df[, c("DN", "area", "intArea","intValue")], list(test.dissolve), mean)
  
  #test.toagg$dissolve <- 1
  #union_f <- future({
  #!  
  
  test.union <- unionSpatialPolygons(test.toagg, c(row.names(test.df)[1],row.names(test.df)[1]))
  #})
  
  #df.agg_f <- future({
  #test.df.agg <- aggregate(test.df[, c("DN")], list(test.toagg$dissolve), mean)
  #})
  
  #neighbors <- paste(test.df[test$dissolve==ids[1],]$neighbors,collapse=",")
  
  neighbors <- paste(test.df$neighbors,collapse=",")
  neighbors <- unique(str_split(neighbors, ","))[[1]]
  neighbors <- neighbors[!(neighbors %in% ids)]
  neighbors <- paste(neighbors, collapse = ",")
  area <- sum(test.df$area)
  DN <- mean(test.df$DN)
  test.df <- test.df[1,]
  test.df$DN <- DN
  test.df$neighbors <- neighbors
  test.df$area <- area
  test.df$fid <- ids[1]
  #row.names(test.df) <- ids[1]
  
  #test.union <- value(union_f)
  #test.df.agg <- value(df.agg_f)
  
  ##aggregate <- foreach (i=1:2, .packages = "rgeos") %dopar% {
  ##  if(i==1){
  ##    unionSpatialPolygons(test, test$dissolve)  
  ##    }else{
  ##      aggregate(test.df[, c("DN", "area", "intArea","intValue")], list(test.dissolve), mean)
  ##  }
  ##}
  ##test.union <- aggregate[[1]]  
  ##test.df.agg <- aggregate[[2]]
  
  #row.names(test.df.agg) <- as.character(test.df.agg$Group.1)
  #colnames(test.df.agg)[1] <- "fid"
  
  #test.df.agg$neighbors <- neighbors
  neighbors <- NULL
  area <- NULL
  DN <- NULL
  #test.df.agg$dissolve <- 0
  #test.df.agg$dissolved <- 0
  #test.df.agg$neighbors <- test$neighbors[test$fid!=ids[2]]
  #test.df.agg$neighbors[test.df.agg$fid==ids[1]] <- neighbors
  
  #test.shp.agg <- SpatialPolygonsDataFrame(test.union, test.df.agg)
  test.shp.agg <- SpatialPolygonsDataFrame(test.union, test.df)
  
  
  
  #!
  #test.rest <- value(rest_f)
  test.rest <- test[!(test$fid %in% ids),]
  
  #!
  #test.join <- rbind(test.rest, test.shp.agg)
  
  #test <- test.join
  test <- rbind(test.rest, test.shp.agg)
  
  test.rest <- NULL
  test.shp.agg <- NULL
  test.df.agg <- NULL
  test.df <- NULL
  test.union <- NULL
  test.toagg <- NULL
  test.join <- NULL
  #test.dissolve <- NULL
  test.min.neighbors.min <- NULL
  test.min.neighbors <- NULL
  test.min <- NULL
  neighbors <- NULL
  #print(nrow(test))
  c <- c+1
  #if(c%%10==0){
  print(paste(Sys.time()-time, c, nrow(test), sep="-"))
  #print(c)
  #}
  if(c%%100==0){
    writeOGR(vect, "./bkp", "smoothen_bkp", driver = "ESRI Shapefile")
    break
  }
}

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



fid DN      area   intArea intValue dissolved                                     neighbors dissolve
0   1  3  16675.65  16675.65        3         0                                     336,46245        0
1   2  3  50026.97  50026.97        3         0 3,4,229,230,336,45912,45913,46138,46139,46245        0
2   3  5  16675.65  16675.65        5         0                               2,4,45911,45913        0
3   4  7  33351.31  33351.31        7         0 2,3,229,230,831,45911,45912,46138,46139,46740        0
4   5  4 116729.60 116729.60        4         0                   6,831,834,45915,46740,46743        0
5   6  7  50026.98  50026.98        7         0                     5,7,834,45914,45916,46743        0

fid  DN     area  intArea intValue
1   1 5.5 258505.3 258505.3      5.5
neighbors
1 37978,38118,38120,38380,38460,38529,38601,38602,38671,38911,38925,83887,84027,84029,84289,84369,84438,84510,84511,84820,84834,38461,38925,84834
dissolve
1        0
