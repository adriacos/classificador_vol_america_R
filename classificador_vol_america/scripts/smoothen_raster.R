

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


ids <- 1:400
test <- vect[(vect$fid %in% ids),]


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
neighbours <- gTouches(vect, returnDense=FALSE, byid=TRUE)
neighbours <- sapply(neighbours,paste,collapse=",")
vect$neighbors <- neighbours
vect$DN <- as.numeric(vect$DN)
vect$area <- abs(vect$area)
vect$intArea <- abs(vect$intArea)

#ids <- 1:400
#test <- vect[(vect$fid %in% ids),]
test <- vect

c <- 1
time <- Sys.time()
repeat{
  time <- Sys.time()
  if(c>=1000){
    break
  }
  test.min <- test[which.min(test$area),]
  if(test.min$area >= 60000){
    break
  }
  test.min.neighbors <- test[test$fid %in% str_split(test.min$neighbors, ",")[[1]],]
  test.min.neighbors.min <- test.min.neighbors[which.min(test.min.neighbors$area),]
  
  test$dissolve <- test$fid
  test$dissolve <- factor(test$dissolve, levels=unique(test$fid))
  
  ids <- c(toString(test.min$fid), toString(test.min.neighbors.min$fid))
  test[test$fid %in% ids,"dissolve"] <- ids[1]
  test.dissolve <- test$dissolve
  
  
  test.df <- as(test,"data.frame")
  
  #test.union <- unionSpatialPolygons(test, test$dissolve)  
  #test.df.agg <- aggregate(test.df[, c("DN", "area", "intArea","intValue")], list(test.dissolve), mean)
  
  union_f <- future({
    unionSpatialPolygons(test, test$dissolve)
  })
  
  df.agg_f <- future({
    aggregate(test.df[, c("DN", "area", "intArea","intValue")], list(test.dissolve), mean)
  })
  
  neighbors <- paste(test.df[test$dissolve==ids[1],]$neighbors,collapse=",")
  neighbors <- unique(str_split(neighbors, ","))[[1]]
  neighbors <- neighbors[!(neighbors %in% ids)]
  neighbors <- paste(neighbors, collapse = ",")
  
  
  test.union <- value(union_f)
  test.df.agg <- value(df.agg_f)
  
  #aggregate <- foreach (i=1:2, .packages = "rgeos") %dopar% {
  #  if(i==1){
  #    unionSpatialPolygons(test, test$dissolve)  
  #    }else{
  #      aggregate(test.df[, c("DN", "area", "intArea","intValue")], list(test.dissolve), mean)
  #  }
  #}
  #test.union <- aggregate[[1]]  
  #test.df.agg <- aggregate[[2]]
  
  row.names(test.df.agg) <- as.character(test.df.agg$Group.1)
  colnames(test.df.agg)[1] <- "fid"
  test.df.agg$neighbors <- test$neighbors[test$fid!=ids[2]]
  test.df.agg$neighbors[test.df.agg$fid==ids[1]] <- neighbors

  test.shp.agg <- SpatialPolygonsDataFrame(test.union, test.df.agg)
  
  test <- test.shp.agg
  test.shp.agg <- NULL
  test.df.agg <- NULL
  test.df <- NULL
  test.union <- NULL
  test.dissolve <- NULL
  test.min.neighbors.min <- NULL
  test.min.neighbors <- NULL
  test.min <- NULL
  #print(nrow(test))
  c <- c+1
  #if(c%%10==0){
    print(paste(Sys.time()-time, c,nrow(test), sep="-"))
    #print(c)
  #}
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

