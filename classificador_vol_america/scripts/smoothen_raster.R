

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

vect <- readOGR(dsn = "./classificador_vol_america/vect", layer = "vect_10_corr")
#!
neighbours <- gTouches(vect, returnDense=FALSE, byid=TRUE)
neighbours <- sapply(neighbours,paste,collapse=",")
vect$neighbors <- neighbours
rm(neighbours)
vect$DN <- as.numeric(vect$DN)
vect$area <- abs(vect$area)


test <- vect
test$ids_merged <- NA

c <- 1
time <- Sys.time()
repeat{
  
  test.min <- test[which.min(test$area),]

  if(test.min$area >= 60000){
    break
  }
  
  test.min.neighbors <- test[test$fid %in% str_split(test.min$neighbors, ",")[[1]],]
  test.min.neighbors.min <- test.min.neighbors[which.min(test.min.neighbors$area),]
  

  
  if(nrow(test.min.neighbors)==0){

    merged <- test[!is.na(test$ids_merged),]
    m <- str_split(merged$ids_merged, ",")
    n <- str_split(test.min$neighbors, ",")[[1]]
    s <- sapply(m,function(m){length(intersect(m,n))})
    test.min.neighbors <- merged[s==1,]
    test.min.neighbors.min <- test.min.neighbors[which.min(test.min.neighbors$area),]
    
    merged <- NULL
    
    if(nrow(test.min.neighbors)==0){
      print(paste(c, "NO NEIGHBORS", test.min$fid, sep="-"))
      break
    }
  }
  
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
  
  test.toagg <- test[test$fid %in% ids,]
  
  test.df <- as(test.toagg,"data.frame")
  
  test.union <- unionSpatialPolygons(test.toagg, c(row.names(test.df)[1],row.names(test.df)[1]))

  neighbors <- paste(test.df$neighbors,collapse=",")
  neighbors <- unique(str_split(neighbors, ","))[[1]]
  neighbors <- neighbors[!(neighbors %in% ids)]
  neighbors <- paste(neighbors, collapse = ",")
  area <- sum(test.df$area)
  DN <- mean(test.df$DN)
  ids_merged <- paste(test.df$ids_merged[!is.na(test.df$ids_merged)],collapse=",")
  if(ids_merged==""){
    ids_merged <- NA
  }
  test.df <- test.df[1,]
  test.df$DN <- DN
  test.df$neighbors <- neighbors
  test.df$area <- area
  test.df$fid <- ids[1]
  
  if(is.na(ids_merged)){
    test.df$ids_merged <- ids[2]
  } else{
    test.df$ids_merged <- paste(ids_merged,ids[2],sep = ",")
  }
  
  neighbors <- NULL
  area <- NULL
  DN <- NULL
  ids_merged <- NULL
 
  test.shp.agg <- SpatialPolygonsDataFrame(test.union, test.df)
  
  test.rest <- test[!(test$fid %in% ids),]
 
  test <- rbind(test.rest, test.shp.agg)

  test.rest <- NULL
  test.shp.agg <- NULL
  test.df.agg <- NULL
  test.df <- NULL
  test.union <- NULL
  test.toagg <- NULL
  test.join <- NULL
  test.min.neighbors.min <- NULL
  test.min.neighbors <- NULL
  test.min <- NULL
  neighbors <- NULL

  c <- c+1
  if(c%%50==0){
    print(paste(as.numeric(difftime(Sys.time(),time,units="secs")), c, nrow(test), sep="-"))
    time <- Sys.time()
  }
  if(c%%500==0){
    if(!is.null(test.excl)){
      test.towrite <- rbind(test,test.excl)
      test.towrite$area <- round(test.towrite$area,2)
      writeOGR(test.towrite, "./classificador_vol_america/vect/bkp", "smoothen_bkp", driver = "ESRI Shapefile", overwrite_layer = TRUE)  
      rm(test.towrite)
    }else{
      test.towrite <- test
      test.towrite$area <- round(test.towrite$area,2)
      writeOGR(test.towrite, "./classificador_vol_america/vect/bkp", "smoothen_bkp", driver = "ESRI Shapefile", overwrite_layer = TRUE)
      rm(test.towrite)
    }
  }
}

test.towrite <- rbind(test,test.excl)
test.towrite$area <- round(test.towrite$area,2)
writeOGR(test.towrite, "./classificador_vol_america/vect", "smoothen", driver = "ESRI Shapefile", overwrite_layer = TRUE)  
rm(test.towrite)


45965


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


