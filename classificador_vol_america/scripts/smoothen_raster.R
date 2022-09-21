

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
#vect$intArea <- abs(vect$intArea)

#ids <- 1:400
#test <- vect[(vect$fid %in% ids),]
test <- vect
#test$dissolve <- 0
#test.excl <- NULL
test$ids_merged <- NA

c <- 1
time <- Sys.time()
repeat{
  #time <- Sys.time()
  
  #!
  test.min <- test[which.min(test$area),]
  if(test.min$area >= 60000){
    break
  }
  
  test.min.neighbors <- test[test$fid %in% str_split(test.min$neighbors, ",")[[1]],]
  test.min.neighbors.min <- test.min.neighbors[which.min(test.min.neighbors$area),]
  
  
  if(nrow(test.min.neighbors)==0){
    print(paste(c, "NO NEIGHBORS found", test.min$fid, sep="-"))
    break
    
    merged <- test[!is.na(test$ids_merged),]
    
    test.min.neighbors <- merged[length(intesect(str_split(test$ids_merged, ","),str_split(test.min$neighbors, ",")[[1]]))>0,]
    test.min.neighbors.min <- test.min.neighbors[which.min(test.min.neighbors$area),]
    
    if(nrow(test.min.neighbors)==0){
      print(paste(c, "NO NEIGHBORS found", test.min$fid, sep="-"))
      break
    }
    print(paste(c, "NEIGHBORS FOUND, EVERYTHING ok", test.min$fid, sep="-"))
    
    #if(is.null(test.excl)){
    #  test.excl <- test.min
    #} else{
    #  test.excl <- rbind(test.excl, test.min)
    #}
    #test <- test[test$fid != test.min$fid,]
    #c <- c+1
    #next
  }
  
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
  
  #if(!any(test.min.neighbors$fid %in% test.excl$fid)){
  #  print("¡¡¡ALARM!!")
  #  break
  #}
  
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
  ids_merged <- test.df$ids_merged[1]
  test.df <- test.df[1,]
  test.df$DN <- DN
  test.df$neighbors <- neighbors
  test.df$area <- area
  test.df$fid <- ids[1]
  
  if(is.na(ids_merged)){
    test.df$ids_merged <- ids[2]
  } else{
    test.df$ids_merged <- append(ids_merged,ids[2])
  }
  
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
  ids_merged <- NULL
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
  #test.excl <- test.excl[test.excl$fid!=ids[2],]
  
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
  if(c%%20==0){
    print(paste(Sys.time()-time, c, nrow(test), sep="-"))
    time <- Sys.time()
  }
  if(c%%100==0){
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


