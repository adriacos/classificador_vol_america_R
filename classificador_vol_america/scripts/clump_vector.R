
library(rgdal)
library(rgeos)
library(stringr)
library(maptools)
#library(foreach)
#library(doParallel)
#library(parallel)
#library(future)

#cores <- detectCores()
#registerDoParallel(cores)
#plan(cluster)

clump_vector <- function(filename="vect_10_corr"){
  vect <- readOGR(dsn = "./classificador_vol_america/vect", layer = filename)
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
    DN <- sum(test.df$DN*(test.df$area/area))
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
      test.towrite <- test
      test.towrite$area <- round(test.towrite$area,2)
      writeOGR(test.towrite, "./classificador_vol_america/vect/bkp", "smoothen_bkp", driver = "ESRI Shapefile", overwrite_layer = TRUE)
      rm(test.towrite)
    }
  }
  
  test.towrite <- test
  test.towrite$area <- round(test.towrite$area,2)
  writeOGR(test.towrite, "./classificador_vol_america/vect", "smoothen", driver = "ESRI Shapefile", overwrite_layer = TRUE)  
  rm(test.towrite)
}