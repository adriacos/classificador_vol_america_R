source("./classificador_vol_america/scripts/save_data.R")
source("./classificador_vol_america/scripts/smoothen_raster.R")

library(exactextractr)
library(raster)
library(rgeos)
library(terra)
library(stringr)
library(parallel)
library(rgdal)
library(remotes)
library(SpaDES)

calc_metrics <- function(name, elev, pend, clima.mean_temp, clima.amp_term, clima.mean_prec, clima.reg_pluv){
  print("calc_metrics")
  print(Sys.time())
  vect <- readOGR(paste("./classificador_vol_america/vect/clumped/", name, "_clmp.shp", sep=""))
  rast <- raster(paste("./classificador_vol_america/rasters/", name, ".tif", sep=""))
  
  vect$id <- as.numeric(row.names(vect))
  if(vect[1,]$id==0){
    vect$id <- vect$id +1
  }
  
  ex <- exact_extract(rast, vect, "stdev")
  vect$std <- ex
  #vect[is.na(vect$std),"std"] <- 0
  ex <- exact_extract(rast, vect, "mean")
  vect$mn <- ex
  #vect[is.na(vect$mn),"mn"] <- 0
  ex <- exact_extract(rast, vect, "median")
  vect$mdn <- ex
  #vect[is.na(vect$mdn),"mdn"] <- 0
  #rm(rast)
  rm(ex)
  gc()
  
  ex <- exact_extract(elev, vect, "mean")
  vect$elv <- ex
  #vect[is.na(vect$elv),"elv"] <- 0
  ex <- exact_extract(pend, vect, "mean")
  vect$slp <- ex
  #vect[is.na(vect$slp),"slp"] <- 0
  ex <- exact_extract(clima.mean_temp, vect, "mean")
  vect$mtp <- ex
  #vect[is.na(vect$mtp),"mtp"] <- 0
  ex <- exact_extract(clima.amp_term, vect, "mean")
  vect$apt <- ex
  #vect[is.na(vect$apt),"apt"] <- 0
  ex <- exact_extract(clima.mean_prec, vect, "mean")
  vect$mpt <- ex
  #vect[is.na(vect$mpt),"mpt"] <- 0
  ex <- exact_extract(clima.reg_pluv, vect, "mean")
  vect$rpl <- ex
  #vect[is.na(vect$rpl),"rpl"] <- 0
  
  rm(ex)
  
  neighbours <- gTouches(vect, returnDense=FALSE, byid=TRUE, )
  neighbours <- sapply(neighbours,paste,collapse=",")
  vect$nbr <- neighbours
  rm(neighbours)
  gc()
  
  #mirar si ho ha com a llista o vector o què
  coords <- getSpPPolygonsLabptSlots(vect)
  #coords <- coordinates(centr)
  vect$lat <- coords[,2]
  vect$lng <- coords[,1]
  rm(centr)
  rm(coords)
  
  sp_vect <- vect(vect)
  vect$are <- expanse(sp_vect)
  vect$per <- perim(sp_vect)
  vect$shp <- (2*pi*sqrt(vect$are))/vect$per
  rm(sp_vect)
  
  #vect$clp_16 <- calc_clumpiness_by_polygons_16(rast, vect)
  #vect$clp_8 <- calc_clumpiness_by_polygons_8(rast, vect)
  
  print(Sys.time())
  vect$rug <- calc_ruggedness(rast, vect, vect$are)
  print(Sys.time())
  vect$tpi <- calc_TPI(rast, vect)
  print(Sys.time())
  vect$tri <- calc_TPI(rast, vect)
  print(Sys.time())
  vect$rog <- calc_TPI(rast, vect)
  print(Sys.time())
  
  rm(rast)
  
  l <- lapply(vect$id, calc_neighbor_metrics_, vect)
  vect <- cbind(vect, do.call("rbind", l))
  vect <- vect[,-which(names(vect)=="nbr")]
  
  save_metrics_vector(name, vect)
  print(Sys.time())
}

calc_neighbor_metrics_ <- function(id, vect){
  
  n_mn_mn <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$mn, na.rm=T)
    
  n_sd_mn <- sd(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$mn, na.rm=T)
  #n_mxdf_mn <- max(abs(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$mn, na.rm=T) - min(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$mn, na.rm=T)
  
  n_mn_mdn <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$mdn, na.rm=T)
  
  n_mn_std <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$std, na.rm=T)
  
  n_mn_slp <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$slp, na.rm=T)
  n_std_slp <- sd(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$slp, na.rm=T)
  
  n_mn_shp <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$shp, na.rm=T)
  n_std_shp <- sd(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$shp, na.rm=T)
  
  return(data.frame(t(unlist(list("n_mn_mn"=n_mn_mn,"n_sd_mn"=n_sd_mn, "n_mn_mdn"=n_mn_mdn, 
                           "n_mn_std"=n_mn_std, "n_mn_slp"=n_mn_slp, "n_mn_shp"=n_mn_shp, "n_std_shp"=n_std_shp)))))
}

calc_ruggedness <- function(rast, vect, vect.area){
  count_classes_by_polygons(vect, rast, 0.09)/vect.area
}

calc_roughness <- function(rast, vect){
  calc_roughness_by_polygons(vect, rast)
}

calc_TPI <- function(rast, vect){
  calc_TPI_by_polygons(vect, rast)
}

calc_TRI <- function(rast, vect){
  calc_TRI_by_polygons(vect, rast)
}

# source("./classificador_vol_america/scripts/smoothen_raster.R")
# source("./classificador_vol_america/scripts/vectorise_raster.R")
# r_006 <- contrast_raster(rast, 0.06)
# v_006 <- vectorise_raster(r_006)
# library(sf)
# sf <- st_as_sf(v)
# sf_006 <- st_as_sf(v_006)
# int <- st_intersection(sf, sf_006)


