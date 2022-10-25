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
library(geosphere)

calc_metrics <- function(name, elev, pend, clima.mean_temp, clima.amp_term, clima.mean_prec, clima.reg_pluv){
  print("calc_metrics")
  print(Sys.time())
  vect <- readOGR(paste("./classificador_vol_america/vect/10km/clumped/", name, "_clmp.shp", sep=""))
  rast <- raster(paste("./classificador_vol_america/rasters/10km/", name, ".tif", sep=""))
  
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
  
  #vect$rug <- calc_ruggedness(rast, vect, vect$are)

  vect$tpi <- calc_TPI(rast, vect)
  
  vect$sqr <- calc_squaredness(vect)
  
  #vect$tri <- calc_TRI(rast, vect)

  #vect$rog <- calc_roughness(rast, vect)
  
  rm(rast)
  
  l <- lapply(vect$id, calc_neighbor_metrics_, vect)
  vect <- cbind(vect, do.call("rbind", l))
  
  l <- lapply(vect$id, calc_neighbor_metrics_2_, vect)
  vect <- cbind(vect, do.call("rbind", l))
  
  vect <- vect[,-which(names(vect)=="nbr")]
  
  save_metrics_vector(name, vect)
  print(Sys.time())
}

calc_neighbor_metrics_ <- function(id, vect){
  
  n_mn_mn <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$mn, na.rm=T)
  n_sd_mn <- sd(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$mn, na.rm=T)
 
  n_mn_mdn <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$mdn, na.rm=T)
  
  n_mn_std <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$std, na.rm=T)
  
  n_mn_tpi <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$tpi, na.rm=T)
  n_sd_tpi <- sd(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$tpi, na.rm=T)
  
  n_mn_slp <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$slp, na.rm=T)
  n_sd_slp <- sd(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$slp, na.rm=T)
  
  n_mn_shp <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$shp, na.rm=T)
  n_sd_shp <- sd(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$shp, na.rm=T)
  
  n_mn_sqr <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$sqr, na.rm=T)
  n_sd_sqr <- sd(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$sqr, na.rm=T)
  
  return(data.frame(t(unlist(list("n_mn_mn"=n_mn_mn,"n_sd_mn"=n_sd_mn, "n_mn_mdn"=n_mn_mdn, 
                           "n_mn_std"=n_mn_std, "n_mn_slp"=n_mn_slp, "n_mn_shp"=n_mn_shp, "n_std_shp"=n_std_shp, "n_mn_tpi"=n_mn_tpi)))))
}

calc_neighbor_metrics_2_ <- function(id, vect){
  
  n_mn_mn_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_mn_mn, na.rm=T)
  n_sd_mn_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_sd_mn, na.rm=T)
  
  n_mn_mdn_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_mn_mdn, na.rm=T)
  
  n_mn_std_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_mn_std, na.rm=T)
  
  n_mn_tpi_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_mn_tpi, na.rm=T)
  n_sd_tpi_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_sd_tpi, na.rm=T)
  
  n_mn_slp_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_mn_slp, na.rm=T)
  n_sd_slp_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_sd_slp, na.rm=T)
  
  n_mn_shp <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_mn_shp, na.rm=T)
  n_sd_shp_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_sd_shp, na.rm=T)
  
  n_mn_sqr <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_mn_sqr, na.rm=T)
  n_sd_sqr_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_sd_sqr, na.rm=T)
  
  return(data.frame(t(unlist(list("n_mn_mn"=n_mn_mn,"n_sd_mn"=n_sd_mn, "n_mn_mdn"=n_mn_mdn, 
                                  "n_mn_std"=n_mn_std, "n_mn_slp"=n_mn_slp, "n_mn_shp"=n_mn_shp, "n_std_shp"=n_std_shp, "n_mn_tpi"=n_mn_tpi)))))
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

calc_squaredness <- function(vect){
  #per calcular mètriques de com de quadrat és
  
  #ATENCIó: els marges de les quadrícules tenen valors no reals, caldria veure com solucionar-ho.
  
  #Possibles solucions:
  #- Ajuntar els quatre clumpeds del voltant, tornar a passar clump_vector (per, amb sort, ajuntar els polígons adjacents),
  #    calcular mètriques i tornar a tallar per extensió
  #- Ajuntar tots els clumpeds de CAT/BCN, tornar a passar clumped (però trigarà molt), calcular mètriques i, si cal, tornar a tallar
  #- Treure des d'un principi els rasters per quadrícules de 2kmx2km amb centre als centroides, calcular mètriques i retallar a 1x1km
  v <- vect
  sf <- st_as_sf(vect)
  smp <- st_simplify(sf, preserveTopology = T, dTolerance = 2)
  v <- as_Spatial(smp)
  
  vt <- vect(vect)
  vect$peri <- perim(vt)
  rm(vt)
  
  y <- fortify(v) 
  ag <- aggregate(y, by=list(id=y$id), FUN=length)
  for(i in ag$id){
    v[i,"vtx"] <- ag[ag$id==i,2] 
  }
  #v$sqr_a <- v$vtx/v$area
  sqr_p <- v$vtx/v$peri
}

calc_longness <- function(vect){
  calc_longest_line_within(vect)/vect$are
}
calc_longest_line_within <- function(vect){
  
  #Treure vèrtex del polígon
  #fer matriu de distàncies entre cada vèrtex
  #convertir relacions en línies
  #terra::relation -> quedar-nos només amb les línies que estiguin "within" el polígon
  #quedar-nos amb la més llarga
  
  
}
