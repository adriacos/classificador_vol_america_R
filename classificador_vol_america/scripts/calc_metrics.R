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
library(sf)
library(ggplot2)

calc_metrics <- function(){
  print("calc_metrics")
  print(Sys.time())
  
  #afegir radiació
  elev <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Elevacions/elevacions_CAT.tif")
  pend <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Elevacions/pendent_CAT.tif")
  clima.mean_temp <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_TMPANUAL/ATMOSFERA_ATLES6190_TMPANUAL_5mx5m.tif")
  clima.amp_term <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_AMPTERMI/ATMOSFERA_ATLES6190_AMPTERMI_5mx5m.tif")
  clima.mean_prec <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_PPTANUAL/ATMOSFERA_ATLES6190_PPTANUAL_5mx5m.tif")
  #clima.reg_pluv <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_REGPLUVI/ATMOSFERA_ATLES6190_REGPLUVI_5mx5m.tif")

  if(file.exists("./classificador_vol_america/vect/classified/automatically/global_mtc.shp")){
    vect <- reproject_EPSG_4258_vect(readOGR(paste("./classificador_vol_america/vect/classified/automatically/global_mtc.shp", sep="")))
  }else{
    vect <- reproject_EPSG_4258_vect(readOGR(paste("./classificador_vol_america/vect/classified/automatically/global_nb.gpkg", sep="")))
  }
  rast <- raster(paste("./classificador_vol_america/rasters/all/global.tif", sep=""))
  
  if(!"mdn"%in%colnames(vect)){
    vect$id <- 1:nrow(vect)
    ex <- exact_extract(rast, vect, "stdev")
    vect$std <- ex
    #vect[is.na(vect$std),"std"] <- 0
    ex <- exact_extract(rast, vect, "mean")
    vect$mn <- ex
    #vect[is.na(vect$mn),"mn"] <- 0
    ex <- exact_extract(rast, vect, "median")
    vect$mdn <- ex
    #vect[is.na(vect$mdn),"mdn"] <- 0
    rm(ex)
    gc()
    writeOGR(vect, "./classificador_vol_america/vect/classified/automatically/", "global_mtc", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  }
  
  if(!"mpt"%in%colnames(vect)){
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
    # ex <- exact_extract(clima.reg_pluv, vect, "mean")
    # vect$rpl <- ex
    #vect[is.na(vect$rpl),"rpl"] <- 0
    rm(elev)
    rm(pend)
    rm(clima.mean_temp)
    rm(clima.amp_term)
    rm(clima.mean_prec)
    rm(ex)
    gc()
    writeOGR(vect, "./classificador_vol_america/vect/classified/automatically/", "global_mtc", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  }
 
  
   
  if(!"shp"%in%colnames(vect)){
    coords <- coordinates(vect)
    vect$lat <- coords[,2]
    vect$lng <- coords[,1]
    rm(coords)
    sp_vect <- vect(vect)
    vect$are <- expanse(sp_vect)
    vect$per <- perim(sp_vect)
    #mirar quin ens agrada més
    vect$shp <- (2*pi*sqrt(vect$are))/vect$per
    #vect$shp <- vect$per/sqrt(4*pi*vect$are)
    rm(sp_vect)
    gc()
    writeOGR(vect, "./classificador_vol_america/vect/classified/automatically/", "global_mtc", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  }
  
  if(!"tpi"%in%colnames(vect)){
    #vect$rug <- calc_ruggedness(rast, vect, vect$are)
    vect$tpi <- calc_TPI(rast, vect)
    writeOGR(vect, "./classificador_vol_america/vect/classified/automatically/", "global_mtc", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  }
  if(!"sqr"%in%colnames(vect)){
    vect$sqr <- calc_squaredness(vect)
    #vect$tri <- calc_TRI(rast, vect)
    #vect$rog <- calc_roughness(rast, vect)
    rm(rast)
    gc()
    writeOGR(vect, "./classificador_vol_america/vect/classified/automatically/", "global_mtc", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  }
  
  if(!"n_mn_mn_2"%in%colnames(vect)){
    neighbours <- gTouches(vect, returnDense=FALSE, byid=TRUE)
    neighbours <- sapply(neighbours,paste,collapse=",")
    vect$nbr <- neighbours
    rm(neighbours)
    gc()
    # cluster <- makeCluster(detectCores()-2, outfile="calc_neighbor_metrics_log.txt")
    # clusterExport(cluster, list("vect","calc_neighbor_metrics_"))
    l <- lapply(vect$id, calc_neighbor_metrics_, vect)
    # l <- parLapply(cluster, vect$id, calc_neighbor_metrics_, vect)
    # stopCluster()
    vect <- cbind(vect, do.call("rbind", l))
    l <- lapply(vect$id, calc_neighbor_metrics_2_, vect)
    vect <- cbind(vect, do.call("rbind", l))
    vect$nbr <- NULL
    rm(l)
    writeOGR(vect, "./classificador_vol_america/vect/classified/automatically/", "global_mtc", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  }
  save_metrics_vector(vect)
  print(Sys.time())
}

calc_neighbor_metrics_ <- function(id, vect){
  print(id)
  if(vect[vect$id==id,]$nbr==""){
    return(data.frame(t(unlist(list("n_mn_mn"=NA,"n_sd_mn"=NA, "n_mn_mdn"=NA, "n_pnt_m"=NA, "n_pnt_t"=NA,
                                    "n_nt_mn"=NA,"n_nt_tp"=NA,"n_sd_tpi"=NA,"n_sd_slp"=NA,"n_sd_shp"=NA,
                                    "n_mn_sqr"=NA,"n_sd_sqr"=NA,
                                    "n_mn_std"=NA, "n_mn_slp"=NA, "n_mn_shp"=NA, "n_mn_tpi"=NA)))))
  }
  
  n_mn_mn <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$mn, na.rm=T)
  n_sd_mn <- sd(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$mn, na.rm=T)
 
  if(is.na(vect[vect$id==id,]$mn)){
    n_pnt_m <- NA
  }else{
  n_pnt_m <-  length(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],][
    !is.na(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$mn) & 
    abs(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$mn - vect[vect$id==id,]$mn)<= 0.5,])/
    length(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],])
  }
  
  if(is.na(vect[vect$id==id,]$tpi)){
    n_pnt_t <- NA
  }else{
  n_pnt_t <-  length(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],][
    !is.na(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$tpi) & 
      abs(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$tpi - vect[vect$id==id,]$tpi)<= 0.5,])/
    length(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],])
  }
  
  n_nt_mn <- min(abs(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$mn - vect[vect$id==id,]$mn))
  n_nt_tp <- min(abs(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$tpi - vect[vect$id==id,]$tpi))
  
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

  return(data.frame(t(unlist(list("n_mn_mn"=n_mn_mn,"n_sd_mn"=n_sd_mn, "n_mn_mdn"=n_mn_mdn, "n_pnt_m"=n_pnt_m, "n_pnt_t"=n_pnt_t,
                                  "n_nt_mn"=n_nt_mn,"n_nt_tp"=n_nt_tp,"n_sd_tpi"=n_sd_tpi,"n_sd_slp"=n_sd_slp,"n_sd_shp"=n_sd_shp,
                                  "n_mn_sqr"=n_mn_sqr,"n_sd_sqr"=n_sd_sqr,
                           "n_mn_std"=n_mn_std, "n_mn_slp"=n_mn_slp, "n_mn_shp"=n_mn_shp, "n_mn_tpi"=n_mn_tpi)))))
}

calc_neighbor_metrics_2_ <- function(id, vect){
  print(id)
  if(vect[vect$id==id,]$nbr==""){
    return(data.frame(t(unlist(list("n_mn_mn_2"=NA,"n_sd_mn_2"=NA, "n_mn_mdn_2"=NA, 
                                    "n_mn_std_2"=NA, "n_mn_tpi_2"=NA, "n_sd_tpi_2"=NA, 
                                    "n_mn_slp_2"=NA, "n_sd_slp_2"=NA, "n_mn_shp_2"=NA, 
                                    "n_sd_shp_2"=NA, "n_mn_sqr_2"=NA, "n_sd_sqr_2"=NA)))))
  }
  
  n_mn_mn_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_mn_mn, na.rm=T)
  n_sd_mn_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_sd_mn, na.rm=T)
  
  n_mn_mdn_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_mn_mdn, na.rm=T)
  
  n_mn_std_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_mn_std, na.rm=T)
  
  n_mn_tpi_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_mn_tpi, na.rm=T)
  n_sd_tpi_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_sd_tpi, na.rm=T)
  
  n_mn_slp_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_mn_slp, na.rm=T)
  n_sd_slp_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_sd_slp, na.rm=T)
  
  n_mn_shp_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_mn_shp, na.rm=T)
  n_sd_shp_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_sd_shp, na.rm=T)
  
  n_mn_sqr_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_mn_sqr, na.rm=T)
  n_sd_sqr_2 <- mean(vect[vect$id %in% str_split(vect[vect$id==id,]$nbr, ",")[[1]],]$n_sd_sqr, na.rm=T)
  
  return(data.frame(t(unlist(list("n_mn_mn_2"=n_mn_mn_2,"n_sd_mn_2"=n_sd_mn_2, "n_mn_mdn_2"=n_mn_mdn_2, 
                                  "n_mn_std_2"=n_mn_std_2, "n_mn_tpi_2"=n_mn_tpi_2, "n_sd_tpi_2"=n_sd_tpi_2, 
                                  "n_mn_slp_2"=n_mn_slp_2, "n_sd_slp_2"=n_sd_slp_2, "n_mn_shp_2"=n_mn_shp_2, 
                                  "n_sd_shp_2"=n_sd_shp_2, "n_mn_sqr_2"=n_mn_sqr_2, "n_sd_sqr_2"=n_sd_sqr_2)))))
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
 
  v <- vect
  sf <- st_as_sf(v)
  
  #es podria fer que la tolerància depengués del perímetre, segons alguna fórmula que hauria de provar
  smp <- st_simplify(sf, preserveTopology = T, dTolerance = 2)
  v <- as_Spatial(smp)
  
  vt <- vect(vect)
  v$peri <- perim(vt)
  rm(vt)
  
  y <- fortify(v) 
  ag <- aggregate(y, by=list(id=y$id), FUN=length)
  for(i in ag$id){
    v[i,"vtx"] <- ag[ag$id==i,2] 
  }
  #v$sqr_a <- v$vtx/v$area
  
  #sqr_p <- 1/(v$vtx/sqrt(v$peri))
  sqr_p <- 1/(v$vtx/log(v$peri))
  #sqr_p <- 1/(v$vtx/v$peri)
  sqr_p
}

calc_longness <- function(vect){
  calc_longest_line_within(vect)/vect$are
}

calc_longest_line_within <- function(vects){
  vects <- reproject_EPSG_25831_vect(vect)
  #vects$id___ <- 1:nrow(vects)
  
  n.cores <- detectCores()-2
  vects_split <- split(vects, factor(sort(rank(row.names(vects))%%n.cores)))
  
  
  clust <- makeCluster(n.cores, outfile="calc_longest_line_log.txt")
  clusterExport(clust, c("vects_split","calc_longest_line_within_", "calc_longest_line_within__"), envir =  environment())
  clusterEvalQ(clust, library(sf))
  lngs_ln_split <- parLapply(clust, vects_split, function(vcts){
    lngs_ln <- sapply(1:nrow(vcts), function(i, vcts){
      calc_longest_line_within__(vcts[i,])
    }, vcts)
  })
  # lngs_ln <- parSapply(clust,ids, function(i, vects){
  #   print(length(vects))
  #   calc_longest_line_within__(vects[i,])
  # }, vects)
  stopCluster(clust)
  
  
}

calc_longest_line_within_ <- function(i, vects){
  print(length(vects))
  calc_longest_line_within__(vects[i,])
}
  
calc_longest_line_within__ <- function(v){
  print(v$id)
  v <- st_as_sf(v)
  vb <- st_buffer(v, -0.3, nQuadSegs=1)
  v_coords <- st_coordinates(vb)
  vertices <- st_multipoint(v_coords[,c(1,2)]) %>% st_zm() %>% 
  st_geometry() %>% st_cast('POINT')
  distances <- st_distance(vertices)
  
  
  order <- order(distances, decreasing=T)
  order <- cbind(row(distances)[order], col(distances)[order])
  order <- do.call(rbind, unique(lapply(1:nrow(order),function(i){
    sort(order[i,])
    })))
  dist <- 0
  for(rw in 1:nrow(order)){
    v1 <- vertices[order[rw,1]]
    v2 <- vertices[order[rw,2]]
    pair <- st_combine(c(v1, v2))
    line <- st_cast(pair, "LINESTRING") %>% st_set_crs(st_crs(v))
    if(length(st_crosses(v, line)[[1]])==0){
      dist <- distances[order[rw,1],order[rw,2]]
      break()
    }
  }
  return(dist)
  
}
