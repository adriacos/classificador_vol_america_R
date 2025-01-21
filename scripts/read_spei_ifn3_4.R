library(ncdf4)
library(CFtime)
library(sf)
library(lattice)
# library(RColorBrewer)
library(lubridate)


ncin <- nc_open("./test/spei12.nc")
gc()
spei12_array <- ncvar_get(ncin,"spei")
fillvalue <- ncatt_get(ncin,"spei","_FillValue")
gc()
spei12_array[spei12_array==fillvalue$value] <- NA
rm(fillvalue)
gc()

lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin,"lat")

time <- ncvar_get(ncin,"time")
tunits <- ncatt_get(ncin,"time","units")


spei_vec_long <- as.vector(spei12_array)
rm(spei12_array)
gc()
spei_mat <- matrix(spei_vec_long, nrow=dim(lon)*dim(lat), ncol=dim(time))
rm(spei_vec_long)
gc()

coords <- setNames(as.data.frame(as.matrix(expand.grid(lon,lat))),c("lon","lat"))
coords <- as.data.frame(coords)
vects_spei <- st_buffer(st_as_sf(coords,coords=c("lon", "lat"),
                                 crs=23030),550,endCapStyle="SQUARE")
# vects_spei$id <- 1:nrow(vects_spei)
vects_spei$lon <- coords$lon
vects_spei$lat <- coords$lat
rm(coords)
gc()

data <- read.csv("./test/data_comp_ifn2_ifn3.csv")[,c("idparcela","utm_x_fus30_ifn3","utm_y_fus30_ifn3")]
dm <- setNames(read.csv(paste("./test/parcelaifn",3,"_espanya.csv",sep=""))[,c("idparcela","datamostreig")],c("idparcela","datamosteig_ifn3"))
data <- merge(data,dm)
dm <- setNames(read.csv(paste("./test/parcelaifn",2,"_espanya.csv",sep=""))[,c("idparcela","anymostreig")],c("idparcela","anymostreig_ifn2"))
data <- merge(data,dm)
rm(dm)
gc()

data.gis <- st_as_sf(data,coords=c("utm_x_fus30_ifn3","utm_y_fus30_ifn3"),crs=25830)
vects_spei <- st_transform(vects_spei,crs=st_crs(data.gis))
lonlat <- as.matrix(expand.grid(lon,lat))
# rm(data)
gc()
source("./classificador_vol_america/scripts/clump_vector.R")
# unlink("./classificador_vol_america/logs/log_spei.txt")
# cl <- create_cluster_clump_(2,"log_spei")
speis_p <- lapply(data.gis$idparcela,function(idp,data.gis,vects_spei,spei_mat,lonlat,time){
  print(idp)
  dp <- data.gis[data.gis$idparcela==idp,]
  dp <- st_intersection(dp,vects_spei)
  dp <- as.data.frame(dp)
  days_ifn3 <- as.numeric(ymd(dp$datamosteig_ifn3)-ymd("1970-01-01"))
  days_ifn2 <- as.numeric(ymd(paste((dp$anymostreig_ifn2+1),"-01-01",sep=""))-ymd("1970-01-01"))
  tp <- which(time<days_ifn3&time>=days_ifn2)
  speis_dp <- spei_mat[which(lonlat[,1]==dp$lon&lonlat[,2]==dp$lat),tp]
  rm(days_ifn2)
  rm(days_ifn3)
  rm(tp)
  speis_dp
},data.gis,vects_spei,spei_mat,lonlat,time)
# stopCluster(cl)
# rm(cl)
# gc()
# unlink("./classificador_vol_america/logs/log_spei.txt")
data$spei_12_m <- sapply(speis_p,mean)
data$spei_12_max <- sapply(speis_p,max)
data$spei_12_min <- sapply(speis_p,min)
data$spei_12_med <- sapply(speis_p,median)
data$spei_12_3quart <- sapply(speis_p,function(sp){quantile(sp,0.75,na.rm=T)})
data$spei_12_1quart <- sapply(speis_p,function(sp){quantile(sp,0.25,na.rm=T)})
data <- data[,colnames(data)[colnames(data)%in%c("idparcela","spei_12_m","spei_12_max","spei_12_med","spei_12_min","spei_12_3quart","spei_12_1quart")]]
# data <- merge(read.csv("./test/data_comp_ifn2_ifn3.csv"),data)
write.csv(data,"./test/data_comp_ifn2_ifn3_spei_12.csv")


