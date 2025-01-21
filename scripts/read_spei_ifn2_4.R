library(ncdf4)
library(CFtime)
library(sf)
library(lattice)
# library(RColorBrewer)
library(lubridate)

get_idparcela <- function(data){
  data$idparcela <- as.numeric(paste(data$provincia,unlist(lapply(data$estadillo,function(est){paste(paste(rep("0",4-nchar(as.character(est))),collapse=""),est,sep="")})),sep=""))
  data
}

ncin <- nc_open("./test/spei24.nc")
gc()
spei24_array <- ncvar_get(ncin,"spei")
fillvalue <- ncatt_get(ncin,"spei","_FillValue")
gc()
spei24_array[spei12_array==fillvalue$value] <- NA
rm(fillvalue)
gc()

lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin,"lat")

time <- ncvar_get(ncin,"time")
tunits <- ncatt_get(ncin,"time","units")


spei_vec_long <- as.vector(spei24_array)
rm(spei24_array)
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

data <- read.csv("./test/data_comp_ifn2_ifn3_ifn4.csv")[,c("idparcela","utm_x_fus30_ifn3","utm_y_fus30_ifn3")]
dm <- setNames(read.csv(paste("./test/parcelaifn",3,"_espanya.csv",sep=""))[,c("idparcela","datamostreig")],c("idparcela","datamostreig_ifn3"))
data <- merge(data,dm)
dm <- setNames(read.csv(paste("./test/parcelaifn",2,"_espanya.csv",sep=""))[,c("idparcela","anymostreig")],c("idparcela","anymostreig_ifn2"))
data <- merge(data,dm)
rm(dm)
dm <- setNames(read.csv(paste("./test/_PCParcelas.csv",sep=""))[,c("Provincia","Estadillo","FechaFin")],c("provincia","estadillo","datamostreig_ifn4"))
dm <- get_idparcela(dm)
data <- merge(data,dm[,c("idparcela","datamostreig_ifn4")])
data$datamostreig_ifn4 <- sapply(str_split(data$datamostreig_ifn4," "),function(dd){dd[[1]]})
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
speis_p_2_4 <- lapply(data.gis$idparcela,function(idp,data.gis,vects_spei,spei_mat,lonlat,time){
  print(idp)
  dp <- data.gis[data.gis$idparcela==idp,]
  dp <- st_intersection(dp,vects_spei)
  dp <- as.data.frame(dp)
  days_ifn4 <- as.numeric(ymd(dp$datamostreig_ifn4)-ymd("1970-01-01"))
  days_ifn2 <- as.numeric(ymd(paste((dp$anymostreig_ifn2+1),"-01-01",sep=""))-ymd("1970-01-01"))
  tp <- which(time<days_ifn4&time>=days_ifn2)
  speis_dp <- spei_mat[which(lonlat[,1]==dp$lon&lonlat[,2]==dp$lat),tp]
  rm(days_ifn2)
  rm(days_ifn4)
  rm(tp)
  speis_dp
},data.gis,vects_spei,spei_mat,lonlat,time)
# stopCluster(cl)
# rm(cl)
# gc()
# unlink("./classificador_vol_america/logs/log_spei.txt")
data$spei_ifn2_ifn4_24_m <- sapply(speis_p_2_4,mean)
data$spei_ifn2_ifn4_24_max <- sapply(speis_p_2_4,max)
data$spei_ifn2_ifn4_24_min <- sapply(speis_p_2_4,min)
data$spei_ifn2_ifn4_24_med <- sapply(speis_p_2_4,median)
data$spei_ifn2_ifn4_24_sumneg00 <- abs(sapply(speis_p_2_4,function(sp){sum(sp[sp<0])}))
data$spei_ifn2_ifn4_24_sumneg05 <- abs(sapply(speis_p_2_4,function(sp){sum(sp[sp<(-0.5)])}))
data$spei_ifn2_ifn4_24_sumneg15 <- abs(sapply(speis_p_2_4,function(sp){sum(sp[sp<(-1.5)])}))
data$spei_ifn2_ifn4_24_sumneg10 <- abs(sapply(speis_p_2_4,function(sp){sum(sp[sp<(-1)])}))
data$spei_ifn2_ifn4_24_tneg00 <- sapply(speis_p_2_4,function(sp){length(sp[sp<0])/length(sp)})
data$spei_ifn2_ifn4_24_tneg05 <- sapply(speis_p_2_4,function(sp){length(sp[sp<(-0.5)])/length(sp)})
data$spei_ifn2_ifn4_24_tneg15 <- sapply(speis_p_2_4,function(sp){length(sp[sp<(-1.5)])/length(sp)})
data$spei_ifn2_ifn4_24_tneg10 <- sapply(speis_p_2_4,function(sp){length(sp[sp<(-1)])/length(sp)})
data$spei_ifn2_ifn4_24_3quart <- sapply(speis_p_2_4,function(sp){quantile(sp,0.75,na.rm=T)})
data$spei_ifn2_ifn4_24_1quart <- sapply(speis_p_2_4,function(sp){quantile(sp,0.25,na.rm=T)})
data <- data[,colnames(data)[colnames(data)%in%c("idparcela","spei_ifn2_ifn4_24_m",
                                                 "spei_ifn2_ifn4_24_max","spei_ifn2_ifn4_24_med",
                                                 "spei_ifn2_ifn4_24_min","spei_ifn2_ifn4_24_3quart",
                                                 "spei_ifn2_ifn4_24_1quart","spei_ifn2_ifn4_24_sumneg00",
                                                 "spei_ifn2_ifn4_24_sumneg05",
                                                 "spei_ifn2_ifn4_24_sumneg15","spei_ifn2_ifn4_24_sumneg10",
                                                 "spei_ifn2_ifn4_24_tneg00","spei_ifn2_ifn4_24_tneg05",
                                                 "spei_ifn2_ifn4_24_tneg15",
                                                 "spei_ifn2_ifn4_24_tneg10")]]
# data <- merge(read.csv("./test/data_comp_ifn2_ifn3.csv"),data)
rm(speis_p_2_4)
gc()


speis_p_3_4 <- lapply(data.gis$idparcela,function(idp,data.gis,vects_spei,spei_mat,lonlat,time){
  print(idp)
  dp <- data.gis[data.gis$idparcela==idp,]
  dp <- st_intersection(dp,vects_spei)
  dp <- as.data.frame(dp)
  days_ifn4 <- as.numeric(ymd(dp$datamostreig_ifn4)-ymd("1970-01-01"))
  days_ifn3 <- as.numeric(ymd(dp$datamostreig_ifn3)-ymd("1970-01-01"))
  tp <- which(time<days_ifn4&time>=days_ifn3)
  speis_dp <- spei_mat[which(lonlat[,1]==dp$lon&lonlat[,2]==dp$lat),tp]
  rm(days_ifn3)
  rm(days_ifn4)
  rm(tp)
  speis_dp
},data.gis,vects_spei,spei_mat,lonlat,time)
# stopCluster(cl)
# rm(cl)
# gc()
# unlink("./classificador_vol_america/logs/log_spei.txt")
data$spei_ifn3_ifn4_24_m <- sapply(speis_p_3_4,mean)
data$spei_ifn3_ifn4_24_max <- sapply(speis_p_3_4,max)
data$spei_ifn3_ifn4_24_min <- sapply(speis_p_3_4,min)
data$spei_ifn3_ifn4_24_med <- sapply(speis_p_3_4,median)
data$spei_ifn3_ifn4_24_sumneg00 <- abs(sapply(speis_p_3_4,function(sp){sum(sp[sp<0])}))
data$spei_ifn3_ifn4_24_sumneg05 <- abs(sapply(speis_p_3_4,function(sp){sum(sp[sp<(-0.5)])}))
data$spei_ifn3_ifn4_24_sumneg15 <- abs(sapply(speis_p_3_4,function(sp){sum(sp[sp<(-1.5)])}))
data$spei_ifn3_ifn4_24_sumneg10 <- abs(sapply(speis_p_3_4,function(sp){sum(sp[sp<(-1)])}))
data$spei_ifn3_ifn4_24_tneg00 <- sapply(speis_p_3_4,function(sp){length(sp[sp<0])/length(sp)})
data$spei_ifn3_ifn4_24_tneg05 <- sapply(speis_p_3_4,function(sp){length(sp[sp<(-0.5)])/length(sp)})
data$spei_ifn3_ifn4_24_tneg15 <- sapply(speis_p_3_4,function(sp){length(sp[sp<(-1.5)])/length(sp)})
data$spei_ifn3_ifn4_24_tneg10 <- sapply(speis_p_3_4,function(sp){length(sp[sp<(-1)])/length(sp)})
data$spei_ifn3_ifn4_24_3quart <- sapply(speis_p_3_4,function(sp){quantile(sp,0.75,na.rm=T)})
data$spei_ifn3_ifn4_24_1quart <- sapply(speis_p_3_4,function(sp){quantile(sp,0.25,na.rm=T)})
# data <- data[,colnames(data)[colnames(data)%in%c("idparcela",
#                                                  "spei_ifn2_ifn4_24_m","spei_ifn2_ifn4_24_max","spei_ifn2_ifn4_24_med","spei_ifn2_ifn4_24_min","spei_ifn2_ifn4_24_3quart","spei_ifn2_ifn4_24_1quart","spei_ifn2_ifn4_24_sumneg00","spei_ifn2_ifn4_24_sumneg15","spei_ifn2_ifn4_24_sumneg10","spei_ifn2_ifn4_24_tneg00","spei_ifn2_ifn4_24_tneg15","spei_ifn2_ifn4_24_tneg10",
#                                                  "spei_ifn3_ifn4_24_m","spei_ifn3_ifn4_24_max","spei_ifn3_ifn4_24_med","spei_ifn3_ifn4_24_min","spei_ifn3_ifn4_24_3quart","spei_ifn3_ifn4_24_1quart","spei_ifn3_ifn4_24_sumneg00","spei_ifn3_ifn4_24_sumneg15","spei_ifn3_ifn4_24_sumneg10","spei_if3_ifn4_24_tneg00","spei_ifn3_ifn4_24_tneg15","spei_if3_ifn4_24_tneg10")]]
# data <- merge(read.csv("./test/data_comp_ifn2_ifn3.csv"),data)
rm(speis_p_3_4)
gc()


speis_p_2_3 <- lapply(data.gis$idparcela,function(idp,data.gis,vects_spei,spei_mat,lonlat,time){
  print(idp)
  dp <- data.gis[data.gis$idparcela==idp,]
  dp <- st_intersection(dp,vects_spei)
  dp <- as.data.frame(dp)
  # days_ifn4 <- as.numeric(ymd(dp$datamostreig_ifn4)-ymd("1970-01-01"))
  days_ifn3 <- as.numeric(ymd(dp$datamostreig_ifn3)-ymd("1970-01-01"))
  days_ifn2 <- as.numeric(ymd(paste((dp$anymostreig_ifn2+1),"-01-01",sep=""))-ymd("1970-01-01"))
  tp <- which(time<days_ifn3&time>=days_ifn2)
  speis_dp <- spei_mat[which(lonlat[,1]==dp$lon&lonlat[,2]==dp$lat),tp]
  rm(days_ifn2)
  rm(days_ifn4)
  rm(tp)
  speis_dp
},data.gis,vects_spei,spei_mat,lonlat,time)
# stopCluster(cl)
# rm(cl)
# gc()
# unlink("./classificador_vol_america/logs/log_spei.txt")
data$spei_ifn2_ifn3_24_m <- sapply(speis_p_2_3,mean)
data$spei_ifn2_ifn3_24_max <- sapply(speis_p_2_3,max)
data$spei_ifn2_ifn3_24_min <- sapply(speis_p_2_3,min)
data$spei_ifn2_ifn3_24_med <- sapply(speis_p_2_3,median)
data$spei_ifn2_ifn3_24_sumneg00 <- abs(sapply(speis_p_2_3,function(sp){sum(sp[sp<0])}))
data$spei_ifn2_ifn3_24_sumneg05 <- abs(sapply(speis_p_2_3,function(sp){sum(sp[sp<(-0.5)])}))
data$spei_ifn2_ifn3_24_sumneg15 <- abs(sapply(speis_p_2_3,function(sp){sum(sp[sp<(-1.5)])}))
data$spei_ifn2_ifn3_24_sumneg10 <- abs(sapply(speis_p_2_3,function(sp){sum(sp[sp<(-1)])}))
data$spei_ifn2_ifn3_24_tneg00 <- sapply(speis_p_2_3,function(sp){length(sp[sp<0])})
data$spei_ifn2_ifn3_24_tneg05 <- sapply(speis_p_2_3,function(sp){length(sp[sp<(-0.5)])})
data$spei_ifn2_ifn3_24_tneg15 <- sapply(speis_p_2_3,function(sp){length(sp[sp<(-1.5)])})
data$spei_ifn2_ifn3_24_tneg10 <- sapply(speis_p_2_3,function(sp){length(sp[sp<(-1)])})
data$spei_ifn2_ifn3_24_3quart <- sapply(speis_p_2_3,function(sp){quantile(sp,0.75,na.rm=T)})
data$spei_ifn2_ifn3_24_1quart <- sapply(speis_p_2_3,function(sp){quantile(sp,0.25,na.rm=T)})

write.csv(data,"./test/data_comp_ifn2_ifn3_ifn4_spei_24.csv")


