print("clump_vector_big_files_last")
vects <- readRDS("./classificador_vol_america/vect/clumped/global_clmp.rds")
vects <- vects[,c("id", "npl", "pldn", "plmaxare")]

plare <- str_split(vects$plare, ",")
# load("./classificador_vol_america/temp/resolution.RData")
# plare <- lapply(plare,function(areas){
#   plare[plare>38*resolution]
# })

rm(resolution)
vects$plare_mn <- sapply(lapply(plare, as.numeric), mean, na.rm=T)
vects$plare_max <- sapply(lapply(plare, as.numeric), max, na.rm=T)
vects$plare_min <- sapply(lapply(plare, as.numeric), min, na.rm=T)
vects$plare_mdn <- sapply(lapply(plare, as.numeric), median, na.rm=T)
vects$plare_sd<- sapply(lapply(plare, as.numeric), sd, na.rm=T)
vects$plare_cv<- vects$plare_sd/vects$plare_mn
vects$plare_maxdif<- vects$max-vects$min
vects$plare_p10 <- sapply(lapply(plare, as.numeric), quantile, 0.10, na.rm=T)
vects$plare_p25 <- sapply(lapply(plare, as.numeric), quantile, 0.25, na.rm=T)
vects$plare_p75 <- sapply(lapply(plare, as.numeric), quantile, 0.75, na.rm=T)
vects$plare_p90 <- sapply(lapply(plare, as.numeric), quantile, 0.90, na.rm=T)
vects$plare<-NULL

# abans de guardar-lo, calcular: sd, cv, mitjana, mediana, primer quartil, tercer quartil, max i min de les plare
saveRDS(vects,"./classificador_vol_america/vect/clumped/global_clmp.rds",overwrite=T)

clump_vector_current <- "clumped_big_finalised"
save(clump_vector_current,file="./classificador_vol_america/temp/clump_vector_current.RData")

current_global <- "clumped"
save(current_global,file="./classificador_vol_america/temp/current_global")
restartSession(command=source("./classificador_vol_america/scripts/continue.R"))