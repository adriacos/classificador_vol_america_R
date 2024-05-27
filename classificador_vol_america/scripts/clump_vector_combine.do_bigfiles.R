source("./classificador_vol_america/scripts/clump_vector.R")
print("clump_vector_combine.do_bigfiles")
load("./classificador_vol_america/temp/kms_done.RData")
unlink("./classificador_vol_america/vect/clumped/temp/")

for (ikm in kms[-1][!kms[-1]%in%kms_done]){
  current_km <- ikm
  save(current_km,file="./classificador_vol_america/temp/current_km.RData")
  oris_done <- c()
  save(oris_done, file="./classificador_vol_america/temp/oris_done.RData")
  restartSession(command=source("./classificador_vol_america/scripts/clump_vector_combine.do_big.files_launch_km.R"))
}

# vects <- do.call(rbind,lapply(kms[-1],function(km){
#   readRDS(paste("./classificador_vol_america/vect/clumped/kms/", "global_clmp_",km,"km.gpkg", sep=""))
# }))
# load("./classificador_vol_america/temp/vects_filename.RData")
vects <- readRDS(vects,"./classificador_vol_america/vect/clumped/global_clmp_temp.rds",overwrite=T)
saveRDS(vects,"./classificador_vol_america/vect/clumped/global_clmp.rds",overwrite=T)
# rm(vects)
# gc()

clump_vector_current <- "clumped_big_files_pendinglast"
save(clump_vector_current,file="./classificador_vol_america/temp/clump_vector_current.RData")
unlink("./classificador_vol_america/temp/kms_done.RData")
unlink("./classificador_vol_america/temp/oris_km.RData")
unlink("./classificador_vol_america/temp/oris_done.RData")
unlink("./classificador_vol_america/temp/current_km.RData")
unlink("./classificador_vol_america/temp/current_ori.RData")

restartSession(command=source("./classificador_vol_america/scripts/clump_vector_big_files_last.R"))