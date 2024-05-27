source("./classificador_vol_america/scripts/smoothen_raster.R")
print(paste(Sys.time(),"smoothen_raster_supercells_do"))
load("./classificador_vol_america/temp/max_val.RData")
load("./classificador_vol_america/temp/smoothen_raster_grid.RData")

dir.create("./classificador_vol_america/rasters/smoothen",showWarnings=F)
dir.create("./classificador_vol_america/rasters/smoothen/split",showWarnings=F)
files <- list.files("./classificador_vol_america/rasters/original/split/",pattern="\\.tif$")
# raster_split <- lapply(paste("./classificador_vol_america/rasters/original/split/",files,sep=""),raster)
# names(raster_split) <- lapply(raster_split,function(r){
#   sub(" ","",sub(" ","",sub(".tif","",basename(r@file@name))))
# })
print(paste(length(files), " grid elements to smoothen",sep=""))

if(length(files)>0){
  tryCatch(
    {
      # print(paste(Sys.time(),"do",sep="-"))
      n.cores <- detectCores()
      source("./classificador_vol_america/scripts/clump_vector.R")
      unlink("./classificador_vol_america/logs/smoothen_raster_supercells_.txt")
      clust <- create_cluster_clump_(n.cores,"smoothen_raster_supercells_")
      parLapplyLB(clust,files,function(file,max_val,smoothen_raster_grid){
        rast <- raster(paste("./classificador_vol_america/rasters/original/split/",file,sep=""))
        tryCatch({
          # names(rast) <- gsub(" ","",sub(".tif","",file))
          smoothen_raster_supercells_(rast,max_val,smoothen_raster_grid)
        },error=function(x){
          # id <- sub(" ","",sub(" ","",sub(".tif","",basename(rast@file@name))))
          print(paste(Sys.time(),"error",x,sep=" - "))
        })
      },max_val,smoothen_raster_grid)
      stopCluster(clust)
      rm(clust)
    },
    error=function(x) {
      print(paste(Sys.time(),"error",sep="-"))
      rm(files)
      gc()
      smoothen_raster_ini()
    }
  ) 
}
unlink("./classificador_vol_america/logs/smoothen_raster_supercells_.txt")


files <- list.files("./classificador_vol_america/rasters/original/split/",pattern="\\.tif$")
if(length(files>0)){
  raster_split <- lapply(paste("./classificador_vol_america/rasters/original/split/",files,sep=""),raster)
  names(raster_split) <- lapply(raster_split,function(r){
    sub(" ","",sub(" ","",sub(".tif","",basename(r@file@name))))
  })
  smoothen_raster_do_ids_failed <- names(raster_split)
  save(smoothen_raster_do_ids_failed,file="./classificador_vol_america/temp/smoothen_raster_do_ids_failed.RData")
  rm(smoothen_raster_do_ids_failed)
  n.cores <- detectCores()
  unlink("./classificador_vol_america/logs/smoothen_raster_supercells_failed_.txt")
  clust <- create_cluster_clump_(n.cores,"smoothen_raster_supercells_failed_")
  parLapplyLB(clust,raster_split,smoothen_raster_supercells_,max_val,smoothen_raster_grid,pretty=F)
  stopCluster(clust)
  rm(raster_split)
  rm(clust)
  unlink("./classificador_vol_america/logs/smoothen_raster_supercells_failed_.txt")
}

files <- list.files("./classificador_vol_america/rasters/original/split/",pattern="\\.tif$")
if(length(files)>0){
  restartSession(command=source("./classificador_vol_america/scripts/smoothen_raster_supercells_do.R"))
}
unlink("./classificador_vol_america/temp/max_val.RData")
rm(list=ls())
gc()

smoothen_raster_current <- "smoothen"
save(smoothen_raster_current,file="./classificador_vol_america/temp/smoothen_raster_current.RData")

source("./classificador_vol_america/scripts/smoothen_raster.R")
smoothen_raster_ini()
