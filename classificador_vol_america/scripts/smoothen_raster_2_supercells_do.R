source("./classificador_vol_america/scripts/smoothen_raster_2.R")
print(paste(Sys.time(),"smoothen_raster_2_supercells_do"))
# load("./classificador_vol_america/temp/max_val.RData")
max_val <- 10
# max_val <- 25.5
load("./classificador_vol_america/temp/smoothen_raster_2_grid.RData")

dir.create("./classificador_vol_america/rasters/smoothen2",showWarnings=F)
dir.create("./classificador_vol_america/rasters/smoothen2/split",showWarnings=F)
files <- list.files("./classificador_vol_america/rasters/smoothen/split/",pattern="\\.tif$")

# files_done <- list.files("./classificador_vol_america/rasters/smoothen2/split/",pattern="\\.tif$")
# files <- files[!files%in%files_done]
# rm(files_done)
# raster_split <- lapply(paste("./classificador_vol_america/rasters/original/split/",files,sep=""),raster)
# names(raster_split) <- lapply(raster_split,function(r){
#   sub(" ","",sub(" ","",sub(".tif","",basename(r@file@name))))
# })
print(paste(length(files), " grid elements to smoothen",sep=""))

# files <- files[!files%in%c("10.tif","12.tif","19.tif")]

if(length(files)>0){
  tryCatch(
    {
      # print(paste(Sys.time(),"do",sep="-"))
      n.cores <- detectCores()
      free.mem <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024
      if(ceiling(sqrt(free.mem)/(255/5))<=n.cores){
        n.cores<-ceiling(sqrt(free.mem)/(255/5))
      }
      source("./classificador_vol_america/scripts/clump_vector.R")
      unlink("./classificador_vol_america/logs/smoothen_raster_2_supercells_.txt")
      clust <- create_cluster_clump_(n.cores,"smoothen_raster_2_supercells_")
      parLapplyLB(clust,files,function(file,max_val,smoothen_raster_grid){
      # lapply(files,function(file,max_val,smoothen_raster_grid){
          rast <- raster(paste("./classificador_vol_america/rasters/smoothen/split/",file,sep=""))
        tryCatch({
          # names(rast) <- gsub(" ","",sub(".tif","",file))
          smoothen_raster_2_supercells_(rast,max_val,smoothen_raster_2_grid)
        },error=function(x){
          # id <- sub(" ","",sub(" ","",sub(".tif","",basename(rast@file@name))))
          print(paste(file,Sys.time(),"error",x,sep=" - "))
        })
      },max_val,smoothen_raster_2_grid)
      stopCluster(clust)
      rm(clust)
      gc()
      unlink("./classificador_vol_america/logs/smoothen_raster_2_supercells_.txt")
    },
    error=function(x) {
      print(paste(Sys.time(),"error",sep="-"))
      rm(files)
      gc()
      # stop()
      smoothen_raster_2_ini()
    }
  ) 
}
unlink("./classificador_vol_america/logs/smoothen_raster_2_supercells_.txt")


files <- list.files("./classificador_vol_america/rasters/smoothen/split/",pattern="\\.tif$")
# files_done <- list.files("./classificador_vol_america/rasters/smoothen2/split/",pattern="\\.tif$")
# files <- files[!files%in%files_done]
# rm(files_done)

if(length(files>0)){
  raster_split <- lapply(paste("./classificador_vol_america/rasters/smoothen/split/",files,sep=""),raster)
  names(raster_split) <- lapply(raster_split,function(r){
    sub(" ","",sub(" ","",sub(".tif","",basename(r@file@name))))
  })
  smoothen_raster_2_do_ids_failed <- names(raster_split)
  save(smoothen_raster_2_do_ids_failed,file="./classificador_vol_america/temp/smoothen_raster_2_do_ids_failed.RData")
  rm(smoothen_raster_2_do_ids_failed)
  n.cores <- detectCores()
  free.mem <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024
  if(ceiling(sqrt(free.mem)/(255/5))<=n.cores){
    n.cores<-ceiling(sqrt(free.mem)/(255/5))
  }
  unlink("./classificador_vol_america/logs/smoothen_raster_2_supercells_failed_.txt")
  clust <- create_cluster_clump_(n.cores,"smoothen_raster_2_supercells_failed_")
  parLapplyLB(clust,raster_split,smoothen_raster_2_supercells_,max_val,smoothen_raster_2_grid,pretty=F)
  # lapply(raster_split,smoothen_raster_2_supercells_,max_val,smoothen_raster_2_grid,pretty=F)
  stopCluster(clust)
  rm(raster_split)
  rm(clust)
  unlink("./classificador_vol_america/logs/smoothen_raster_2_supercells_failed_.txt")
}

files <- list.files("./classificador_vol_america/rasters/smoothen/split/",pattern="\\.tif$")
if(length(files)>0){
  restartSession(command=source("./classificador_vol_america/scripts/smoothen_raster_2_supercells_do.R"))
}
# unlink("./classificador_vol_america/temp/max_val.RData")
rm(list=ls())
gc()

smoothen_raster_2_current <- "smoothen"
save(smoothen_raster_2_current,file="./classificador_vol_america/temp/smoothen_raster_2_current.RData")

source("./classificador_vol_america/scripts/smoothen_raster_2.R")
smoothen_raster_2_ini()
