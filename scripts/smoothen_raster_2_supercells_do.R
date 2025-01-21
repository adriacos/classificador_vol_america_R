source("./scripts/smoothen_raster_2.R")
print(paste(Sys.time(),"smoothen_raster_2_supercells_do"))
load("./temp/max_val.RData")
# max_val <- 10
load("./temp/smoothen_raster_2_grid.RData")

dir.create("./rasters/smoothen2",showWarnings=F)
dir.create("./rasters/smoothen2/split",showWarnings=F)
dir.create("./rasters/smoothen2/split/failed",showWarnings=F)

files <- list.files("./rasters/smoothen/split/",pattern="\\.tif$")

# files_done <- list.files("./rasters/smoothen2/split/",pattern="\\.tif$")
# files <- files[!files%in%files_done]
# rm(files_done)
# raster_split <- lapply(paste("./rasters/original/split/",files,sep=""),raster)
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
      n.cores <- 16
      source("./scripts/clump_vector.R")
      unlink("./logs/smoothen_raster_2_supercells_.txt")
      clust <- create_cluster_clump_(n.cores,"smoothen_raster_2_supercells_")
      parLapplyLB(clust,files,function(file,max_val,smoothen_raster_2_grid){
      # lapply(files,function(file,max_val,smoothen_raster_grid){
        rast <- raster(paste("./rasters/smoothen/split/",file,sep=""))
        tryCatch({
          # names(rast) <- gsub(" ","",sub(".tif","",file))
          smoothen_raster_2_supercells_(rast,max_val,smoothen_raster_2_grid)
          gc()
        },error=function(x){
          # id <- sub(" ","",sub(" ","",sub(".tif","",basename(rast@file@name))))
          print(paste(file,Sys.time(),"error",x,sep=" - "))
          unlink(paste("./rasters/smoothen2/split/failed/",file,sep=""))
          writeRaster(rast,paste("./rasters/smoothen2/split/failed/",file,sep=""),)
          unlink(paste("./rasters/smoothen/split/",file,sep=""))
        })
      },max_val,smoothen_raster_2_grid)
      stopCluster(clust)
      rm(clust)
      gc()
    },
    error=function(x) {
      print(paste(Sys.time(),"error",sep="-"))
      rm(files)
      gc()
      # stop()
      restartSession(command=source("./scripts/smoothen_raster_2_supercells_do.R"))
      # smoothen_raster_2_ini()
    }
  ) 
}
unlink("./logs/smoothen_raster_2_supercells_.txt")


files <- list.files("./rasters/smoothen/split/",pattern="\\.tif$")
# files_done <- list.files("./rasters/smoothen2/split/",pattern="\\.tif$")
# files <- files[!files%in%files_done]
# rm(files_done)

if(length(files)>0){
  rm(list=ls())
  gc()
  source("./scripts/smoothen_raster_2.R")
  restartSession(command=source("./scripts/smoothen_raster_2_supercells_do.R"))
  # smoothen_raster_2_ini()
}
rm(files)
failed <- list.files("./rasters/smoothen2/split/failed",pattern="\\.tif$")

if(length(failed)>0){
  # raster_split <- lapply(paste("./rasters/smoothen2/split/failed/",failed,sep=""),raster)
  # names(raster_split) <- lapply(raster_split,function(r){
    # sub(" ","",sub(" ","",sub(".tif","",basename(r@file@name))))
  # })
  # smoothen_raster_2_do_ids_failed <- names(raster_split)
  # save(smoothen_raster_2_do_ids_failed,file="./temp/smoothen_raster_2_do_ids_failed.RData")
  # rm(smoothen_raster_2_do_ids_failed)
  n.cores <- detectCores()
  n.cores <- 16
  unlink("./logs/smoothen_raster_2_supercells_failed_.txt")
  source("./scripts/clump_vector.R")
  clust <- create_cluster_clump_(n.cores,"smoothen_raster_2_supercells_failed_")
  parLapplyLB(clust,failed,function(file,max_val,smoothen_raster_2_grid){
    rast <- raster(paste("./rasters/smoothen2/split/failed/",file,sep=""))
    smoothen_raster_2_supercells_(rast,max_val,smoothen_raster_2_grid,pretty=F)
    unlink(paste("./rasters/smoothen2/split/failed/",file,sep=""))
    gc()
  },max_val,smoothen_raster_2_grid)
  # lapply(raster_split,smoothen_raster_2_supercells_,max_val,smoothen_raster_2_grid,pretty=F)
  stopCluster(clust)
  rm(raster_split)
  rm(clust)
  gc()
  unlink("./logs/smoothen_raster_2_supercells_failed_.txt")
  unlink("./rasters/smoothen2/split/failed", recursive=T)
}

failed <- list.files("./rasters/smoothen2/split/failed",pattern="\\.tif$")
# files <- list.files("./rasters/smoothen2/split/",pattern="\\.tif$")
if(length(failed)>0){
  restartSession(command=source("./scripts/smoothen_raster_2_supercells_do.R"))
}
# unlink("./temp/max_val.RData")
rm(list=ls())
gc()

smoothen_raster_2_current <- "smoothen"
save(smoothen_raster_2_current,file="./temp/smoothen_raster_2_current.RData")

source("./scripts/smoothen_raster_2.R")
smoothen_raster_2_ini()
