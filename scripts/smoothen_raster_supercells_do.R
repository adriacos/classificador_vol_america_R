source("./scripts/smoothen_raster.R")
source("./scripts/clump_vector.R")

print(paste(Sys.time(),"smoothen_raster_supercells_do"))
load("./temp/max_val.RData")
load("./temp/smoothen_raster_grid.RData")

dir.create("./rasters/smoothen",showWarnings=F)
dir.create("./rasters/smoothen/split",showWarnings=F)
dir.create("./rasters/smoothen/split/failed",showWarnings=F)

files <- list.files("./rasters/original/split/",pattern="\\.tif$")
# raster_split <- lapply(paste("./rasters/original/split/",files,sep=""),raster)
# names(raster_split) <- lapply(raster_split,function(r){
#   sub(" ","",sub(" ","",sub(".tif","",basename(r@file@name))))
# })
print(paste(length(files), " grid elements to smoothen",sep=""))

test <- function(){
  files <- list.files("./rasters/smoothen2/split/",pattern="\\.tif$")
  n.cores <- detectCores()
  clust <- create_cluster_clump_(n.cores,"smoothen_raster_supercells_test_done")
  parLapplyLB(clust,files,function(f){
    print(f)
    if(file.exists(paste("./temp/st/",gsub(".tif",".png",f),sep=""))){
      print("exists")
      return(T)
    }
    t <- raster(paste("./rasters/smoothen2/split/",f,sep=""))
    png(paste("./temp/st/",gsub(".tif",".png",f),sep=""),height=200,width=200)
    plot(t)
    dev.off()
    return(T)
  })
  stopCluster(clust)
  rm(clust)
  gc()
}

test2 <- function(){
  files <- list.files("./rasters/smoothen2/split/",pattern="\\.tif$")
  lapply(files,function(f){
    print(f)
    unlink(paste("./vect/smoothen_raster_2_grid/",gsub(".tif",".gpkg",f),sep=""))
  })
}

if(length(files)>0){
  tryCatch(
    {
      # print(paste(Sys.time(),"do",sep="-"))
      n.cores <- detectCores()
      n.cores <- 9
      source("./scripts/clump_vector.R")
      unlink("./logs/smoothen_raster_supercells_.txt")
      clust <- create_cluster_clump_(n.cores,"smoothen_raster_supercells_")
      parLapplyLB(clust,files,function(file,max_val,smoothen_raster_grid){
        rast <- raster(paste("./rasters/original/split/",file,sep=""))
        tryCatch({
          # names(rast) <- gsub(" ","",sub(".tif","",file))
          smoothen_raster_supercells_(rast,max_val,smoothen_raster_grid)
        },error=function(x){
          # id <- sub(" ","",sub(" ","",sub(".tif","",basename(rast@file@name))))
          print(paste(Sys.time(),"error",x,sep=" - "))
          unlink(paste("./rasters/smoothen/split/failed/",file,sep=""))
          writeRaster(rast,paste("./rasters/smoothen/split/failed/",file,sep=""),)
          unlink(paste("./rasters/original/split/",file,sep=""))
        })
      },max_val,smoothen_raster_grid)
      stopCluster(clust)
      rm(clust)
      gc()
    },
    error=function(x) {
      print(paste(Sys.time(),"error",sep="-"))
      rm(files)
      gc()
      smoothen_raster_ini()
    }
  ) 
}
unlink("./logs/smoothen_raster_supercells_.txt")


files <- list.files("./rasters/original/split/",pattern="\\.tif$")
if(length(files>0)){
  rm(list=ls())
  gc()
  source("./scripts/smoothen_raster.R")
  smoothen_raster_ini()
}

failed <- list.files("./rasters/smoothen/split/failed",pattern="\\.tif$")
if(length(failed)>0){
  raster_split <- lapply(paste("./rasters/original/split/",files,sep=""),raster)
  names(raster_split) <- lapply(raster_split,function(r){
    sub(" ","",sub(" ","",sub(".tif","",basename(r@file@name))))
  })
  smoothen_raster_do_ids_failed <- names(raster_split)
  save(smoothen_raster_do_ids_failed,file="./temp/smoothen_raster_do_ids_failed.RData")
  rm(smoothen_raster_do_ids_failed)
  n.cores <- detectCores()
  n.cores <- 12
  unlink("./logs/smoothen_raster_supercells_failed_.txt")
  clust <- create_cluster_clump_(n.cores,"smoothen_raster_supercells_failed_")
  parLapplyLB(clust,raster_split,smoothen_raster_supercells_,max_val,smoothen_raster_grid,pretty=F)
  stopCluster(clust)
  rm(raster_split)
  rm(clust)
  unlink("./logs/smoothen_raster_supercells_failed_.txt")
  unlink("./rasters/smoothen/split/failed", recursive=T)
}

files <- list.files("./rasters/original/split/",pattern="\\.tif$")
if(length(files)>0){
  restartSession(command=source("./scripts/smoothen_raster_supercells_do.R"))
}
# unlink("./temp/max_val.RData")
rm(list=ls())
gc()

smoothen_raster_current <- "smoothen"
save(smoothen_raster_current,file="./temp/smoothen_raster_current.RData")

source("./scripts/smoothen_raster.R")
smoothen_raster_ini()
