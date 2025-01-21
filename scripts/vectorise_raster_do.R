library(terra)
library(raster)
# library(rgdal)
library(plyr)

# print("vectorise_raster_do")
print(paste(Sys.time()," vectorise_raster_do"))

dir.create("./vect/vectorised/", showWarnings = FALSE)
dir.create("./vect/vectorised/split/", showWarnings = FALSE)
dir.create("./vect/vectorised/split/ori", showWarnings = FALSE)
dir.create("./vect/vectorised/split/simplified", showWarnings = FALSE)

# files_list <- list.files("./rasters/pnoa/supercells/split",pattern="\\.tif$")
files_list <- list.files("./rasters/smoothen2/split",pattern="\\.tif$")
# files_list <- files_list[!files_list%in%gsub(".rds",".tif",list.files("./vect/vectorised/split/ori"))]
names(files_list) <- sapply(files_list,function(x){
  as.numeric(sub(".tif","",x))
})    

print(paste(length(files_list)," files to vectorise",sep=""))

# crs <- st_crs(st_read("./vect/grids/limit.gpkg"))
crs <- st_crs(st_read("./vect/limit.gpkg"))
n.cores <- round(detectCores()-(detectCores()/2))
n.cores <- 16
# n.cores <- detectCores()
source("./scripts/vectorise_raster.R")
unlink("./logs/vectorise_raster_do.txt")
cl <- create_cluster_clump_(n.cores,"vectorise_raster_do")
parLapplyLB(cl,files_list, vectorise_raster_do_,crs)
# lapply(files_list, vectorise_raster_do_,crs)
stopCluster(cl)
rm(cl)
gc()
unlink("./logs/vectorise_raster_do.txt")

files_list <- list.files("./rasters/smoothen2/split",pattern="\\.tif$")
# files_list <- files_list[!files_list%in%gsub(".rds",".tif",list.files("./vect/vectorised/split/ori"))]
names(files_list) <- sapply(files_list,function(x){
  as.numeric(sub(".tif","",x))
})    

if(length(files_list)>0){
  rm(files_list)
  vectorise_raster_ini()
}
rm(files_list)

unlink("./rasters/smoothen2/split",recursive=T)
# rm(rasters_list)
# rm(rast)
# rm(vect)
gc()

vectorise_raster_current <- "vectorised"
save(vectorise_raster_current,file="./temp/vectorise_raster_current.RData")

vectorise_raster_ini()
