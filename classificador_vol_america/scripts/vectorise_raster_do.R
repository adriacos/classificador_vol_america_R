library(terra)
library(raster)
# library(rgdal)
library(plyr)

# print("vectorise_raster_do")
print(paste(Sys.time()," vectorise_raster_do"))

dir.create("./classificador_vol_america/vect/vectorised/", showWarnings = FALSE)
dir.create("./classificador_vol_america/vect/vectorised/split/", showWarnings = FALSE)
dir.create("./classificador_vol_america/vect/vectorised/split/ori", showWarnings = FALSE)
dir.create("./classificador_vol_america/vect/vectorised/split/simplified", showWarnings = FALSE)

# files_list <- list.files("./classificador_vol_america/rasters/pnoa/supercells/split",pattern="\\.tif$")
files_list <- list.files("./classificador_vol_america/rasters/smoothen2/split",pattern="\\.tif$")
# files_list <- files_list[!files_list%in%gsub(".rds",".tif",list.files("./classificador_vol_america/vect/vectorised/split/ori"))]
names(files_list) <- sapply(files_list,function(x){
  as.numeric(sub(".tif","",x))
})    

print(paste(length(files_list)," files to vectorise",sep=""))

# crs <- st_crs(st_read("./classificador_vol_america/vect/grids/limit.gpkg"))
crs <- st_crs(st_read("./classificador_vol_america/vect/limit.gpkg"))
# n.cores <- round(detectCores()-(detectCores()/2))

n.cores <- detectCores()
free.mem <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024
if(ceiling(sqrt(free.mem)/(252/2))<=n.cores){
  n.cores<-ceiling(sqrt(free.mem)/(252/2))
}
# n.cores <- 3
source("./classificador_vol_america/scripts/vectorise_raster.R")
unlink("./classificador_vol_america/logs/vectorise_raster_do.txt")
cl <- create_cluster_clump_(n.cores,"vectorise_raster_do")
parLapplyLB(cl,files_list, vectorise_raster_do_,crs)
# lapply(files_list, vectorise_raster_do_,crs)
stopCluster(cl)
rm(cl)
unlink("./classificador_vol_america/logs/vectorise_raster_do.txt")

files_list <- list.files("./classificador_vol_america/rasters/smoothen2/split",pattern="\\.tif$")
# files_list <- files_list[!files_list%in%gsub(".rds",".tif",list.files("./classificador_vol_america/vect/vectorised/split/ori"))]
names(files_list) <- sapply(files_list,function(x){
  as.numeric(sub(".tif","",x))
})    

if(length(files_list)>0){
  rm(files_list)
  vectorise_raster_ini()
}
rm(files_list)

unlink("./classificador_vol_america/rasters/smoothen2/split",recursive=T)
rm(rasters_list)
# rm(rast)
# rm(vect)
gc()

vectorise_raster_current <- "vectorised"
save(vectorise_raster_current,file="./classificador_vol_america/temp/vectorise_raster_current.RData")

vectorise_raster_ini()
