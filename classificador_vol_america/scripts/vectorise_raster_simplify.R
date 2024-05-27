library(sf)
# print("vectorise_raster_simplify")
print(paste(Sys.time()," vectorise_raster_simplify"))
library(raster)

files_list <- list.files("./classificador_vol_america/vect/vectorised/split/ori",pattern="\\.rds$")
names(files_list) <- sapply(files_list,function(x){
  as.numeric(sub(".rds","",x))
})   

# load("./classificador_vol_america/temp/smoothen_raster_do_ids_failed.RData")
# 
# nottosimplify <- files_list[!names(files_list)%in%smoothen_raster_do_ids_failed]
# if(length(nottosimplify)>0){
#   n.cores <- detectCores()
#   unlink("./classificador_vol_america/logs/vectorise_raster_simplify_nottosimplify.txt")
#   cl <- create_cluster_clump_(n.cores,"vectorise_raster_simplify_nottosimplify")
#   minareas <- parLapplyLB(nottosimplify,function(file){
#     unlink(paste("./classificador_vol_america/vect/vectorised/split/simplified/",file,sep=""))
#     vect <- readRDS(paste("./classificador_vol_america/vect/vectorised/split/ori/",file,sep=""))
#     minarea <- min(st_area(vect))
#     writeRDS(vect,
#              paste("./classificador_vol_america/vect/vectorised/split/simplified/",file,sep=""))
#     unlink(paste("./classificador_vol_america/vect/vectorised/split/ori/",file,sep=""))
#     minarea
#   })
#   save(min(minareas),"./classificador_vol_america/temp/min_smoothened_regular_area.RData")
#   rm(minareas)
#   stopCluster(cl)
# }
# rm(nottosimplify)
# 
# tosimplify <- files_list[names(files_list)%in%smoothen_raster_do_ids_failed]
if(length(files_list)>0){
  dir.create("./classificador_vol_america/vect/clumped/",showWarnings=F)
  dir.create("./classificador_vol_america/vect/clumped/temp/",showWarnings=F)
  dir.create("./classificador_vol_america/vect/temp/")
  dir.create("./classificador_vol_america/rasters/temp/")
  dir.create("./classificador_vol_america/vect/vectorised/split/simplified", showWarnings = FALSE)
  rast <- raster("./classificador_vol_america/rasters/original/original.tif")
  source("./classificador_vol_america/scripts/clump_vector.R")
  crs <- st_crs(st_read("./classificador_vol_america/vect/limit.gpkg"))
  # source("./classificador_vol_america/scripts/vectorise_raster_simplify_function.R")
  source("./classificador_vol_america/scripts/vectorise_raster_simplify_function.R")
  print(paste(length(files_list)," raster files need to be simplified",sep=""))
  # load("./classificador_vol_america/temp/resolution.RData")
  load("./classificador_vol_america/temp/resolution.RData")
  if(length(files_list)<detectCores()){
    lapply(files_list,simplify_file,rast,crs,38*resolution,log=T)
  }else{
    unlink("./classificador_vol_america/logs/vectorise_raster_simplify_parallel.txt")
    library(parallel)
    n.cores <- detectCores()
    
    # install.packages("BAMMtools")
    library(BAMMtools)
    unlink("./classificador_vol_america/logs/vectorise_raster_simplify_parallel_nrows.txt")
    cl <- create_cluster_clump_(n.cores,"vectorise_raster_simplify_parallel_nrows")
    nrows <- parSapplyLB(cl,files_list,function(f){
      nrow(readRDS(paste("./classificador_vol_america/vect/vectorised/split/ori/",f,sep="")))
    })
    stopCluster(cl)
    rm(cl)
    unlink("./classificador_vol_america/logs/vectorise_raster_simplify_parallel_nrows.txt")
    
    breaks <- getJenksBreaks(nrows, 4)
    short <- nrows[nrows<=breaks[2]]
    short <- files_list[names(files_list)%in%names(short)]
    short <- split(short, cut(seq_along(short),n.cores,labels=F))
    
    #TODO: order by length and apply the following:
    # ids <- ids[order(nrows,decreasing=T)]
    # ids <- split(ids,ceiling(seq_along(ids)/n.cores))
    # for(ii in 1:length(ids)){
    #   if(ii%%2!=0){
    #     ids[[ii]] <- rev(ids[[ii]])
    #   }
    # }
    # ids <- lapply((1:n.cores),function(n.core,ids){sapply(ids,function(ii,n.core){
    #   ii[n.core]
    # },n.core)},ids)
    # ids <- lapply(ids,function(ii){
    #   ii[!is.na(ii)]
    # })
    
    
    
    long <- nrows[nrows>breaks[2]]
    long <- files_list[names(files_list)%in%names(long)]
    
    
    long <- split(long, cut(seq_along(long),n.cores,labels=F))
    
    files_list <- mapply(function(s,l){append(s,l)},short,long,SIMPLIFY=F)
    files_list <- lapply(files_list,function(ff){
      ff[order(as.numeric(names(ff)))]
    })
    rm(breaks)
    rm(short)
    rm(long)
    rm(nrows)
    # files_list <- split(files_list, cut(seq_along(files_list),n.cores,labels=F))
    
    unlink("./classificador_vol_america/logs/vectorise_raster_simplify_parallel.txt")
    cl <- create_cluster_clump_(n.cores,"vectorise_raster_simplify_parallel")
    parLapplyLB(cl,files_list,function(files,rast,crs,resolution){
      lapply(files,simplify_file,rast,crs,arealimit=38*resolution,parallel=F,dosmall=T,log=F)
    },rast,crs,resolution)
    stopCluster(cl)
    rm(cl)
    unlink("./classificador_vol_america/logs/vectorise_raster_simplify_parallel.txt")
  }
  unlink("./classificador_vol_america/temp/vects_clumped/")
  gc()
}
rm(files_list)


small <- list.files("./classificador_vol_america/vect/vectorised/split/ori/small",pattern="\\.rds$")
names(small) <- sapply(small,function(x){
  as.numeric(sub(".rds","",x))
})    
if(length(small)>0){
  print(paste(length(small)," small raster files need to be simplified",sep=""))
  source("./classificador_vol_america/scripts/vectorise_raster_simplify_function.R")
  load("./classificador_vol_america/temp/resolution.RData")
  source("./classificador_vol_america/scripts/clump_vector.R")
  rast <- raster("./classificador_vol_america/rasters/original/original.tif")
  n.cores <- detectCores()
  unlink("./classificador_vol_america/logs/vectorise_raster_simplify_small.txt")
  cl <- create_cluster_clump_(n.cores,"vectorise_raster_simplify_small")
  
  parLapplyLB(cl,small,simplify_file,rast,crs,38*resolution,
              readdir="./classificador_vol_america/vect/vectorised/split/ori/small/",
              savedir="./classificador_vol_america/vect/vectorised/split/simplified/",
              dosmall=T,parallel=F,log=T)
  stopCluster(cl)
  rm(cl)
  unlink("./classificador_vol_america/logs/vectorise_raster_simplify_small.txt")
}
rm(small)
rm(rast)

vectorise_raster_current <- "simplified"
save(vectorise_raster_current,file="./classificador_vol_america/temp/vectorise_raster_current.RData")
source("./classificador_vol_america/scripts/vectorise_raster.R")
vectorise_raster_ini()

