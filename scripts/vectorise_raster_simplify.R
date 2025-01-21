library(sf)
# print("vectorise_raster_simplify")
print(paste(Sys.time()," vectorise_raster_simplify"))
library(raster)

files_list <- list.files("./vect/vectorised/split/ori",pattern="\\.rds$")
names(files_list) <- sapply(files_list,function(x){
  as.numeric(sub(".rds","",x))
})   

# load("./temp/smoothen_raster_do_ids_failed.RData")
# 
# nottosimplify <- files_list[!names(files_list)%in%smoothen_raster_do_ids_failed]
# if(length(nottosimplify)>0){
#   n.cores <- detectCores()
#   unlink("./logs/vectorise_raster_simplify_nottosimplify.txt")
#   cl <- create_cluster_clump_(n.cores,"vectorise_raster_simplify_nottosimplify")
#   minareas <- parLapplyLB(nottosimplify,function(file){
#     unlink(paste("./vect/vectorised/split/simplified/",file,sep=""))
#     vect <- readRDS(paste("./vect/vectorised/split/ori/",file,sep=""))
#     minarea <- min(st_area(vect))
#     writeRDS(vect,
#              paste("./vect/vectorised/split/simplified/",file,sep=""))
#     unlink(paste("./vect/vectorised/split/ori/",file,sep=""))
#     minarea
#   })
#   save(min(minareas),"./temp/min_smoothened_regular_area.RData")
#   rm(minareas)
#   stopCluster(cl)
# }
# rm(nottosimplify)
# 
# tosimplify <- files_list[names(files_list)%in%smoothen_raster_do_ids_failed]
if(length(files_list)>0){
  dir.create("./vect/clumped/",showWarnings=F)
  dir.create("./vect/clumped/temp/",showWarnings=F)
  dir.create("./vect/temp/")
  dir.create("./rasters/temp/")
  dir.create("./vect/vectorised/split/simplified", showWarnings = FALSE)
  rast <- raster("./rasters/original/original.tif")
  source("./scripts/clump_vector.R")
  crs <- st_crs(st_read("./vect/limit.gpkg"))
  # source("./scripts/vectorise_raster_simplify_function.R")
  source("./scripts/vectorise_raster_simplify_function.R")
  print(paste(length(files_list)," raster files need to be simplified",sep=""))
  # load("./temp/resolution.RData")
  load("./temp/resolution.RData")
  # if(length(files_list)<detectCores()){
  #   lapply(files_list,simplify_file,rast,crs,38*resolution,log=T)
  # }else{
    unlink("./logs/vectorise_raster_simplify_parallel.txt")
    # library(parallel)
    n.cores <- detectCores()
    # n.cores <- 10
    # install.packages("BAMMtools")
    library(BAMMtools)
    unlink("./logs/vectorise_raster_simplify_parallel_nrows.txt")
    cl <- create_cluster_clump_(n.cores,"vectorise_raster_simplify_parallel_nrows")
    nrows <- parSapplyLB(cl,files_list,function(f){
      print(f)
      nrow <- nrow(readRDS(paste("./vect/vectorised/split/ori/",f,sep="")))
      gc()
      return(nrow)
    })
    stopCluster(cl)
    rm(cl)
    gc()
    unlink("./logs/vectorise_raster_simplify_parallel_nrows.txt")
    
    if(min(nrows)>24000){
      short <- c()
      long <- files_list
    }else{
      # breaks <- getJenksBreaks(nrows, 4)
      short <- nrows[nrows<=24000]
      # short <- nrows[nrows<=breaks[2]]
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
      long <- nrows[nrows>24000]
      # long <- nrows[nrows>breaks[2]]
      long <- files_list[names(files_list)%in%names(long)]
      # long <- split(long, cut(seq_along(long),n.cores,labels=F))
    }
    if(!is.null(short)){
      print(paste(sum(sapply(short,length))," short raster to simplify in parallel",sep=""))
      unlink("./logs/vectorise_raster_simplify_parallel.txt")
      if(sum(sapply(short,length))==1){
        lapply(short,function(files,rast,crs,resolution){
          lapply(files,function(file,rast,crs,resolution){
            print(file)
            simplify_file(file,rast,crs,arealimit=38*resolution,parallel=T,dosmall=T,log=F)
          },rast,crs,resolution)
          # lapply(files,simplify_file,rast,crs,arealimit=38*resolution,parallel=F,dosmall=T,log=F)
        },rast,crs,resolution)
      }else{
        cl <- create_cluster_clump_(n.cores,"vectorise_raster_simplify_parallel")
        parLapplyLB(cl,short,function(files,rast,crs,resolution){
          lapply(files,function(file,rast,crs,resolution){
            print(file)
            simplify_file(file,rast,crs,arealimit=38*resolution,parallel=F,dosmall=T,log=F)
            gc()
          },rast,crs,resolution)
          # lapply(files,simplify_file,rast,crs,arealimit=38*resolution,parallel=F,dosmall=T,log=F)
        },rast,crs,resolution)
        stopCluster(cl)
        rm(cl)
        unlink("./logs/vectorise_raster_simplify_parallel.txt")
        gc()
      }
      gc()
    }
    
    if(!is.null(long)){
      print(paste(sum(sapply(long,length))," long raster to simplify in parallel",sep=""))
      lapply(long,function(file,rast,crs,resolution){
        unlink("./logs/simplify_file_notparalel.txt")
        source("./scripts/vectorise_raster_simplify_function.R")
        simplify_file(file,rast,crs,arealimit=38*resolution,parallel=T,dosmall=T,log=T,outfile="./logs/simplify_file_notparalel.txt")
        unlink("./logs/simplify_file_notparalel.txt")
        gc()
      },rast,crs,resolution)
      
      files_list <- mapply(function(s,l){append(s,l)},short,long,SIMPLIFY=F)
      files_list <- lapply(files_list,function(ff){
        ff[order(as.numeric(names(ff)))]
      })
    }
    rm(breaks)
    rm(short)
    rm(long)
    rm(nrows)
    # files_list <- split(files_list, cut(seq_along(files_list),n.cores,labels=F))
    
    # unlink("./logs/vectorise_raster_simplify_parallel.txt")
    # cl <- create_cluster_clump_(n.cores,"vectorise_raster_simplify_parallel")
    # parLapplyLB(cl,files_list,function(files,rast,crs,resolution){
    #   lapply(files,function(file,rast,crs,resolution){
    #     simplify_file(file,rast,crs,arealimit=38*resolution,parallel=F,dosmall=T,log=T)
    #   },rast,crs,resolution)
    #   # lapply(files,simplify_file,rast,crs,arealimit=38*resolution,parallel=F,dosmall=T,log=F)
    # },rast,crs,resolution)
    # stopCluster(cl)
    # rm(cl)
    # unlink("./logs/vectorise_raster_simplify_parallel.txt")
  # }
  unlink("./temp/vects_clumped/")
  gc()
}
rm(files_list)


small <- list.files("./vect/vectorised/split/ori/small",pattern="\\.rds$")
names(small) <- sapply(small,function(x){
  as.numeric(sub(".rds","",x))
})    
if(length(small)>0){
  print(paste(length(small)," small raster files need to be simplified",sep=""))
  source("./scripts/vectorise_raster_simplify_function.R")
  load("./temp/resolution.RData")
  source("./scripts/clump_vector.R")
  rast <- raster("./rasters/original/original.tif")
  n.cores <- detectCores()
  unlink("./logs/vectorise_raster_simplify_small.txt")
  cl <- create_cluster_clump_(n.cores,"vectorise_raster_simplify_small")
  
  parLapplyLB(cl,small,simplify_file,rast,crs,38*resolution,
              readdir="./vect/vectorised/split/ori/small/",
              savedir="./vect/vectorised/split/simplified/",
              dosmall=T,parallel=F,log=T)
  stopCluster(cl)
  rm(cl)
  unlink("./logs/vectorise_raster_simplify_small.txt")
}
rm(small)
rm(rast)

files_list <- list.files("./vect/vectorised/split/ori",pattern="\\.rds$")
if(length(files_list)>0){
  restartSession(command=source("./scripts/vectorise_raster_simplify.R"))
}
small <- list.files("./vect/vectorised/split/ori/small",pattern="\\.rds$")
if(length(small)>0){
  restartSession(command=source("./scripts/vectorise_raster_simplify.R"))
}

vectorise_raster_current <- "simplified"
save(vectorise_raster_current,file="./temp/vectorise_raster_current.RData")
rm(list=ls())
gc()
source("./scripts/vectorise_raster.R")
vectorise_raster_ini()

