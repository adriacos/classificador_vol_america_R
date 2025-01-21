
library(parallel)
library(raster)
# source("./scripts/read_data.R")
# source("./scripts/save_data.R")
# source("./scripts/create_ortofoto_leaflet.R")
# source("./scripts/export_rasters.R")
source("./scripts/smoothen_raster.R")
source("./scripts/clump_vector.R")
source("./scripts/vectorise_raster.R")
# source("./scripts/calc_metrics.R")
# source("./scripts/create_export_ortofoto_rasters.R")
# source("./scripts/create_export_1956_class_diba_rasters.R")

ini <- function(raster,minresolution,max_value,minresolution,limit=NULL){
  print(paste(Sys.time()," smoothen_raster_ini"))
  # print("smoothen_raster_ini")
  ini_set(raster,minresolution,max_value,limit)
  library(rstudioapi)
  restartSession(command=source("./scripts/continue.R"))
  # continue()
}

# continue <- function(){
#   load("./temp/current_global.RData")
#   if(current_global=="set"){
#     smoothen_raster_ini()
#   }else if(current_global=="smoothen"){
#     vectorise_raster_ini()
#   }else if(current_global=="vectorised"){
#     clump_vector_ini()
#   }else if(current_global=="clumped"){
#     # clump_vector_ini()
#   }
# }


ini_set <- function(raster,minresolution,max_val,limit=NULL){
  print(paste(Sys.time()," ini_set"))
  library(raster)
  dir.create("./rasters/original/",showWarnings=F)
  # writeRaster(raster,"./rasters/original/original.tif")
  unlink("./temp",recursive=T)
  unlink("./logs",recursive=T)
  dir.create("./temp",recursive=T)
  dir.create("./logs",recursive=T)
  raster <- raster("./rasters/original/original.tif")
  resolution <- prod(res(raster))
  # resolution <- mean(res(raster))
  save(resolution,file="./temp/resolution.RData")
  minres <- minresolution
  save(minres,file="./temp/minres.RData")
  
  save(max_val,file="./temp/max_val.RData")
  
  rm(original_recalc)
  rm(rast)
  rm(resolution)
  rm(minresolution)
  rm(minres)
  if(is.null(limit)){
    #TODO: get limit from raster
  }else{
    # limit <- st_transform(limit,st_crs(raster))
  }
  st_write(limit,"./vect/limit.gpkg")
  rm(limit)
  
  normalise_original(raster)
  
  
  gc()
  current_global <- "set"
  save(current_global,file="./temp/current_global.RData")
  # continue()
}

normalise_original <- function(original,max_val,resolution){
  
  unlink("./rasters/original/normalise",recursive=T)
  dir.create("./rasters/original/normalise")
  dir.create("./rasters/original/normalise/split")
  
  limit <- st_read("./vect/limit.gpkg")
  area <- as.numeric(st_area(limit))
  library(parallel)
  source("./scripts/clump_vector.R")
  factor <- sqrt(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024/12000)
  normalise_raster_grid <- create_grid(limit,round(sqrt(area)/(factor*resolution*30000)),round(sqrt(area)/(factor*resolution*30000)))
  n.cores <- detectCores()
  cl <- create_cluster_clump_(n.cores,"normalise_raster")
  parLapplyLB(cl,normalise_raster_grid$id,function(id, normalise_raster_grid, rast){
    print(normalise_raster_grid[normalise_raster_grid$id==id,]$id)
    # buf <- st_buffer(smoothen_raster_grid[smoothen_raster_grid$id==id,], 10)
    library(raster)
    crop <- mask(crop(rast,normalise_raster_grid[normalise_raster_grid$id==id,]),
                 normalise_raster_grid[normalise_raster_grid$id==id,])
    unlink(paste("./rasters/original/normalise/split/",id,".tif",sep=""))
    writeRaster(crop,paste("./rasters/original/normalise/split/",id,".tif",sep=""))
    # crop
  },normalise_raster_grid,original)
  stopCluster(cl)
  rm(cl)
  unlink("./logs/normalise_raster.txt")
  rm(normalise_raster_grid)
  
  dir.create("./rasters/original/normalise/split/normalised")
  files <- list.files("./rasters/original/normalise/split",pattern=".tif$")
  res <- lapply(files,function(file){
    print(file)
    rast <- raster(paste("./rasters/original/normalise/split/",file,sep=""))
    values(rast) <- 10*values(rast)/max_val
    writeRaster(rast,paste("./rasters/original/normalise/split/normalised/",file,sep=""))
    rm(rast)
    gc()
    return(TRUE)
  })
  if(any(res==F)){
    stop("There was an error normalising the raster")
  }
  rm(files)
  
  rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))-(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/10)
  
  rs <- lapply(list.files("./rasters/original/normalise/split/normalised",pattern=".tif$"),function(file){
    raster(paste("./rasters/original/normalise/split/normalised/",file,sep=""))
  })
  library(stringr)
  # ids <- as.numeric(sapply(str_split(names(rs[1]),"_"),function(f){
  #   f[f!=""][1]
  # }))
  merged <- rs[[1]]
  for(i in 2:length(rs)){
    # print(i)
    r2 <- rs[[i]]
    oldlocation <- merged@file@name
    if(!is.null(rasterinmemory)){
      rasterOptions(maxmemory=rasterinmemory)
    }
    merged <- merge(merged, r2)
    newlocation <- merged@file@name
    if(i!=2&oldlocation!=""&oldlocation!=newlocation){
      unlink(oldlocation)
      unlink(sub(".grd",".gri",oldlocation))
    }
    gc()
    print(paste(i,"merged"))
    if(i==length(rs)){
      writeRaster(merged, "./rasters/original/original_normalised.tif",overwrite=T)
      unlink(newlocation)
      unlink(sub(".grd",".gri",newlocation))
    }
  }
}

# 
# ini__ <- function(){
#   #maximum number of parallel clump_vector that can be done. Theoretically equal 
#   #to the number of cores, but practically limited to 5 due to memory issues
#   #cores <- detectCores()
#   cores <- 5
#   
#   while(TRUE){
#     #try_export_corrupted()
#     smoothen_rasters_all()
#     save_ortofotos_to_rasters()
#   }
#   return(0)
#   
#   ids <- read_quad_ids()
#   done <- get_done_ids() 
#   while(length(done)<length(ids)){
#     #auto_class_BCN_all()
#     #calc_metrics_all()
#     vectorised <- get_vectorised_ids()
#     if(length(vectorised)>3*cores){
#       clump_vectors_all()
#     }
#     smoothen_rasters_all()
#     vectorise_rasters_all()
#     vectorised <- get_vectorised_ids()
#     if(length(vectorised)>3*cores){
#       clump_vectors_all()
#     }
#     save_ortofotos_to_rasters()
#     done <- get_done_ids()
#   }
#     
#     # if(length(vectorised)>0){
#     #   res <- length(vectorised)%%cores
#     #   if(res==0){
#     #     clump_vectors_all()
#     #   }else{
#     #     vectorise_rasters_all(cores-res)
#     #     vectorised <- get_vectorised_ids()
#     #     res <- length(vectorised)%%cores
#     #     if(res==0){
#     #       clump_vectors_all()
#     #     }else{
#     #       smoothen_rasters_all(cores-res)
#     #       vectorise_rasters_all()
#     #       vectorised <- get_vectorised_ids()
#     #       res <- length(vectorised)%%cores
#     #       if(res==0){
#     #         clump_vectors_all()
#     #       }else{
#     #         save_ortofotos_to_rasters(cores-res)
#     #         smoothen_rasters_all()
#     #         vectorise_rasters_all()
#     #       }
#     #     }
#     #   }
#     # }else{
#     #   smoothen <- get_smoothen_ids()
#     #   if(length(smoothen)>0){
#     #     res <- length(smoothen)%%cores
#     #     if(res==0){
#     #       vectorise_rasters_all()
#     #       next()
#     #     }
#     #     else{
#     #       smoothen_rasters_all(cores-res)
#     #       vectorise_rasters_all()
#     #       next()
#     #     }
#     #   }else{
#     #     exported <- get_exported_ids()
#     #     smoothen <- get_smoothen_ids()
#     #     vectorised <- get_vectorised_ids()
#     #     exported <- exported[!exported %in% smoothen]
#     #     exported <- exported[!exported %in% vectorised]
#     #     if(length(exported)>0){
#     #       smoothen_rasters_all()
#     #       vectorise_rasters_all()
#     #       next()  
#     #     }
#     #   }
#     # }
#     # save_ortofotos_to_rasters()
#     # done <- get_done_ids()
#   # }
# }
# 
# save_1956_diba_to_rasters <- function(){
#   ids <- read_quad_ids()
#   vects <- get_quad_vect(ids)
#   vects <- reproject_EPSG_4258_vect(vects)
#   vects <- buffer(vects, width=200, dissolve=T)
#   
#   if(length(vects)!= length(ids)){
#     print("ALARM - vects length != ids length")
#     stop()
#   }
#   
#   coordinates <- get_vectors_centroids_coords(vects)
#   #coordinates <- get_EPSG_4258_vectors_centroids_lat_lng(vects)
#   lats <- coordinates[,2]
#   lngs <- coordinates[,1]
#   rm(coordinates)
#   rm(vects)
#   gc()
#   
#   mapply(create_export_1956_class_diba_leaflet, ids, lats, lngs)
#   
#   rm(lat)
#   rm(long)
# }
# 
# save_ortofotos_to_rasters <- function(n=NULL){
#   print("save_ortofotos_to_rasters")
#   # done <- get_done_ids() 
#   smoothen <- get_smoothen_ids() 
#   corrupted <- get_corrupted_ids() 
#   
#   ids <- read_quad_ids_not_exported(notin=append(corrupted, smoothen))
#   
#   # all <- read_quad_ids()
#   # 
#   # if(dir.exists("C:/Users/acosd/Desktop/CREAF/Mapes/Quadricula")){
#   #   ids <- ids[ids %in% all[round((length(all)/4)*3):length(all)]]
#   #   ids <- rev(ids)
#   # }else if(dir.exists("C:/Users/a.cos/Documents/Tesi/DADES/Quadricula")){
#   #   ids <- ids[ids %in% all[1:(length(all)/2)-1]]
#   # }
#   # # else if(dir.exists("./data/Quadricula")){
#   #   # ids <- ids[ids %in% all[(length(all)/2):(round((length(all)/4)*3)-1)]]
#   #   # ids <- rev(ids)
#   # # }
#   
#   # if(!is.null(n)){
#   #   ids <- ids[1:n]
#   # }else{
#   #   ids <- ids[1:50]
#   # }
#   ids <- sort(ids)
#   
#   rm(done)
#   vects <- get_quad_vect(ids)
#   vects <- reproject_EPSG_4258_vect(vects)
#   
#   if(length(vects)!= length(ids)){
#     print("ALARM - vects length != ids length")
#     stop()
#   }
#   
#   coordinates <- get_vectors_centroids_coords(vects)
#   lats <- coordinates[,2]
#   lngs <- coordinates[,1]
#   rm(coordinates)
#   rm(vects)
#   gc()
# 
#   res <- mapply(create_export_ortofoto_raster, ids, lats, lngs)
#   nulls <- length(sapply(res, is.null)[sapply(res, is.null)==T])
#   if(nulls>0){
#     save_ortofotos_to_rasters(nulls)
#   }
#   
#   rm(lat)
#   rm(long)
#   gc()
# }
# 
# try_export_corrupted <- function(){
#   ids <- get_corrupted_ids() 
#   ids <- unique(ids)
#   ids <- sort(ids)
#   vects <- get_quad_vect(ids)
#   vects <- reproject_EPSG_4258_vect(vects)
#   
#   if(length(vects)!= length(ids)){
#     print("ALARM - vects length != ids length")
#     stop()
#   }
#   
#   coordinates <- get_vectors_centroids_coords(vects)
#   lats <- coordinates[,2]
#   lngs <- coordinates[,1]
#   rm(coordinates)
#   rm(vects)
#   gc()
#   
#   mapply(create_export_ortofoto_raster, ids, lats, lngs)
#   
#   rm(lat)
#   rm(long)
#   gc()
# }
# 
# smoothen_rasters_all <- function(n=NULL){
#   print("smoothen_rasters_all")
#   #ids <- get_raster_ids_done_not_smoothen()
#   ids <- get_exported_ids()
#   smoothen <- get_smoothen_ids()
#   #vectorised <- get_vectorised_ids()
#   ids <- ids[!ids %in% smoothen]
#   #ids <- ids[!ids %in% vectorised]
#   if(length(ids)==0){
#     return(NULL)
#   }
#   if(!is.null(n)){
#     ids <- ids[1:n]
#   }
#  
#   sapply(ids, smoothen_raster)
# }
# 
# vectorise_rasters_all <- function(n=NULL){
#   print("vectorise_rasters_all")
#   #ids <- get_raster_ids_smoothen_done()
#   ids <- get_smoothen_ids()
#   if(length(ids)==0){
#     return(NULL)
#   }
#   if(!is.null(n)){
#     ids <- ids[1:n]
#   }
#   
#   cl <- makeCluster(5, outfile="log_vectorise.txt")
#   clusterExport(cl, c("ids", "vectorise_save_smoothen_raster"), envir = environment())
#   clusterEvalQ(cl, list(source("./scripts/vectorise_raster.R"), library(raster), library(terra), library(rgdal)))
#   parLapplyLB(cl, ids, vectorise_save_smoothen_raster)
#   #vectorise_raster(rast)
#   stopCluster(cl)
# }
# 
# clump_vectors_all <- function(){
#   print("clump_vectors_all")
#   cores <- 5
#   vects <- get_vectorised_vectors()
#   vects <- vects[order(sapply(vects, nrow), decreasing=T)]
#   ids <- names(vects)
#   
#   if(length(ids)==0){
#     return(NULL)
#   }
#   print(ids)
#     cl <- makeCluster(cores, outfile="log_clump.txt")
#     clusterEvalQ(cl, list(source("./scripts/clump_vector.R"), library(rgdal), library(rgeos), library(stringr), library(maptools)))
#     clusterExport(cl, c("ids", "clump_vector"), envir = environment())
#     parLapplyLB(cl, ids, clump_vector)
#     stopCluster(cl)
# }
# 
# auto_class_BCN_all <- function(){
#   cores <- 5
#   ids <- get_metrics_ids()
#   if(length(ids)==0){
#     return(NULL)
#   }
#   vects <- get_metrics_vectors()
#   if(length(ids)!=length(vectors)){
#     print("auto_class_BCN_all - ids length different than vectors length")
#     stop()
#   }
#   cl <- makeCluster(cores, outfile="ini_log.txt")
#   clusterExport(cl, c("ids","vects"), envir = environment())
#   clusterEvalQ(cl, list(source("./scripts/auto_class_BCN.R"), source("./scripts/read_data.R"), library(exactextractr)))
#   vects <- clusterMap(cl, auto_class_BCN, vects, ids)
#   stopCluster(cl)
#   save_id_done(ids)
# }
# 
# calc_metrics_all <- function(){
#   print("calc_metrics_all")
#   #ids <- get_vectors_clumped_file_ids_not_metrics()
#   ids <- get_clumped_ids()
#   if(length(ids)==0){
#     return(NULL)
#   }
#   
#   elev <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Elevacions/elevacions_CAT.tif")
#   pend <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Elevacions/pendent_CAT.tif")
#   clima.mean_temp <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_TMPANUAL/ATMOSFERA_ATLES6190_TMPANUAL_5mx5m.tif")
#   clima.amp_term <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_AMPTERMI/ATMOSFERA_ATLES6190_AMPTERMI_5mx5m.tif")
#   clima.mean_prec <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_PPTANUAL/ATMOSFERA_ATLES6190_PPTANUAL_5mx5m.tif")
#   clima.reg_pluv <- raster("C:/Users/acosd/Desktop/CREAF/Mapes/Clima/ATMOSFERA_ATLES6190_REGPLUVI/ATMOSFERA_ATLES6190_REGPLUVI_5mx5m.tif")
# 
#   print(ids)
#   
#   cl <- makeCluster(5, outfile="log_metrics.txt")
#   clusterExport(cl, c("ids", "calc_metrics", "elev", "pend", "clima.mean_temp","clima.amp_term","clima.mean_prec","clima.reg_pluv"), envir = environment())
#   clusterEvalQ(cl, list(source("./scripts/calc_metrics.R"), library(raster), library(terra), library(rgdal)))
#   parLapplyLB(cl, ids, calc_metrics, elev, pend, clima.mean_temp,clima.amp_term,clima.mean_prec,clima.reg_pluv)
#   stopCluster(cl)
# }