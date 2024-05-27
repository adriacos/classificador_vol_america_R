
library(parallel)
library(raster)
# source("./classificador_vol_america/scripts/read_data.R")
# source("./classificador_vol_america/scripts/save_data.R")
# source("./classificador_vol_america/scripts/create_ortofoto_leaflet.R")
# source("./classificador_vol_america/scripts/export_rasters.R")
source("./classificador_vol_america/scripts/smoothen_raster.R")
source("./classificador_vol_america/scripts/clump_vector.R")
source("./classificador_vol_america/scripts/vectorise_raster.R")
# source("./classificador_vol_america/scripts/calc_metrics.R")
# source("./classificador_vol_america/scripts/create_export_ortofoto_rasters.R")
# source("./classificador_vol_america/scripts/create_export_1956_class_diba_rasters.R")

ini <- function(raster,minresolution,max_value,minresolution,limit=NULL){
  print(paste(Sys.time()," ini"))
  # print("smoothen_raster_ini")
  ini_set(raster,minresolution,max_value,limit)
  library(rstudioapi)
  restartSession(command=source("./classificador_vol_america/scripts/continue.R"))
  # continue()
}

# continue <- function(){
#   load("./classificador_vol_america/temp/current_global.RData")
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
  dir.create("./classificador_vol_america/rasters/original/",showWarnings=F)
  # writeRaster(raster,"./classificador_vol_america/rasters/original/original.tif")
  unlink("./classificador_vol_america/temp",recursive=T)
  unlink("./classificador_vol_america/logs",recursive=T)
  dir.create("./classificador_vol_america/temp",recursive=T)
  dir.create("./classificador_vol_america/logs",recursive=T)
  raster <- raster("./classificador_vol_america/rasters/original/original.tif")
  resolution <- prod(res(raster))
  # resolution <- mean(res(raster))
  save(resolution,file="./classificador_vol_america/temp/resolution.RData")
  minres <- minresolution
  save(minres,file="./classificador_vol_america/temp/minres.RData")
  
  save(max_val,file="./classificador_vol_america/temp/max_val.RData")
  
  rm(rast)
  rm(resolution)
  rm(minresolution)
  rm(minres)
  if(is.null(limit)){
    #TODO: get limit from raster
  }else{
    # limit <- st_transform(limit,st_crs(raster))
  }
  limit <- st_read("./classificador_vol_america/vect/limit_bcn.gpkg")
  st_write(limit,"./classificador_vol_america/vect/limit.gpkg")
  rm(limit)
  
  normalise_original()
  
  
  gc()
  current_global <- "set"
  save(current_global,file="./classificador_vol_america/temp/current_global.RData")
  # continue()
}

normalise_original <- function(){
  
  original <- raster("./classificador_vol_america/rasters/original/original.tif")
  load("./classificador_vol_america/temp/resolution.RData")
  load("./classificador_vol_america/temp/max_val.RData")
  
  unlink("./classificador_vol_america/rasters/original/normalise",recursive=T)
  dir.create("./classificador_vol_america/rasters/original/normalise")
  dir.create("./classificador_vol_america/rasters/original/normalise/split")
  limit <- st_read("./classificador_vol_america/vect/limit.gpkg")
  area <- as.numeric(st_area(limit))
  library(parallel)
  source("./classificador_vol_america/scripts/clump_vector.R")
  factor <- sqrt(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024/12000)/(1.6*sqrt(detectCores()))
  normalise_raster_grid <- create_grid(limit,round(sqrt(area)/(factor*resolution*30000)),round(sqrt(area)/(factor*resolution*30000)))
  n.cores <- detectCores()
  unlink("./classificador_vol_america/logs/normalise_raster.txt")
  cl <- create_cluster_clump_(n.cores,"normalise_raster")
  parLapplyLB(cl,normalise_raster_grid$id,function(id, normalise_raster_grid,rast){
  # parLapplyLB(cl,c(114,2,3,4,5,6,7,12,13,14,15,16,17,18,23,24,26),function(id, normalise_raster_grid,rast){
    print(normalise_raster_grid[normalise_raster_grid$id==id,]$id)
    # buf <- st_buffer(smoothen_raster_grid[smoothen_raster_grid$id==id,], 10)
    # library(raster)
    unlink(paste("./classificador_vol_america/rasters/original/normalise/split/",id,".tif",sep=""))
    crop <- crop(rast,normalise_raster_grid[normalise_raster_grid$id==id,])
    mask <- mask(crop,normalise_raster_grid[normalise_raster_grid$id==id,])
    if(crop@file@name!=""){
      unlink(crop@file@name)
      unlink(gsub(".grd",".gri",crop@file@name))
    }
    rm(crop)
    writeRaster(mask,paste("./classificador_vol_america/rasters/original/normalise/split/",id,".tif",sep=""))
    if(mask@file@name!=""){
      unlink(mask@file@name)
      unlink(gsub(".grd",".gri",mask@file@name))
    }
    lapply(str_match(list.files(paste(str_split(mask@file@name,"/")[[1]][1:length(str_split(mask@file@name,"/")[[1]])-1],collapse="/"),full.names=T),
              paste(paste(str_split(mask@file@name,"_")[[1]][1:3],collapse="_"),"_",
                    "\\d*","_",str_split(mask@file@name,"_")[[1]][length(str_split(mask@file@name,"_")[[1]])-1],"_",
                    "\\d*",".\\w*",sep=""))[,1],unlink)
    rm(mask)
    # crop
    gc()
    return(T)
  },normalise_raster_grid,original)
  stopCluster(cl)
  rm(cl)
  gc()
  unlink("./classificador_vol_america/logs/normalise_raster.txt")
  rm(normalise_raster_grid)
  
  unlink("./classificador_vol_america/rasters/original/normalise/split/normalised",recursive=T)
  dir.create("./classificador_vol_america/rasters/original/normalise/split/normalised")
  files <- list.files("./classificador_vol_america/rasters/original/normalise/split",pattern=".tif$")
  free.mem <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024
  n.cores <- min(detectCores(),round(16*free.mem/27000))
  rm(free.mem)
  tmp <- ceiling((1:length(files))/(length(files)/n.cores))
  files_spl <- split(files,tmp)
  rm(tmp)
  source("./classificador_vol_america/scripts/clump_vector.R")
  unlink("./classificador_vol_america/logs/normalise_original_raster.txt")
  clust <- create_cluster_clump_(n.cores,"normalise_original_raster") 
  res <- parLapplyLB(clust,files_spl,function(files){
    sapply(files,function(file){
      print(file)
      rast <- raster(paste("./classificador_vol_america/rasters/original/normalise/split/",file,sep=""))
      values(rast) <- 10*values(rast)/max_val
      writeRaster(rast,paste("./classificador_vol_america/rasters/original/normalise/split/normalised/",file,sep=""))
      rm(rast)
      gc()
      return(TRUE)
    })
  })
  stopCluster(clust)
  rm(clust)
  gc()
  unlink("./classificador_vol_america/logs/normalise_original_raster.txt")
  
  if(any(unlist(res)==F)){
    stop("There was an error normalising the raster")
  }
  rm(files)
  
  files <- list.files("./classificador_vol_america/rasters/original/normalise/split",pattern=".tif$")
  lapply(files,function(f){
    unlink(paste("./classificador_vol_america/rasters/original/normalise/split",f,sep=""))
  })
  
  # unlink("./classificador_vol_america/rasters/original/normalise/merged",recursive=T)
  # dir.create("./classificador_vol_america/rasters/original/normalise/merged")
  rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))-(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/(detectCores()*2))
  merge_rasters_ <- function(fs,r,rasterinmemory=NULL){
    if(!is.null(rasterinmemory)){
      # print("rrrr")
      rasterOptions(maxmemory=rasterinmemory)
    }
    # for(fs in files_split){
      rs <- lapply(fs,function(f){
        if(!is.null(rasterinmemory)){
          rasterOptions(maxmemory=rasterinmemory)
        }
        raster(paste("./classificador_vol_america/rasters/original/normalise/split/normalised/",f,sep=""))
      })
      if(length(rs)>1){
        ids <- as.numeric(sapply(str_split(names(rs[1]),"_"),function(f){
          f[f!=""][1]
        }))
        merged <- rs[[1]]
        for(i in 2:length(rs)){
          # print(i)
          r2 <- rs[[i]]
          ids <- append(ids, as.numeric(sapply(str_split(names(rs[i]),"_"),function(f){
            f[f!=""][length(f[f!=""])]
          })))
          if(length(ids)==1){
            newname <- paste(ids[1],".tif",sep="")
          }else{
            newname <- paste(ids[1],"_",tail(ids,1),".tif",sep="")
          }
          # print(newname)
          if(i==2){ 
            oldname <- newname
          }
          oldlocation <- merged@file@name
          if(!is.null(rasterinmemory)){
            rasterOptions(maxmemory=rasterinmemory)
          }
          merged <- mosaic(merged,r2,fun=mean)
          rm(r2)
          gc()
          newlocation <- merged@file@name
          if(i!=2&oldlocation!=""&oldlocation!=newlocation){
            unlink(oldlocation)
            unlink(sub(".grd",".gri",oldlocation))
            print(paste(newname,i,"deleted old temp file"))
          }
          print(paste(newname,i,"merged"))
          if(i==length(rs)){
            writeRaster(merged, paste("./classificador_vol_america/rasters/original/normalise/split/normalised/",paste(rep("_",r),collapse=""),newname,sep=""),overwrite=T)
            print(paste(newname,"saved")) 
            unlink(newlocation)
            unlink(sub(".grd",".gri",newlocation))
            # lapply(fs,function(f){
            #   unlink(paste("./classificador_vol_america/rasters/original/normalise/split/normalised/",f,sep=""))
            # })
          }
        }
      }else if(length(rs)==1){
        writeRaster(rs[[1]], paste("./classificador_vol_america/rasters/original/normalise/split/normalised/","_",fs,sep=""),overwrite=T)
        # unlink(paste("./classificador_vol_america/rasters/original/normalise/split/normalised/",fs,sep=""))
      }
    # }
  }
  
  r <- 1
  repeat{
    print(paste("r",r,sep=""))
    files_all <- list.files("./classificador_vol_america/rasters/original/normalise/split/normalised",pattern="\\.tif$")
    names(files_all) <- sapply(files_all,function(f){
      sub(".tif","",f)
    })
    files_all <- files_all[order(as.numeric(gsub("_","",sapply(str_split(names(files_all),"_"),function(f){
      f[f!=""][1]
    }))))]
    if(length(files_all)<=1){
      break()
    }
    if(r==1){
      files <- files_all[!startsWith(names(files_all),"_")]
      print(paste(length(files),"files to merge"))
      files_split <- split(files,ceiling(seq_along(files)/9)) 
      rm(files)
    }else if(r==2){
      files <- files_all[startsWith(names(files_all),"_")&!startsWith(names(files_all),"__")]
      print(paste(length(files),"files to merge"))
      files_split <- split(files,ceiling(seq_along(files)/4))
      rm(files)
    }else if(r<=4){
      files <- files_all[startsWith(names(files_all),paste(rep("_",r-1),collapse=""))&!startsWith(names(files_all),paste(rep("_",r),collapse=""))]
      # files <- files_all[startsWith(names(files_all),"_")&!startsWith(names(files_all),"__")]
      print(paste(length(files),"files to merge"))
      files_split <- split(files,ceiling(seq_along(files)/3))
      rm(files)
    }else{
      files <- files_all[startsWith(names(files_all),paste(rep("_",r-1),collapse=""))&!startsWith(names(files_all),paste(rep("_",r),collapse=""))]
      print(paste(length(files),"files to merge"))
      files_split <- split(files,ceiling(seq_along(files)/2))
      rm(files)
    }
    
    rm(files_all)
    if(length(files_split)>0){
      if(length(files_split)>1&length(files_split[[length(files_split)]])==1){
        files_split[[length(files_split)-1]] <- append(files_split[[length(files_split)-1]],
                                                       files_split[[length(files_split)]]) 
        files_split <- files_split[-length(files_split)]
      }
      # if(r>1){
      #   rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1.5
      #   merge_rasters_(files_split,r,rasterinmemory)
      # }else{
        if(length(files_split)>1){
          n.cores <- detectCores()
          free.mem <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024
          n.cores <- min(length(files_split),min(detectCores(),ceiling(10*free.mem/64000/(r/sqrt(r-1)))))
          if(r==4){
            n.cores <- 3
          }else{
            n.cores <- 1
          }
          # n.cores <- 8
          rm(free.mem)
          # files_split <- split(files_split, cut(seq_along(files_split),n.cores,labels=F))
          # rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024/(n.cores*r)
          rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024/(n.cores*1.6)
          source("./classificador_vol_america/scripts/clump_vector.R")
          unlink("./classificador_vol_america/logs/normalise_original_merge.txt")
          cl <- create_cluster_clump_(n.cores,"normalise_original_merge")
          tt <- parLapplyLB(cl,files_split,function(fs,r,rasterinmemory){
            merge_rasters_(fs,r,rasterinmemory)
          },r,rasterinmemory)
          stopCluster(cl)
          rm(cl)    
          gc()
          unlink("./classificador_vol_america/logs/normalise_original_merge.txt")
          rm(tt)
        }else{
          rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024*1.5
          merge_rasters_(files_split[[1]],r,rasterinmemory)
        }
      # }
    }
    rm(files_split)
    r <- r+1
  }
  rm(r)
  
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
#   # # else if(dir.exists("./classificador_vol_america/data/Quadricula")){
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
#   clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/vectorise_raster.R"), library(raster), library(terra), library(rgdal)))
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
#     clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/clump_vector.R"), library(rgdal), library(rgeos), library(stringr), library(maptools)))
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
#   clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/auto_class_BCN.R"), source("./classificador_vol_america/scripts/read_data.R"), library(exactextractr)))
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
#   clusterEvalQ(cl, list(source("./classificador_vol_america/scripts/calc_metrics.R"), library(raster), library(terra), library(rgdal)))
#   parLapplyLB(cl, ids, calc_metrics, elev, pend, clima.mean_temp,clima.amp_term,clima.mean_prec,clima.reg_pluv)
#   stopCluster(cl)
# }