source("./scripts/smoothen_raster.R")
source("./scripts/clump_vector.R")
# print("merge_supercells_rasters")
print(paste(Sys.time()," merge_supercells_rasters"))
#TODO: change name to "merge_supercells_rasters"
# dir <- "./rasters/smoothen/split/"
# dir <- "C:/Users/acosd/Desktop/CREAF/classifiador/LLE"
# dir <- "./rasters/original/normalise/split/normalised"
dir <- "./rasters/LLE"

test <- function(){
  merged <- raster("./rasters/BCN_GIR_LLE.tif")
  r2 <- raster("./rasters/TAR/TAR_normalised.tif")
  merged <- mosaic(merged,r2,fun="max",na.rm=T,filename="./rasters/CAT_normalised.tif")
  
}

rescale_big_raster <- function(r,maxval_ori,maxval_fin){
  limit <- st_as_sfc(st_bbox(raster(r)))
  grid <- create_grid(limit,2,2)
  split <- lapply(grid$id,function(id,grid,r){
    print(paste("rescale",grid[grid$id==id,]$id))
    limit <- grid[grid$id==id,]
    crop <- mask(crop(r,limit),limit)
    values(crop) <- maxval_fin*values(crop)/maxval_ori 
    crop
  },grid,r)
  merged <- split[[1]]
  for(i in 2:length(split)){
    merged <- mosaic(merged,split[[i]],fun="max",na.rm=T)
  }
  rm(split)
  rm(grid)
  rm(limit)
  gc()
  merged
}


merge_rasters_rescale_ <- function(files_split,dir,r,rasterinmemory=NULL){
  if(!is.null(rasterinmemory)){
    # print("rrrr")
    rasterOptions(maxmemory=rasterinmemory)
  }
  for(fs in files_split){
    rs <- lapply(fs,function(f){
      if(!is.null(rasterinmemory)){
        rasterOptions(maxmemory=rasterinmemory)
      }
      raster(paste(dir,"/",f,sep=""))
    })
    if(length(rs)>1){
      ids <- as.numeric(sapply(str_split(names(rs[1]),"_"),function(f){
        f[f!=""][1]
      }))
      merged <- rs[[1]]
      if(r==1){
        merged <- rescale_big_raster(merged,255,10)
        print("1 rescaled")
        # values(merged) <- 10*values(merged)/255
      }
      gc()
      for(i in 2:length(rs)){
        # print(i)
        r2 <- rs[[i]]
        if(r==1){
          r2 <- rescale_big_raster(r2,255,10)
          print(paste(i,"rescaled"))
          # values(r2) <- 10*values(r2)/255
        }
        gc()
        ids <- append(ids, as.numeric(sapply(str_split(names(rs[i]),"_"),function(f){
          f[f!=""][length(f[f!=""])]
        })))
        if(length(ids)==1){
          newname <- paste(ids[1],".tif",sep="")
        }else{
          newname <- paste(ids[1],"_",tail(ids,1),".tif",sep="")
        }
        if(i==2){
          oldname <- newname
        }
        oldlocation <- merged@file@name
        if(!is.null(rasterinmemory)){
          rasterOptions(maxmemory=rasterinmemory)
        }
        # merged <- merge(merged, r2)
        if(i==length(rs)){
          unlink(paste(dir,"/",paste(rep("_",r),collapse=""),newname,sep=""))
          merged <- mosaic(merged,r2,fun="max",na.rm=T,
                           filename=paste(dir,"/",paste(rep("_",r),collapse=""),newname,sep=""))
          print(paste(newname,"saved")) 
        }else{
          merged <- mosaic(merged,r2,fun="max",na.rm=T)
        }
        newlocation <- merged@file@name
        if(i!=2&oldlocation!=""&oldlocation!=newlocation){
          unlink(oldlocation)
          unlink(sub(".grd",".gri",oldlocation))
          print(paste(newname,i,"deleted old temp file"))
        }
        print(paste(newname,i,"merged"))
        if(i==length(rs)){
          # unlink(paste(dir,"/",paste(rep("_",r),collapse=""),newname,sep=""))
          # writeRaster(merged, paste(dir,"/",paste(rep("_",r),collapse=""),newname,sep=""),overwrite=T)
          # print(paste(newname,"saved")) 
          # unlink(newlocation)
          # unlink(sub(".grd",".gri",newlocation))
          lapply(fs,function(f){
            unlink(paste(dir,"/",f,sep=""))
          })
        }
      }
    }else if(length(rs)==1){
      unlink(rs[[1]], paste(dir,"/","_",fs,sep=""))
      writeRaster(rs[[1]], paste(dir,"/","_",fs,sep=""),overwrite=T)
      unlink(paste(dir,"/",fs,sep=""))
    }
  }
}


r <- 1
repeat{
  print(paste("r",r,sep=""))
  files_all <- list.files(dir,pattern="\\.tif$")
  # files_all <- files_all[!files_all%in%c("43.tif","44.tif")]
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
    files_split <- split(files,ceiling(seq_along(files)/2)) 
    rm(files)
  }else if(r==2){
    files <- files_all[startsWith(names(files_all),"_")&!startsWith(names(files_all),"__")]
    print(paste(length(files),"files to merge"))
    files_split <- split(files,ceiling(seq_along(files)/2))
    rm(files)
  }else if(r==3){
    files <- files_all[startsWith(names(files_all),"__")&!startsWith(names(files_all),"___")]
    print(paste(length(files),"files to merge"))
    files_split <- split(files,ceiling(seq_along(files)/2))
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
    # rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024*1.5
    # merge_rasters_(files_split,dir,r,rasterinmemory)
    # }else{
    # if(length(files_split)>1){
    n.cores <- detectCores()
    free.mem <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024
    # if(ceiling(sqrt(free.mem)/(252/9))<=n.cores){
    # n.cores<-ceiling(sqrt(free.mem)/(252/9))
    # }
    # if(r>1){
    # n.cores <- ceiling(n.cores/(r))
    # }
    if(r==1){
      n.cores <- 3
    }else if(r==2){
      n.cores <- 2
    }else{
      n.cores <- 1
    }
    if(n.cores>detectCores()){
      n.cores <- detectCores()
    }
    
    n.cores <- 1
    unlink("./logs/merge_supercells_rasters.txt")
    if(n.cores>1){
      files_split <- files_split <- split(files_split, cut(seq_along(files_split),n.cores,labels=F))
      rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024/(n.cores*1.5)
      source("./scripts/clump_vector.R")
      cl <- create_cluster_clump_(n.cores,"merge_supercells_rasters")
      tt <- parLapplyLB(cl,files_split,function(fs,dir,r,rasterinmemory){
        merge_rasters_rescale_(fs,dir,r,rasterinmemory)
      },dir,r,rasterinmemory)
      stopCluster(cl)
      rm(cl)
      unlink("./logs/merge_supercells_rasters.txt")
      rm(tt) 
    }else{
      rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024/(n.cores*1.5)
      merge_rasters_rescale_(files_split,dir,r,rasterinmemory)
    }
    
    # }else{
    # rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024*1.5
    # merge_rasters_(files_split[[1]],dir,r,rasterinmemory)
    # }
    # }
    library(rstudioapi)
    restartSession(command=source("./scripts/merge_rasters_rescale_any.R"))
  }
  rm(files_split)
  r <- r+1
}
rm(r)

files_all <- list.files(dir,pattern="\\.tif$")
if(length(files_all)>1){
  restartSession(command=source("./scripts/merge_rasters_mosaic.R"))
}else{
  writeRaster(raster(paste(dir,files_all[1],sep="")),
              "./rasters/smoothen/merged.tif")
  unlink("./rasters/smoothen/split",recursive=T)
  rm(list=ls())
  gc()
  smoothen_raster_current <- "merged"
  save(smoothen_raster_current,file="./temp/smoothen_raster_current.RData")
  source("./scripts/smoothen_raster.R")
  smoothen_raster_ini()
}


test <- function(){
  rast <- raster("./GIR/____1_24.tif")
  limit <- st_read("./GIR/Girona_buf10000.gpkg")
  rast <- crop(rast,limit)
  rast <- mask(rast,limit)
  writeRaster(rast,"./GIR/original_GIR.tif")
}

rescale_raster_any <- function(){
  max_val <- 255
  rast <- raster("./TAR/9.tif")
  values(rast) <- 10*values(rast)/max_val
}

# #do this only after filtering ids_done
# rasters <- sapply(files,function(f){
#  return(raster(paste(dir,f,sep=""))) 
# })
# names(rasters) <- sub(" ","",sub(" ","",sub(".tif","",names(rasters))))
# rasters <- rasters[order(as.numeric(names(rasters)))]

# if(!dir.exists(paste(dir, "temp/",sep=""))){
#  dir.create(paste(dir, "temp/",sep=""))
# }

#TODO: Fer de la manera seg??ent: 
#. guardar els ids fets en un arxiu ids_merged_raster_mosaic_x.RData
#. llegir els ids fets i restar-los
#. els arxius .tif resultants de la primera passada, donar-los noms consecutius (1,2,3,etc.)
#. els arxius de cada passada es guardaran a la carpeta merged/stepx
#. guardar la passada que s'hagi completat com a merged_supercells_rasters_round_done.RData
#. en comen??ar, mirar la passada que s'hagi completat i fer la seg??ent, restant els ids fets per a aquella passada
#. cada passada, es poden agafar de cinc en cinc. A la primera, es poden fer en paral??lel potser
#. anar fent passades fins que nom??s en quedi un
#. en acabar, esborrar tots els arxius ids_merged_raster_mosaic_x.RData i el merged_raster_mosaic_round_done.RData

# files_done <- list.files(paste(dir, "temp/",sep=""), pattern = "\\.tif$")
# ids_done <- sapply(files_done,function(x){
#  sub(".tif","",str_split(x,"_")[[1]])
# })
# if(length(ids_done)>0){
#  ids_todo <- names(rasters)
#  ids_done <- max(as.numeric(unlist(ids_done)))
#  # ids_done <- ids_done[length(ids_done)]
#  ids_done <- ids_todo[1:which(ids_todo==ids_done)]
#  ids_todo <- ids_todo[!ids_todo%in%ids_done]
#  rasters <- rasters[as.numeric(names(rasters))%in%ids_todo]
# }
# rm(ids_todo)
# rm(ids_done)
# rm(files_done)
# rm(files)
# print(length(rasters))

#la primera passada potser es pot fer en paral??lel?
rasters_split <- split(rasters,ceiling(seq_along(rasters)/15))
for(rs in rasters_split){
  if(length(rs)>1){
    ids <- as.numeric(names(rs[1]))
    merged <- rs[[1]]
    for(i in 2:length(rs)){
      print(i)
      r2 <- rs[[i]]
      ids <- append(ids, as.numeric(names(rs[i])))
      # newname <- paste(paste(ids, collapse="_"),".tif",sep="")
      if(length(ids)==1){
        newname <- paste(ids[1],".tif",sep="")
      }else{
        newname <- paste(ids[1],"_",tail(ids,1),".tif",sep="")
      }
      print(newname)
      if(i==2){
        oldname <- newname
      }
      # template <- projectRaster(from = r2, to= merged, alignOnly=TRUE)
      # print("template")
      # aligned <- projectRaster(from=r2, to=template)
      # alignedlocation <- aligned@file@name
      # print("aligned")
      # rm(template)
      # gc()
      
      print(oldname)
      oldlocation <- merged@file@name
      # merged <- mosaic(merged, aligned,fun=mean)
      merged <- mosaic(merged, r2,fun=mean)
      newlocation <- merged@file@name
      if(i!=2&oldlocation!=""&oldlocation!=newlocation){
        unlink(oldlocation)
        unlink(sub(".grd",".gri",oldlocation))
        print("deleted old temp file")
      }
      print("merged")
      # if(alignedlocation!=""){
      #  unlink(alignedlocation)
      #  unlink(sub(".grd",".gri",alignedlocation))
      # }
      # rm(aligned)
      gc()
      if(i%%15==0|i==length(rs)){
        # writeRaster(merged, paste("./rasters/pnoa/split/supercells/temp/",newname,sep=""),overwrite=T)
        writeRaster(merged, paste("./rasters/smooothen/split/temp/",newname,sep=""),overwrite=T)
        print("saved") 
        unlink(newlocation)
        unlink(sub(".grd",".gri",newlocation))
        # rm(merged)
        # unlink(paste("./rasters/pnoa/split/supercells/temp/",oldname,sep=""))
        # print("deleted")
        # oldname <- newname
        # gc()
      }
    }
  }else if(length(rs)==1){
    # writeRaster(rasters[[1]], paste("./rasters/pnoa/split/supercells/temp/",names(rasters[1],".tif"),sep=""),overwrite=T)
    writeRaster(rs[[1]], paste("./rasters/smooothen/split/temp/",names(rs[1]),".tif",sep=""),overwrite=T)
    restartSession(command=source("./scripts/merge_rasters_mosaic.R"))
  }
  rm(list=ls())
  gc()
  restartSession(command=source("./scripts/merge_rasters_mosaic.R"))
}


if(length(list.files(paste(dir, "temp/",sep="")))>1){
  files <- list.files(paste(dir, "temp/",sep=""), pattern = "\\.tif$")
  rasters <- sapply(files,function(f){
    return(raster(paste(dir,"temp/",f,sep=""))) 
  })
  names(rasters) <- sub(" ","",sub(" ","",sub(".tif","",names(rasters))))
  
  ids_done <- sapply(names(rasters),function(x){
    max(as.numeric(str_split(x,"_")[[1]]))
  })
  rasters <- rasters[order(ids_done)]
  ids_done <- ids_done[order(ids_done)]
  
  ids <- min(as.numeric(str_split(names(rasters)[1],"_")[[1]]))
  merged <- rasters[[1]]
  for(i in 2:length(rasters)){
    print(i)
    print(Sys.time())
    r2 <- rasters[[i]]
    ids <- append(ids, max(as.numeric(str_split(ids_done[i],"_")[[1]])))
    
    newname <- paste(ids[1],"_",tail(ids,1),".tif",sep="")
    print(newname)
    if(i==2){
      oldname <- paste(names(rasters[1]),".tif",sep="")
    }
    
    # template <- projectRaster(from = r2, to= merged, alignOnly=TRUE)
    # print("template")
    # aligned <- projectRaster(from=r2, to=template)
    # alignedlocation <- aligned@file@name
    # print("aligned")
    # rm(template)
    # gc()
    
    print(oldname)
    oldlocation <- merged@file@name
    # merged <- mosaic(merged, aligned,fun=mean)
    merged <- mosaic(merged, r2,fun=mean)
    newlocation <- merged@file@name
    if(i!=2&oldlocation!=""&oldlocation!=newlocation){
      unlink(oldlocation)
      unlink(sub(".grd",".gri",oldlocation))
      print("deleted old temp file")
    }
    print("merged")
    # if(alignedlocation!=""){
    #   unlink(alignedlocation)
    #   unlink(sub(".grd",".gri",alignedlocation))
    # }
    # rm(aligned)
    gc()
    
    # writeRaster(merged, paste("./rasters/pnoa/split/supercells/temp/",newname,sep=""),overwrite=T)
    writeRaster(merged, paste("./rasters/smooothen/split/temp/",newname,sep=""),overwrite=T)
    print("saved") 
    # # rm(merged)
    # unlink(paste("./rasters/pnoa/split/supercells/temp/",oldname,sep=""))
    unlink(paste("./rasters/smooothen/split/temp/",oldname,sep=""))
    unlink(r2@file@name)
    print("deleted")
    oldname <- newname
    # plot(merged)
    # # gc()
  }
  rm(r2)
  rm(rs)
  rm(rasters)
  
  file.copy(from=paste("./rasters/smooothen/split/temp/",newname,sep=""),
            to=paste("./rasters/smooothen/","merged.tif",sep=""))
  unlink(newlocation)
  unlink(sub(".grd",".gri",newlocation))
  rm(newname)
  rm(merged)
  rm(newlocation)
  rm(oldlocation)
  rm(oldname)
  unlink("./rasters/smooothen/split/",recursive=T)
}
if(dir.exists("./rasters/smooothen/split/temp")){
  if(length(list.files(paste(dir, "temp/",sep="")))==1){
    if(file.exists(paste("./rasters/smooothen/","merged.tif",sep=""))){
      unlink(paste("./rasters/smooothen/","merged.tif",sep=""))
    }
    file <- list.files(paste(dir, "temp/",sep=""))[1]
    file.copy(from=paste(dir, "temp/",file,sep=""),
              to=paste("./rasters/smooothen/","merged.tif",sep=""))
    
    unlink("./rasters/smooothen/split/",recursive=T)
  }else{
    stop("merge_rasters_mosaic - more than one resulting merged file")
  }
}

# unlink("./rasters/pnoa/split/",recursive=T)

# rm(list=ls())
# gc()
# smoothen_raster_current <- "merged"
# save(smoothen_raster_current,file="./temp/smoothen_raster_current.RData")
# 
# smoothen_raster_ini()
