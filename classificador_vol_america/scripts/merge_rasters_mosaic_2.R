source("./classificador_vol_america/scripts/smoothen_raster_2.R")
# print("merge_supercells_rasters_2")
print(paste(Sys.time()," merge_supercells_rasters_2"))
#TODO: change name to "merge_supercells_rasters_2"
dir <- "./classificador_vol_america/rasters/smoothen2/split/"

merge_rasters_ <- function(files_split,dir,r,rasterinmemory=NULL){
  if(!is.null(rasterinmemory)){
    # print("rrrr")
    rasterOptions(maxmemory=rasterinmemory)
  }
  for(fs in files_split){
    rs <- lapply(fs,function(f){
      if(!is.null(rasterinmemory)){
        rasterOptions(maxmemory=rasterinmemory)
      }
      raster(paste(dir,f,sep=""))
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
        # ids <- append(ids, as.numeric(str_split(names(rs[i]),"_")[[1]][length(str_split(names(rs[i]),"_")[[1]])]))
        # newname <- paste(paste(ids, collapse="_"),".tif",sep="")
        if(length(ids)==1){
          newname <- paste(ids[1],".tif",sep="")
        }else{
          newname <- paste(ids[1],"_",tail(ids,1),".tif",sep="")
        }
        # print(newname)
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
        
        # print(oldname)
        oldlocation <- merged@file@name
        # merged <- mosaic(merged, aligned,fun=mean)
        if(!is.null(rasterinmemory)){
          rasterOptions(maxmemory=rasterinmemory)
        }
        merged <- mosaic(merged, r2,fun=mean)
        newlocation <- merged@file@name
        if(i!=2&oldlocation!=""&oldlocation!=newlocation){
          unlink(oldlocation)
          unlink(sub(".grd",".gri",oldlocation))
          print(paste(newname,i,"deleted old temp file"))
        }
        print(paste(newname,i,"merged"))
        # if(alignedlocation!=""){
        #   unlink(alignedlocation)
        #   unlink(sub(".grd",".gri",alignedlocation))
        # }
        # rm(aligned)
        # gc()
        if(i==length(rs)){
          # writeRaster(merged, paste("./classificador_vol_america/rasters/pnoa/split/supercells/temp/",newname,sep=""),overwrite=T)
          unlink(paste("./classificador_vol_america/rasters/smoothen2/split/",paste(rep("_",r),collapse=""),newname,sep=""))
          writeRaster(merged, paste("./classificador_vol_america/rasters/smoothen2/split/",paste(rep("_",r),collapse=""),newname,sep=""),overwrite=T)
          print(paste(newname,"saved")) 
          unlink(newlocation)
          unlink(sub(".grd",".gri",newlocation))
          lapply(fs,function(f){
            unlink(paste(dir,f,sep=""))
          })
          # # rm(merged)
          # # unlink(paste("./classificador_vol_america/rasters/pnoa/split/supercells/temp/",oldname,sep=""))
          # # print("deleted")
          # # oldname <- newname
          # # gc()
        }
      }
    }else if(length(rs)==1){
      # print(names(fs[1]))
      # writeRaster(rasters[[1]], paste("./classificador_vol_america/rasters/pnoa/split/supercells/temp/",names(rasters[1],".tif"),sep=""),overwrite=T)
      unlink(paste("./classificador_vol_america/rasters/smoothen2/split/","_",fs,sep=""))
      writeRaster(rs[[1]], paste("./classificador_vol_america/rasters/smoothen2/split/","_",fs,sep=""),overwrite=T)
      unlink(paste("./classificador_vol_america/rasters/smoothen2/split/",fs,sep=""))
      # restartSession(command=source("./classificador_vol_america/scripts/merge_rasters_mosaic.R"))
    }
  }
}


r <- 1
repeat{
  print(paste("r",r,sep=""))
  files_all <- list.files(dir,pattern="\\.tif$")
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
    files_split <- split(files,ceiling(seq_along(files)/6)) 
    rm(files)
  }else if(r==2){
    files <- files_all[startsWith(names(files_all),"_")&!startsWith(names(files_all),"__")]
    print(paste(length(files),"files to merge"))
    files_split <- split(files,ceiling(seq_along(files)/3))
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
    # if(length(files_split)>1&length(files_split[[length(files_split)]])==1){
    #   files_split[[length(files_split)-1]] <- append(files_split[[length(files_split)-1]],
    #                                                  files_split[[length(files_split)]]) 
    #   files_split <- files_split[-length(files_split)]
    # }
    
    
    # if(r>1){
    #   rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024*1.5
    #   merge_rasters_(files_split,dir,r,rasterinmemory)
    # }else{
    # if(length(files_split)>1){
    n.cores <- detectCores()
    free.mem <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024
    if(ceiling(sqrt(free.mem)/(252/16))<=n.cores){
      n.cores<-ceiling(sqrt(free.mem)/(252/16))
    }
    if(r>1){
      n.cores <- ceiling(n.cores/(r))
    }
    # if(n.cores>6){
    #   n.cores <- 6
    # }
    n.cores <- ceiling(n.cores/2)
    
    n.cores <- 1
    unlink("./classificador_vol_america/logs/merge_supercells_rasters_2.txt")
    if(n.cores>1){
      files_split <- files_split <- split(files_split, cut(seq_along(files_split),n.cores,labels=F))
      rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024/(n.cores*1.5)
      source("./classificador_vol_america/scripts/clump_vector.R")
      cl <- create_cluster_clump_(n.cores,"merge_supercells_rasters_2")
      tt <- parLapplyLB(cl,files_split,function(fs,dir,r,rasterinmemory){
        merge_rasters_(fs,dir,r,rasterinmemory)
      },dir,r,rasterinmemory)
      stopCluster(cl)
      rm(cl)
      unlink("./classificador_vol_america/logs/merge_supercells_rasters_2.txt")
      rm(tt)  
    }else{
      rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024/(n.cores*1.5)
      merge_rasters_(files_split,dir,r,rasterinmemory)
    }
    
    # }else{
    #   rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024*1.5
    #   merge_rasters_(files_split[[1]],dir,r,rasterinmemory)
    # }
    # }
    library(rstudioapi)
    restartSession(command=source("./classificador_vol_america/scripts/merge_rasters_mosaic_2.R"))
  }
  rm(files_split)
  r <- r+1
}
rm(r)

files_all <- list.files(dir,pattern="\\.tif$")
if(length(files_all)>1){
  restartSession(command=source("./classificador_vol_america/scripts/merge_rasters_mosaic_2.R"))
}else{
  writeRaster(raster(paste(dir,files_all[1],sep="")),
              "./classificador_vol_america/rasters/smoothen2/merged.tif")
  unlink("./classificador_vol_america/rasters/smoothen2/split",recursive=T)
  rm(list=ls())
  gc()
  smoothen_raster_2_current <- "merged"
  save(smoothen_raster_2_current,file="./classificador_vol_america/temp/smoothen_raster_2_current.RData")
  source("./classificador_vol_america/scripts/smoothen_raster_2.R")
  smoothen_raster_2_ini()
}

