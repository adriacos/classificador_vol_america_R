source("./scripts/smoothen_raster.R")
# print("merge_supercells_rasters")
print(paste(Sys.time()," merge_supercells_rasters"))
#TODO: change name to "merge_supercells_rasters"
dir <- "./rasters/smoothen/split/"
unlink("./temp/split",recursive=T)
dir.create("./temp/split")

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
        # if(i==length(rs)){
        #   stop()
        # }
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
        
        
        if(i==length(rs)){
          unlink(paste(dir,"/",paste(rep("_",r),collapse=""),newname,sep=""))
          merged <- mosaic(merged,r2,fun="max",na.rm=T,
                           filename=paste(dir,"/",paste(rep("_",r),collapse=""),newname,sep=""))
          print(paste(newname,"saved")) 
        }else{
          merged <- mosaic(merged,r2,fun="max",na.rm=T,filename=paste("./temp/split/",paste(rep("_",r),collapse=""),newname,sep=""))
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
        gc()
        
        
        
        # merged <- mosaic(merged, r2,fun=mean)
        # newlocation <- merged@file@name
        # if(i!=2&oldlocation!=""&oldlocation!=newlocation){
        #   unlink(oldlocation)
        #   unlink(sub(".grd",".gri",oldlocation))
        #   print(paste(newname,i,"deleted old temp file"))
        # }
        # print(paste(newname,i,"merged"))
        # # if(alignedlocation!=""){
        # #   unlink(alignedlocation)
        # #   unlink(sub(".grd",".gri",alignedlocation))
        # # }
        # # rm(aligned)
        # # gc()
        # if(i==length(rs)){
        #   # writeRaster(merged, paste("./rasters/pnoa/split/supercells/temp/",newname,sep=""),overwrite=T)
        #   writeRaster(merged, paste("./rasters/smoothen/split/",paste(rep("_",r),collapse=""),newname,sep=""),overwrite=T)
        #   print(paste(newname,"saved")) 
        #   unlink(newlocation)
        #   unlink(sub(".grd",".gri",newlocation))
        #   lapply(fs,function(f){
        #     unlink(paste(dir,f,sep=""))
        #   })
        #   # # rm(merged)
        #   # # unlink(paste("./rasters/pnoa/split/supercells/temp/",oldname,sep=""))
        #   # # print("deleted")
        #   # # oldname <- newname
        #   # # gc()
        # }
      }
      rm(merged)
      rm(r2)
      gc()
    }else if(length(rs)==1){
      # print(names(fs[1]))
      # writeRaster(rasters[[1]], paste("./rasters/pnoa/split/supercells/temp/",names(rasters[1],".tif"),sep=""),overwrite=T)
      writeRaster(rs[[1]], paste("./rasters/smoothen/split/","_",fs,sep=""),overwrite=T)
      unlink(paste("./rasters/smoothen/split/",fs,sep=""))
      gc()
      # restartSession(command=source("./scripts/merge_rasters_mosaic.R"))
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
  # if(r==1){
  #   files <- files_all[!startsWith(names(files_all),"_")]
  #   print(paste(length(files),"files to merge"))
  #   files_split <- split(files,ceiling(seq_along(files)/12)) 
  #   rm(files)
  # # }else if(r==2){
  #   # files <- files_all[startsWith(names(files_all),"_")&!startsWith(names(files_all),"__")]
  #   # print(paste(length(files),"files to merge"))
  #   # files_split <- split(files,ceiling(seq_along(files)/3))
  #   # rm(files)
  # }else{
    files <- files_all[startsWith(names(files_all),paste(rep("_",r-1),collapse=""))&!startsWith(names(files_all),paste(rep("_",r),collapse=""))]
    print(paste(length(files),"files to merge"))
    files_split <- split(files,ceiling(seq_along(files)/2))
    rm(files)
  # }
  rm(files_all)
  if(length(files_split)>0){
    if(length(files_split)>1&length(files_split[[length(files_split)]])==1){
      files_split[[length(files_split)-1]] <- append(files_split[[length(files_split)-1]],
                                                     files_split[[length(files_split)]]) 
      files_split <- files_split[-length(files_split)]
    }
    # if(r>1){
    #   rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024*1.5
    #   merge_rasters_(files_split,dir,r,rasterinmemory)
    # }else{
      if(length(files_split)>1){
        n.cores <- detectCores()
        if(r==1){
          n.cores <- 13
        }else if(r==2){
          n.cores <- 7
        }else if(r==3){
          n.cores <- 3
        }else if(r==4){
          n.cores <- 3
        }else{
          n.cores <- 1
        }
        
        
        
        
        if(n.cores==1){
          rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024*1.5
          merge_rasters_(files_split,dir,r,rasterinmemory)
          gc()
        }else{
          files_split <- files_split <- split(files_split, cut(seq_along(files_split),n.cores,labels=F))
          rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024*1.5/n.cores
          source("./scripts/clump_vector.R")
          unlink("./logs/merge_supercells_rasters.txt")
          cl <- create_cluster_clump_(n.cores,"merge_supercells_rasters")
          tt <- parLapplyLB(cl,files_split,function(fs,dir,r,rasterinmemory){
            merge_rasters_(fs,dir,r,rasterinmemory)
          },dir,r,rasterinmemory)
          stopCluster(cl)
          rm(cl)
          unlink("./logs/merge_supercells_rasters.txt")
          rm(tt)
          gc()
        }
      }else{
        rasterinmemory <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))*1024*1.5
        merge_rasters_(files_split,dir,r,rasterinmemory)
        gc()
      }
    # }
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


# #do this only after filtering ids_done
# rasters <- sapply(files,function(f){
#   return(raster(paste(dir,f,sep="")))  
# })
# names(rasters) <- sub(" ","",sub(" ","",sub(".tif","",names(rasters))))
# rasters <- rasters[order(as.numeric(names(rasters)))]

# if(!dir.exists(paste(dir, "temp/",sep=""))){
#   dir.create(paste(dir, "temp/",sep=""))
# }

#TODO: Fer de la manera següent: 
#. guardar els ids fets en un arxiu ids_merged_raster_mosaic_x.RData
#. llegir els ids fets i restar-los
#. els arxius .tif resultants de la primera passada, donar-los noms consecutius (1,2,3,etc.)
#. els arxius de cada passada es guardaran a la carpeta merged/stepx
#. guardar la passada que s'hagi completat com a merged_supercells_rasters_round_done.RData
#. en començar, mirar la passada que s'hagi completat i fer la següent, restant els ids fets per a aquella passada
#. cada passada, es poden agafar de cinc en cinc. A la primera, es poden fer en paral·lel potser
#. anar fent passades fins que només en quedi un
#. en acabar, esborrar tots els arxius ids_merged_raster_mosaic_x.RData i el merged_raster_mosaic_round_done.RData

# files_done <- list.files(paste(dir, "temp/",sep=""), pattern = "\\.tif$")
# ids_done <- sapply(files_done,function(x){
#   sub(".tif","",str_split(x,"_")[[1]])
# })
# if(length(ids_done)>0){
#   ids_todo <- names(rasters)
#   ids_done <- max(as.numeric(unlist(ids_done)))
#   # ids_done <- ids_done[length(ids_done)]
#   ids_done <- ids_todo[1:which(ids_todo==ids_done)]
#   ids_todo <- ids_todo[!ids_todo%in%ids_done]
#   rasters <- rasters[as.numeric(names(rasters))%in%ids_todo]
# }
# rm(ids_todo)
# rm(ids_done)
# rm(files_done)
# rm(files)
# print(length(rasters))

#la primera passada potser es pot fer en paral·lel?
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
      #   unlink(alignedlocation)
      #   unlink(sub(".grd",".gri",alignedlocation))
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
    #   unlink(alignedlocation)
    #   unlink(sub(".grd",".gri",alignedlocation))
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
