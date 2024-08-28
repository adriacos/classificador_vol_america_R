library(stringr)
library(exactextractr)
library(raster)
library(smoothr)
library(pryr)
library(data.table)
library(sf)


clump_vector_crop_raster <- function(vect,rast,quad_id,file_id,log=F){
  if(log==T){
    print(paste("crop raster",file_id,"_",quad_id,sep=""))    
  }
  dir.create(paste("./classificador_vol_america/rasters/temp/",file_id,"/",sep=""),showWarnings=F)
  unlink(paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",
               file_id,"_",quad_id,".gri",sep=""))
  # unlink(paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",
  #              file_id,"_",quad_id,".tif",sep=""))
  # if(file.exists(paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",
  #                      file_id,"_",quad_id,".gri",sep=""))){
  #   rast <- raster(paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",
  #                        file_id,"_",quad_id,".gri",sep=""))
  # }else{
  rast <- crop(rast, st_transform(vect,st_crs(rast)), filename=paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",
                                                                     file_id,"_",quad_id,sep=""),overwrite=T)
  # writeRaster(rast,paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",
  #                        file_id,"_",quad_id,".tif",sep=""))
  # }
  rast
}
clump_vector_extract_values <- function(vect,rast,quad_id,file_id,var,operations,log=F){
  
  # vect$sd <- 0
  # values(rast) <- 10*values(rast)/255
  
  for(operation in operations){
    name <- paste(var,operation,sep="")
    if(var=="value"){
      if(operation=="mean"){
        name <- "DN"
      }else if(operation=="stdev"){
        name <- "sd"
      }else if(operation=="stdev"){
        name <- "median"
      }
    }
    vect[,name] <- exact_extract(rast, vect, operation, progress=F)
    if(nrow(vect[is.na(vect[,name]),]>0)){
      vect[is.na(vect[,name]),name:=0]
    }
    if(log==T){
      print(paste("quad:",quad_id," file:",file_id," ",name," extracted",sep=""))
    }  
    rm(name)
  }
  
  # vect$sd <- exact_extract(rast, vect, "stdev", progress=F)
  # if(nrow(vect[is.na(vect$sd),]>0)){
  #   vect[is.na(vect$sd),"sd":=0]
  # }
  # if(log==T){
  #   print(paste("quad:",quad_id," file:",file_id," sd extracted",sep=""))
  # }
  # 
  # vect$DN  <- exact_extract(rast, vect, "mean", progress=F)
  # if(nrow(vect[is.na(vect$DN),]>0)){
  #   vect[is.na(vect$DN),"DN":=5]
  # }
  # vect$DN <- as.numeric(vect$DN)
  # if(log==T){
  #   print(paste("quad:",quad_id," file:",file_id," DN extracted",sep=""))
  # }
  rm(rast)
  vect
}



clump_vector_remove_repited <- function(vect_dt,quad_id,file_id){
  # vect_dt <- split(vect_dt,by=colnames(vect_dt)[-which(colnames(vect_dt)%in%c("id","geometry"))])
  # print(vect_dt[1])
  vect_dt <- split(vect_dt,by=c("sd","DN","area"))
  if(any(sapply(vect_dt,nrow)>1)){
    # print(paste("quad:",quad_id," file:",file_id," ",length(vect_dt[sapply(vect_dt,nrow)>1])," repeated!",sep=""))
    # 
    # for(i in 1:length(vect_dt)){
    #   if(vect_dt[[i]]$id[[1]]==3006){
    #     rep <- vect_dt[[i]]
    #     break()
    #   }
    # }
    vect_dupl <- vect_dt[sapply(vect_dt,nrow)>1]
    vect_nodupl <- rbindlist(vect_dt[sapply(vect_dt,nrow)==1],use.names=T)
    vect_dupl <-rbindlist(lapply(vect_dupl,function(rep,quad_id,file_id){
      # print(rep$id[[1]])
      # n.row <- nrow(rep)
      if(nrow(rep)==1){
        return(rep)
      }
      rep <- st_as_sf(rep)
      x <- 1
      tt <- NULL
      repeat{
        # print(paste("x:",x,sep=""))
        if(x>nrow(rep)){
          break()
        }
        rep.x <- rep[x,]
        y <- 1
        repeat{
          # print(paste("x:",x,"y:",y,"nrow:",nrow(rep),sep=""))
          if(y>nrow(rep)){
            break()
          }
          if(x==y){
            y <- y+1
            next()
          }
          rep.y <- rep[y,]
          if(length(st_equals(rep.x,rep.y)[[1]])>0){
            # print(rep.x)
            # print(rep.y)
            # print(file_id)
            # print(quad_id)
            if(is.null(tt)){
              tt <- rbind(rep.x,rep.y)
            }else{
              tt <- rbind(tt,rep.y)
            }
            rep <- rep[-y,]
            next()
          }
          y <- y+1
        }
        x <- x+1
      }
      if(!file.exists(paste("./test/","file_",file_id,"quad_",quad_id,as.data.frame(tt)[1,"id"],".gpkg",sep=""))){
        st_write(tt,paste("./test/","file_",file_id,"quad_",quad_id,as.data.frame(tt)[1,"id"],".gpkg",sep=""))
      }
      data.table(rep)
    },quad_id,file_id),use.names=T)
    vect_dt <- rbind(vect_dupl,vect_nodupl)
    rm(vect_dupl)
    rm(vect_nodupl)
  }else{
    vect_dt <- rbindlist(vect_dt,use.names=T)
  }
  vect_dt
}

extract_metrics_parallel <- function(vect,rast,quad_id,file_id,var,operations=c("mean"),log=F){
  n.cores <- ceiling(detectCores()/2)
  vect_dt <- as.data.table(vect)
  bbox <- st_bbox(vect)
  bbox["xmin"] <-min(sapply(vect_dt$geometry,function(v){st_bbox(v)$xmin}))
  bbox["ymin"] <- min(sapply(vect_dt$geometry,function(v){st_bbox(v)$ymin}))
  bbox["xmax"] <- max(sapply(vect_dt$geometry,function(v){st_bbox(v)$xmax}))
  bbox["ymax"] <- max(sapply(vect_dt$geometry,function(v){st_bbox(v)$ymax}))
  limit <- st_as_sf(st_sf(st_as_sfc(bbox)))
  rm(bbox)
  width <- as.numeric(sqrt(st_area(limit)))/1000
  
  free.mem <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024
  if(width/(sqrt(n.cores))<=free.mem/(2100*n.cores)){
    km_core <- width/(sqrt(n.cores))
  }else{
    km_core <- free.mem/(2100*n.cores)
  }
  rm(free.mem)
  vect_dt <- set_quad_ori_km_first(vect_dt,km_core,limit,log=F)
  rm(limit)
  vect_dt[,paste("quad_id_",km_core,"km",sep=""):=sapply(vect_dt[[which(colnames(vect_dt)==paste("quad_id_",km_core,"km",sep=""))]],function(id){
    str_split(id,",")[[1]][1]
  })]
  # setindex(vect_dt,paste("quad_id_",(width/(sqrt(n.cores))),"km",sep=""))
  vect_dt <- split(vect_dt,by=c(paste("quad_id_",km_core,"km",sep="")))
  unlink("./classificador_vol_america/logs/clump_vector_parallelextract.txt")
  rm(vect)
  gc()
  cl <- create_cluster_clump_(n.cores,outfile="clump_vector_parallelextract")
  vect <- do.call(rbind,parLapplyLB(cl,vect_dt,function(vv,quad_id,file_id,rast,log,km_core,var,operations){
    id <- vv[[1,paste("quad_id_",km_core,"km",sep="")]]
    # print(id)
    vv <- st_as_sf(vv)
    rast <- clump_vector_crop_raster(vv,rast,paste(quad_id,"-",id,sep=""),file_id,log)
    gc()
    print(paste("quad:",quad_id," file:",file_id," id:",id," raster croped",sep=""))
    vv <- clump_vector_extract_values(vv,rast,paste(quad_id,"-",id,sep=""),file_id,var,operations,log)
    rm(rast)
    unlink(paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",file_id,"_",paste(quad_id,"-",id,sep=""),".gri",sep=""))
    unlink(paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",file_id,"_",paste(quad_id,"-",id,sep=""),".grd",sep=""))
    gc()
    vv
  },quad_id,file_id,rast,log,km_core,var,operations))
  stopCluster(cl)
  rm(cl)
  unlink("./classificador_vol_america/logs/clump_vector_parallelextract.txt")
  rm(vect_dt)
  gc()
  vect[,paste("quad_ori_",km_core,"km",sep="")] <- NULL
  vect[,paste("quad_id_",km_core,"km",sep="")] <- NULL
  vect[,paste("quad_row_",km_core,"km",sep="")] <- NULL
  vect[,paste("quad_col_",km_core,"km",sep="")] <- NULL
  rm(width)
  vect
}

extract_neighbors_parallel <- function(vect,quad_id,file_id,log=F){
  n.cores <- ceiling(detectCores()-(detectCores()/4))
  vect_dt <- as.data.table(vect)
  bbox <- st_bbox(vect)
  bbox["xmin"] <-min(sapply(vect_dt$geometry,function(v){st_bbox(v)$xmin}))
  bbox["ymin"] <- min(sapply(vect_dt$geometry,function(v){st_bbox(v)$ymin}))
  bbox["xmax"] <- max(sapply(vect_dt$geometry,function(v){st_bbox(v)$xmax}))
  bbox["ymax"] <- max(sapply(vect_dt$geometry,function(v){st_bbox(v)$ymax}))
  limit <- st_as_sf(st_sf(st_as_sfc(bbox)))
  rm(bbox)
  gc()
  width <- as.numeric(sqrt(st_area(limit)))/1000
  
  free.mem <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024
  if(width/(sqrt(n.cores))<=free.mem/(2100*n.cores)){
    km_core <- width/(sqrt(n.cores))
  }else{
    km_core <- free.mem/(2100*n.cores)
  }
  
  vect_dt <- set_quad_ori_km_first(vect_dt,km_core,limit,log=F)
  rm(limit)
  unique_ids <- unique(vect_dt[[paste("quad_id_",km_core,"km",sep="")]])
  vect_dt <- split(vect_dt,by=c(paste("quad_id_",km_core,"km",sep="")))
  for(id in unique_ids){
    if(grepl(",",id,fixed=TRUE)){
      ids_ <- str_split(id,",")[[1]]
      for(id_ in ids_){
        vect_dt[[id_]] <- rbind(vect_dt[[id_]],vect_dt[[id]])
      }
    }
  }
  rm(id)
  rm(id_)
  rm(ids_)
  rm(width)
  unique_ids <- unique_ids[!(grepl(",",unique_ids,fixed=TRUE))]
  vect_dt <- vect_dt[unique_ids]
  rm(unique_ids)
  gc()
  unlink("./classificador_vol_america/logs/clump_vector_neighbours_get.txt")
  cl <- create_cluster_clump_(n.cores,outfile="clump_vector_neighbours_get")
  vect_dt <- rbindlist(parLapplyLB(cl,vect_dt,function(vv){
    vv <- vv[,c("id","geometry")]
    id <- vv[[1,"id"]]
    print(nrow(vv))
    vv <- st_as_sf(vv)
    neighbours <- st_overlaps(st_cast(vv, "MULTILINESTRING"))
    vv$neighbors <- sapply(neighbours,function(nn,ids){
      paste(sapply(nn,function(n,ids){
        ids[n]
      },ids),collapse=",")
    },as.data.frame(vv)$id)
    rm(neighbours)
    gc()
    library(nngeo)
    tt <- st_contains(st_remove_holes(vv),remove_self=T)
    vv$tt <- sapply(tt,function(t,vv){
      paste(vv[t,]$id,collapse=",")
    },vv)
    rm(tt)
    gc()
    if(nrow(as.data.frame(vv)[vv$tt!="",])>0){
      contained <- as.data.frame(do.call(rbind,lapply(as.data.frame(vv)[vv$tt!="","id"],function(id,vv){
        # print(id)
        v <- vv[vv$id==id,]
        v <- st_cast(st_difference(st_remove_holes(v),v)[c("id","tt")],"POLYGON")
        if(nrow(v)==0){
          return(NULL)
        }
        v <- rbind(vv[vv$id%in%str_split(v$tt,",")[[1]],c("id","tt")],st_cast(v,"POLYGON"))
        rownames(v) <- 1:nrow(v)
        v$tt <- st_equals(v,remove_self=T)
        # v$tt <- st_overlaps(st_cast(v, "MULTILINESTRING"),remove_self=T)
        v$tt <- sapply(v$tt,function(nn,ids){
          paste(sapply(nn,function(n,ids){
            ids[n]
          },ids),collapse=",")
        },as.data.frame(v)$id)
        colnames(v)[colnames(v)=="tt"] <- "contained"
        v <- data.table(v)[,c("id","contained")]
        v <- v[,.(contained=paste(contained,collapse=",")),by=id]
        v
      },vv)))[,c("id","contained")]
      vv <- merge(vv,contained,all.x=T)
      vv$neighbors <- paste(vv$neighbors,vv$contained,sep=",")
      vv$neighbors <- sapply(vv$neighbors,function(n){
        n <- unique(str_split(n,",")[[1]])
        n <- n[n!="NA"]
        n <- n[n!=""]
        paste(n,collapse=",")
      })
      rm(neighbours)
      rm(contains)
      vv$contained <- NULL
      gc()
    }  
    vv$tt <- NULL
    vv <- as.data.table(vv)
    # print(id)
    gc()
    vv
  }),use.names=T)
  stopCluster(cl)
  rm(cl)
  rm(n.cores)
  gc()
  unlink("./classificador_vol_america/logs/clump_vector_neighbours_get.txt")
  vect_dt <- vect_dt[,.(neighbors=paste(neighbors,collapse=",")),by=id]
  vect_dt[,neighbors:=sapply(neighbors,function(n){
    n <- unique(str_split(n,",")[[1]])
    n <- n[n!="NA"]
    n <- n[n!=""]
    paste(n,collapse=",")
  })]
  
  vect$neighbors <- NULL
  vect <- merge(vect,vect_dt,by="id")
  rm(vect_dt)
  vect
}

clump_vector_prepare <- function(vect_dt,rast,quad_id,file_id,extract=T,parallelextract=F,forceddnextract=F,log=F){
  # print(paste(file_id,quad_id,"prepare",Sys.time()))
  time <- Sys.time()
  
  vect <- st_as_sf(vect_dt)
  rm(vect_dt)
  gc()
  vect$area <- as.numeric(st_area(vect))
  vect$area <- abs(vect$area)
  vect$id <- 1:nrow(vect)
  # print("area extracted")
  
  load("./classificador_vol_america/temp/resolution.RData")
  
  if(log==T){
    print(paste(file_id,quad_id,"extract",Sys.time()))
  }
  if(extract==T){
    if(nrow(vect[is.na(vect$DN),])>0|nrow(vect[is.na(vect$sd),])>0|forceddnextract==T){
      # print(paste("quad:",quad_id," file:",file_id," Extract-",Sys.time()))
      if(parallelextract==T){
        if(log==T){
          print(paste("quad:",quad_id," file:",file_id," Parallel extract-",Sys.time()))
        }
        # n.cores <- detectCores()-(detectCores()/4)
        # n.cores <- detectCores()
        
        vect <- st_as_sf(extract_metrics_parallel(vect,rast,quad_id=quad_id,file_id=file_id,var="value",operations=c("mean","sd")))
        
        # 
        # n.cores <- ceiling(detectCores()/2)
        # vect_dt <- as.data.table(vect)
        # bbox <- st_bbox(vect)
        # bbox["xmin"] <-min(sapply(vect_dt$geometry,function(v){st_bbox(v)$xmin}))
        # bbox["ymin"] <- min(sapply(vect_dt$geometry,function(v){st_bbox(v)$ymin}))
        # bbox["xmax"] <- max(sapply(vect_dt$geometry,function(v){st_bbox(v)$xmax}))
        # bbox["ymax"] <- max(sapply(vect_dt$geometry,function(v){st_bbox(v)$ymax}))
        # limit <- st_as_sf(st_sf(st_as_sfc(bbox)))
        # rm(bbox)
        # width <- as.numeric(sqrt(st_area(limit)))/1000
        # 
        # free.mem <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024
        # if(width/(sqrt(n.cores))<=free.mem/(2100*n.cores)){
        #   km_core <- width/(sqrt(n.cores))
        # }else{
        #   km_core <- free.mem/(2100*n.cores)
        # }
        # rm(free.mem)
        # vect_dt <- set_quad_ori_km_first(vect_dt,km_core,limit,log=F)
        # rm(limit)
        # vect_dt[,paste("quad_id_",km_core,"km",sep=""):=sapply(vect_dt[[which(colnames(vect_dt)==paste("quad_id_",km_core,"km",sep=""))]],function(id){
        #   str_split(id,",")[[1]][1]
        # })]
        # # setindex(vect_dt,paste("quad_id_",(width/(sqrt(n.cores))),"km",sep=""))
        # vect_dt <- split(vect_dt,by=c(paste("quad_id_",km_core,"km",sep="")))
        # unlink("./classificador_vol_america/logs/clump_vector_parallelextract.txt")
        # rm(vect)
        # gc()
        # cl <- create_cluster_clump_(n.cores,outfile="clump_vector_parallelextract")
        # vect <- do.call(rbind,parLapplyLB(cl,
        #                                   vect_dt
        #                                   # unique(vect_dt[[which(colnames(vect_dt)==paste("quad_id_",(width/(sqrt(n.cores))),"km",sep=""))]])
        #                                   ,function(vv,quad_id,file_id,rast,log,km_core){
        #                                     id <- vv[[1,paste("quad_id_",km_core,"km",sep="")]]
        #                                     # print(id)
        #                                     vv <- st_as_sf(vv)
        #                                     rast <- clump_vector_crop_raster(vv,rast,paste(quad_id,"-",id,sep=""),file_id,log)
        #                                     gc()
        #                                     print(paste("quad:",quad_id," file:",file_id," id:",id," raster croped",sep=""))
        #                                     vv <- clump_vector_extract_values(vv,rast,paste(quad_id,"-",id,sep=""),file_id,log)
        #                                     rm(rast)
        #                                     unlink(paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",file_id,"_",paste(quad_id,"-",id,sep=""),".gri",sep=""))
        #                                     unlink(paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",file_id,"_",paste(quad_id,"-",id,sep=""),".grd",sep=""))
        #                                     gc()
        #                                     vv
        #                                   },quad_id,file_id,rast,log,km_core))
        # # },vect_dt,paste("quad_id_",(width/(sqrt(n.cores))),"km",sep="")))
        # stopCluster(cl)
        # rm(cl)
        # unlink("./classificador_vol_america/logs/clump_vector_parallelextract.txt")
        # rm(vect_dt)
        # gc()
        # vect[,paste("quad_ori_",km_core,"km",sep="")] <- NULL
        # vect[,paste("quad_id_",km_core,"km",sep="")] <- NULL
        # vect[,paste("quad_row_",km_core,"km",sep="")] <- NULL
        # vect[,paste("quad_col_",km_core,"km",sep="")] <- NULL
        # rm(width)
        # print(paste("values extracted-",Sys.time()))
      }else{
        # rast <- clump_vector_crop_raster(vect,rast,quad_id,file_id,log)
        # # print("raster croped")
        vect <- clump_vector_extract_values(vect,rast,quad_id,file_id,log)
        # unlink(paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",file_id,"_",quad_id,".gri",sep=""))
        # unlink(paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",file_id,"_",quad_id,".grd",sep=""))
      }
      rm(rast)
      gc()
    }
    # rast <- clump_vector_crop_raster(vect,rast,quad_id,file_id)
    # values(rast) <- 10*values(rast)/max(values(rast),na.rm=T)
    # 
    # vect$sd <- exact_extract(rast, vect, "stdev", progress=F)
    # if(nrow(vect[is.na(vect$sd),]>0)){
    #   vect[is.na(vect$sd),"sd":=0]
    # }
    # # print("sd extracted")
    # 
    # vect$DN  <- exact_extract(rast, vect, "mean", progress=F)
    # if(nrow(vect[is.na(vect$DN),]>0)){
    #   vect[is.na(vect$DN),"DN":=5]
    # }
    # vect$DN <- as.numeric(vect$DN)
    # # print("DN extracted")
    # rm(rast)
    # unlink(paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",file_id,"_",quad_id,".gri",sep=""))
    # unlink(paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",file_id,"_",quad_id,".grd",sep=""))
    # # else if((length(unique(as.data.frame(vect)[,"plare"]))==1&unique(is.na(unique(as.data.frame(vect)[,"plare"]))))){
    # #   vect$plare <- as.character(vect$area)
    # # }
  }
  
  n.row <- nrow(vect)
  vect <- st_as_sf(clump_vector_remove_repited(data.table(vect),quad_id,file_id))
  if(n.row-nrow(vect)!=0){
    print(paste("START quad:",quad_id," file:",file_id," ",n.row-nrow(vect)," repeated rows removed",sep=""))
  }
  
  vect$id <- 1:nrow(vect)
  
  if(log==T){
    print(paste(file_id,quad_id,"neighbours",Sys.time()))
  }
  library(nngeo)
  if(parallelextract==T){
    
    
    vect <- extract_neighbors_parallel(vect,quad_id,file_id,log)
    # 
    # n.cores <- ceiling(detectCores()-(detectCores()/4))
    # # n.cores <- 16
    # vect_dt <- as.data.table(vect)
    # bbox <- st_bbox(vect)
    # bbox["xmin"] <-min(sapply(vect_dt$geometry,function(v){st_bbox(v)$xmin}))
    # bbox["ymin"] <- min(sapply(vect_dt$geometry,function(v){st_bbox(v)$ymin}))
    # bbox["xmax"] <- max(sapply(vect_dt$geometry,function(v){st_bbox(v)$xmax}))
    # bbox["ymax"] <- max(sapply(vect_dt$geometry,function(v){st_bbox(v)$ymax}))
    # limit <- st_as_sf(st_sf(st_as_sfc(bbox)))
    # rm(bbox)
    # width <- as.numeric(sqrt(st_area(limit)))/1000
    # 
    # free.mem <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024
    # if(width/(sqrt(n.cores))<=free.mem/(2800*n.cores)){
    #   km_core <- width/(sqrt(n.cores))
    # }else{
    #   km_core <- free.mem/(2800*n.cores)
    # }
    # 
    # vect_dt <- set_quad_ori_km_first(vect_dt,km_core,limit,log=F)
    # rm(limit)
    # # vect_dt[,paste("quad_id_",(width/(sqrt(n.cores))),"km",sep=""):=sapply(vect_dt[[which(colnames(vect_dt)==paste("quad_id_",(width/(sqrt(n.cores))),"km",sep=""))]],function(id){
    # #   str_split(id,",")[[1]][1]
    # # })]
    # # setindex(vect_dt,paste("quad_id_",(width/(sqrt(n.cores))),"km",sep=""))
    # unique_ids <- unique(vect_dt[[paste("quad_id_",km_core,"km",sep="")]])
    # vect_dt <- split(vect_dt,by=c(paste("quad_id_",km_core,"km",sep="")))
    # for(id in unique_ids){
    #   if(grepl(",",id,fixed=TRUE)){
    #     ids_ <- str_split(id,",")[[1]]
    #     for(id_ in ids_){
    #       vect_dt[[id_]] <- rbind(vect_dt[[id_]],vect_dt[[id]])
    #     }
    #   }
    # }
    # rm(id)
    # rm(id_)
    # rm(ids_)
    # rm(width)
    # unique_ids <- unique_ids[!(grepl(",",unique_ids,fixed=TRUE))]
    # vect_dt <- vect_dt[unique_ids]
    # rm(unique_ids)
    # gc()
    # unlink("./classificador_vol_america/logs/clump_vector_neighbours_get.txt")
    # cl <- create_cluster_clump_(n.cores,outfile="clump_vector_neighbours_get")
    # vect_dt <- rbindlist(parLapplyLB(cl,vect_dt,function(vv){
    #   vv <- vv[,c("id","geometry")]
    #   id <- vv[[1,"id"]]
    #   # print(vv[[1,"id"]])
    #   print(nrow(vv))
    #   vv <- st_as_sf(vv)
    #   # vv <- vv[vv$id==8632|vv$id==8589|vv$id==8557|vv$id==8628|vv$id==8590|vv$id==8652,]
    #   neighbours <- st_overlaps(st_cast(vv, "MULTILINESTRING"))
    #   # neighbours_i <- st_intersects(st_buffer(vv,0))
    #   # neighbours_o <- st_overlaps(vv)
    #   # neighbours_t <- st_touches(vv)
    #   vv$neighbors <- sapply(neighbours,function(nn,ids){
    #     paste(sapply(nn,function(n,ids){
    #       ids[n]
    #     },ids),collapse=",")
    #   },as.data.frame(vv)$id)
    #   rm(neighbours)
    #   gc()
    #   library(nngeo)
    #   tt <- st_contains(st_remove_holes(vv),remove_self=T)
    #   vv$tt <- sapply(tt,function(t,vv){
    #     paste(vv[t,]$id,collapse=",")
    #   },vv)
    #   rm(tt)
    #   gc()
    #   if(nrow(as.data.frame(vv)[vv$tt!="",])>0){
    #     contained <- as.data.frame(do.call(rbind,lapply(as.data.frame(vv)[vv$tt!="","id"],function(id,vv){
    #       # print(id)
    #       v <- vv[vv$id==id,]
    #       v <- st_cast(st_difference(st_remove_holes(v),v)[c("id","tt")],"POLYGON")
    #       if(nrow(v)==0){
    #         return(NULL)
    #       }
    #       v <- rbind(vv[vv$id%in%str_split(v$tt,",")[[1]],c("id","tt")],st_cast(v,"POLYGON"))
    #       rownames(v) <- 1:nrow(v)
    #       v$tt <- st_equals(v,remove_self=T)
    #       # v$tt <- st_overlaps(st_cast(v, "MULTILINESTRING"),remove_self=T)
    #       v$tt <- sapply(v$tt,function(nn,ids){
    #         paste(sapply(nn,function(n,ids){
    #           ids[n]
    #         },ids),collapse=",")
    #       },as.data.frame(v)$id)
    #       colnames(v)[colnames(v)=="tt"] <- "contained"
    #       v <- data.table(v)[,c("id","contained")]
    #       v <- v[,.(contained=paste(contained,collapse=",")),by=id]
    #       v
    #     },vv)))[,c("id","contained")]
    #     vv <- merge(vv,contained,all.x=T)
    #     vv$neighbors <- paste(vv$neighbors,vv$contained,sep=",")
    #     vv$neighbors <- sapply(vv$neighbors,function(n){
    #       n <- unique(str_split(n,",")[[1]])
    #       n <- n[n!="NA"]
    #       n <- n[n!=""]
    #       paste(n,collapse=",")
    #     })
    #     rm(neighbours)
    #     rm(contains)
    #     vv$contained <- NULL
    #     gc()
    #   }  
    #   vv$tt <- NULL
    #   vv <- as.data.table(vv)
    #   print(id)
    #   gc()
    #   vv
    # }),use.names=T)
    # stopCluster(cl)
    # rm(cl)
    # rm(n.cores)
    # gc()
    # unlink("./classificador_vol_america/logs/clump_vector_neighbours_get.txt")
    # # rm(vect_dt)
    # vect_dt <- vect_dt[,.(neighbors=paste(neighbors,collapse=",")),by=id]
    # vect_dt[,neighbors:=sapply(neighbors,function(n){
    #   # paste(unique(str_split(n,",")[[1]]),collapse=",")
    #   n <- unique(str_split(n,",")[[1]])
    #   n <- n[n!="NA"]
    #   n <- n[n!=""]
    #   paste(n,collapse=",")
    #   })]
    # 
    # # vect[,paste("quad_ori_",(width/(sqrt(n.cores))),"km",sep="")] <- NULL
    # # vect[,paste("quad_id_",(width/(sqrt(n.cores))),"km",sep="")] <- NULL
    # # vect[,paste("quad_row_",(width/(sqrt(n.cores))),"km",sep="")] <- NULL
    # # vect[,paste("quad_col_",(width/(sqrt(n.cores))),"km",sep="")] <- NULL
    # vect$neighbors <- NULL
    # vect <- merge(vect,vect_dt,by="id")
    # rm(vect_dt)
    # # print(paste("values extracted-",Sys.time()))
  }else{
    # neighbours <- st_overlaps(st_cast(vect, "MULTILINESTRING"))
    # st_overlaps(vect)
    neighbours <- st_overlaps(st_cast(vect, "MULTILINESTRING"))
    vect$neighbors <- sapply(neighbours,paste,collapse=",")
    
    # vect$neighbors <- sapply(as.data.frame(vect)$id,function(id,vect){
    #   vv <- vect[vect$id==id,]
    #   if(vv$neighbors==""){
    #     return("")
    #   }
    #   nghbs <- vect[vect$id%in%str_split(vv$neighbors,",")[[1]],]
    #   if(nrow(nghbs)==1){
    #     return(nghbs$id)
    #   }
    #   nghbs <- nghbs[sapply(nghbs$id,function(id,nghbs,vv){
    #     nghb <- nghbs[nghbs$id==id,]
    #     vv <- st_cast(vv,"MULTILINESTRING")
    #     if(st_length(st_intersection(st_cast(nghb,"MULTILINESTRING"),vv))>=0.01*st_length(vv)){
    #       return(T)
    #     }else{
    #       return(F)
    #     }
    #   },nghbs,vv)==T,]
    #   if(nrow(nghbs)==0){
    #     return("")
    #   }
    #   return(paste(order(nghbs$id),collapse=","))
    # },vect)
    
    tt <- st_contains(st_remove_holes(vect),remove_self=T)
    vect$tt <- sapply(tt,paste,collapse=",")
    rm(tt)
    if(nrow(as.data.frame(vect)[vect$tt!="",])>0){
      contained <- rbindlist(lapply(as.data.frame(vect)[vect$tt!="","id"],function(id,vect){
        # print(id)
        vv <- vect[vect$id==id,]
        vv <- st_cast(st_difference(st_remove_holes(vv),vv)[c("id","tt")],"POLYGON")
        if(nrow(vv)==0){
          return(NULL)
        }
        vv <- rbind(vect[vect$id%in%str_split(vv$tt,",")[[1]],c("id","tt")],st_cast(vv,"POLYGON"))
        rownames(vv) <- 1:nrow(vv)
        vv$tt <- st_equals(vv,remove_self=T)
        # vv$tt <- st_overlaps(st_cast(vv, "MULTILINESTRING"),remove_self=T)
        vv$tt <- sapply(vv$tt,function(nn,ids){
          paste(sapply(nn,function(n,ids){
            ids[n]
          },ids),collapse=",")
        },as.data.frame(vv)$id)
        colnames(vv)[colnames(vv)=="tt"] <- "contained"
        vv <- data.table(vv)[,c("id","contained")]
        vv <- vv[,.(contained=paste(contained,collapse=",")),by=id]
        vv
      },vect))
      vect <- merge(vect,contained,all.x=T)
      vect$neighbors <- paste(vect$neighbors,vect$contained,sep=",")
      vect$contained <- NULL
      vect$neighbors <- sapply(vect$neighbors,function(n){
        n <- unique(str_split(n,",")[[1]])
        n <- n[n!=""]
        n <- n[n!="NA"]
        paste(n,collapse=",")
      })
      rm(neighbours)
      rm(contained)  
    }
    vect$tt <- NULL
  }
  if(log==T){
    print(paste("quad:",quad_id," file:",file_id," Neighbours extracted",sep=""))
  }
  
  vect$toignore <- FALSE
  # st_area(vect)
  
  # print(paste(file_id,quad_id,"prepared",Sys.time()))
  vect_dt <- as.data.table(vect)
  # st_area(st_as_sf(vect_dt))
  rm(vect)
  gc()
  if(extract==T){
    vect_dt[npl==0,"npl":=1]
    if(!("pldn" %in% colnames(vect_dt))){
      vect_dt$pldn <- as.character(vect_dt$DN)
    }else{
      vect_dt[pldn=="","pldn":=as.character(round(vect_dt[pldn==""]$DN,2))] 
    }
    # else if((length(unique(as.data.frame(vect)[,"pldn"]))==1&unique(is.na(unique(as.data.frame(vect)[,"pldn"]))))){
    #   vect$pldn <- as.character(vect$DN)
    # }
    if(!("plare" %in% colnames(vect_dt))){
      vect_dt$plare <- as.character(vect_dt$area)
    }else{
      vect_dt[plare=="","plare":=as.character(vect_dt[plare==""]$area)]
      # vect_dt[is.na(vect_dt$plare),"plare"] <- as.character(vect_dt$area) 
    }
  }
  # st_area(st_as_sf(vect_dt))
  
  setkey(vect_dt,id)
  setindex(vect_dt,toignore)
  setindex(vect_dt,area)
  # st_area(st_as_sf(vect_dt))
  
  if(log==T){
    print(paste("quad:",quad_id," file:",file_id," PREPARED",sep=""))
  }
  
  # if(save_to_disk==T){
  #   saveRDS(vect_dt,paste("./classificador_vol_america/vect/temp/",file_id,"/",quad_id,sep=""))
  #   return(T)
  # }else{
    return(vect_dt) 
  # }
}

clump_vector_simplify <- function(vect_dt,rast,quad_id,file_id,km,
                                  log=T,prepare=T){
  if(nrow(vect_dt)==0){
    return(T)
  }
  arealimit <- 38*resolution
  
  load("./classificador_vol_america/temp/minres.RData")
  if(log==T){
    print(paste("CLUMP VECTOR",file_id,quad_id,nrow(vect_dt),Sys.time(),sep="-"))
  }
  dir.create("./classificador_vol_america/vect/temp/clumped/",showWarnings=F)
  dir.create(paste("./classificador_vol_america/vect/temp/clumped/",file_id,"/",sep=""),showWarnings=F)
  load("./classificador_vol_america/temp/resolution.RData")
  
  rast <- clump_vector_crop_raster(st_as_sf(vect_dt),rast,quad_id,file_id,log)
  # values(rast) <- 10*values(rast)/255
  vect_dt <- clump_vector_prepare(vect_dt,rast,quad_id,file_id,extract=F,log=log)
  
  time <- Sys.time()
  
  if((max(vect_dt$area)<arealimit)||nrow(vect_dt)==1){
    vect_dt[,neighbors:=NULL]
    vect_dt[,toignore:=NULL]
    saveRDS(vect_dt,paste("./classificador_vol_america/vect/temp/clumped/",file_id,"/",quad_id,".rds",sep=""))
    if(log==T){
      print(paste(file_id,quad_id,
                  "FINISH", Sys.time(),
                  sep=" - "))
    }
    return(T)
  }
  
  repeat{
    
    vect.min <- subset(vect_dt,area<arealimit)[.(FALSE),.SD[which.max(area)],on="toignore"]      
    
    if(nrow(vect.min)==0){
      # stop("nrow")
      rm(vect.min)
      break()
    }else if(vect.min$area>=arealimit){
      # print("nrow")
      rm(vect.min)
      break()
    }
    
    vect.min.neighbors <- vect_dt[.(as.numeric(str_split(vect.min$neighbors, ",")[[1]]))]
    
    if(nrow(vect.min.neighbors)==0){
      vect_dt[J(vect.min$id),toignore:=T]
      rm(vect.min.neighbors)
      next()
    }
    
   if(vect.min$area>22*resolution){
    if(nrow(vect.min.neighbors[area<(38*resolution)])>0){
      vect.min.neighbors.min <- vect.min.neighbors[area<(38*resolution),.SD[which.max(area)]]
      }else{
        vect.min.neighbors.min <- vect.min.neighbors[,.SD[which.min((abs(layer-vect.min$layer)))]]
      }  
    }else{
      vect.min.neighbors.min <- vect.min.neighbors[,.SD[which.min((abs(layer-vect.min$layer)))]]
    }
    rm(vect.min.neighbors)
    
    if(nrow(vect.min.neighbors.min)==0){
      vect_dt[J(vect.min$id),toignore:=T]
      next()
    }
 
    ids <- c(vect.min$id, vect.min.neighbors.min$id)
    
    vect.df <- vect_dt[J(ids)]
    
    vneighbors <- unique(str_split(paste(vect.df$neighbors,collapse=","), ",")[[1]])
    vneighbors <- vneighbors[!(vneighbors %in% ids)]
    vneighbors <- paste(vneighbors, collapse = ",")
    varea <- sum(vect.df$area)

    vlayer <- sum(vect.df$layer*(vect.df$area/varea))
    # if(vect.df[1,paste("quad_ori_",km,"km",sep=""),with=F][[1]]!=vect.df[2,paste("quad_ori_",km,"km",sep=""),with=F][[1]]){
      vquadori <- c(vect.df[1,paste("quad_ori_",km,"km",sep=""),with=F][[1]],
                    vect.df[2,paste("quad_ori_",km,"km",sep=""),with=F][[1]])
      vquadori <- unlist(str_split(vquadori,","))
      vquadori <- unique(vquadori)[order(as.numeric(unique(vquadori)))]
      vquadori <- paste(vquadori,collapse=",")
    # }else{
      # vquadori <- vect.df[1,paste("quad_ori_",km,"km",sep=""),with=F][[1]]
    # }
    # if(vect.df[1,paste("quad_id_",km,"km",sep=""),with=F][[1]]!=vect.df[2,paste("quad_id_",km,"km",sep=""),with=F][[1]]){
      vquadid <- c(vect.df[1,paste("quad_id_",km,"km",sep=""),with=F][[1]],
                   vect.df[2,paste("quad_id_",km,"km",sep=""),with=F][[1]])
      vquadid <- unlist(str_split(vquadid,","))
      vquadid <- unique(vquadid)[order(as.numeric(unique(vquadid)))]
      vquadid <- paste(vquadid,collapse=",")
    # }else{
      # vquadid <- vect.df[1,paste("quad_id_",km,"km",sep=""),with=F][[1]]
    # }
    
    vect.df <- vect.df[1,]
    vect.df[,neighbors:=vneighbors]
    vect.df[,area:=varea]
    vect.df[,layer:=vlayer]
    vect.df[,id:=ids[1]]
    vect.df[,paste("quad_id_",km,"km",sep=""):=vquadid]
    vect.df[,paste("quad_ori_",km,"km",sep=""):=vquadori]

    vneighbors <- str_split(vect_dt$neighbors, ",")
    vneighbors <- lapply(vneighbors, function(nb){nb[nb==ids[2]]<-ids[1]; nb})
    vect_dt[,neighbors:=sapply(vneighbors, paste, collapse=",")]
  
    vect.union <- st_as_sf(st_union(st_combine(st_as_sf(vect_dt[J(ids),])),by_feature=T))
    vect.df[,geometry:=NULL]
    vect.shp.agg <- cbind(vect.union,vect.df)
    colnames(vect.shp.agg)[colnames(vect.shp.agg)=="x"] <- "geometry"
    st_geometry(vect.shp.agg) <- "geometry"
    st_crs(vect.shp.agg) <- st_crs(st_crs(st_as_sf(vect_dt)))
    vect_dt[J(ids[1]),names(vect_dt)[-which(names(vect_dt)=="id")]:=vect.shp.agg[,names(vect_dt)[-which(names(vect_dt)=="id")]]]
    vect_dt <- vect_dt[id!=ids[2]]
  
    # c <- c+1
    # if(c%%2000==0){
    #   # if(log==T){
    #     print(paste(file_id,quad_id,c,vect.min$area,nrow(vect_dt),sep="-"))
    #   # }
    #   # time <- Sys.time()
    # }
    vect.min <- NULL
  }
  
  rm(vect.df)
  rm(vect.min)
  rm(vect.min.neighbors)
  rm(vect.min.neighbors.min)
  
  vect_dt[,neighbors:=NULL]
  vect_dt[,toignore:=NULL]
  
  saveRDS(vect_dt,paste("./classificador_vol_america/vect/temp/clumped/",file_id,"/",quad_id,".rds",sep=""))
  
  if(log==T){
  print(paste("FINISH",file_id,quad_id,
            # as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024,
            nrow(vect_dt),
            Sys.time(),
            sep=" - "))
  }
  rm(vect_dt)
  gc()
  return(T)
  
}


#methods: "closest"(closer value),"smallest","biggest"
clump_vector <- function(vect_dt,rast,quad_id,file_id,
                         arealimit=NULL,method="closest",
                         log=T,prepare=T,km,
                         parallelextract=F,forceddnextract=F,
                         lastkm=NULL,lastarealimit=NULL){
  
  if(nrow(vect_dt)==0){
    return(T)
  }
  # print(arealimit)
  load("./classificador_vol_america/temp/minres.RData")
  if(log==T){
    print(paste("CLUMP VECTOR",file_id,quad_id,nrow(vect_dt),Sys.time(),sep="-"))
  }
  dir.create("./classificador_vol_america/vect/temp/clumped/",showWarnings=F)
  dir.create(paste("./classificador_vol_america/vect/temp/clumped/",file_id,"/",sep=""),showWarnings=F)
  load("./classificador_vol_america/temp/resolution.RData")
  
  
  if(as.numeric(sum(st_area(st_as_sf(vect_dt))))<=(ncell(rast)*resolution)/4){
    rast <- clump_vector_crop_raster(st_as_sf(vect_dt),rast,quad_id,file_id,log)    
  }
  # values(rast) <- 10*values(rast)/255
  if(prepare==T){
    vect_dt <- clump_vector_prepare(vect_dt,rast,quad_id,file_id,extract=T,parallelextract=parallelextract,forceddnextract=forceddnextract,log=log)
    # print(paste("prepared",file_id,quad_id,nrow(vect_dt),Sys.time(),sep="-"))
  }
 
  # rast <- raster(paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",file_id,"_",quad_id,".tif",sep=""))
  time <- Sys.time()
  
  if((!is.null(arealimit)&&max(vect_dt$area)<arealimit)||nrow(vect_dt)==1){
    vect_dt[,neighbors:=NULL]
    vect_dt[,toignore:=NULL]
    saveRDS(vect_dt,paste("./classificador_vol_america/vect/temp/clumped/",file_id,"/",quad_id,".rds",sep=""))
    if(log==T){
      print(paste(file_id,quad_id,
                "FINISH", Sys.time(),
                sep=" - "))
    }
    return(T)
  }
  
  
  if(!is.null(lastkm)&!is.null(lastarealimit)){
    vect_dt[vect_dt$area<lastarealimit,"toignore":=T]
    vect_dt[as.data.frame(vect_dt)[,paste("quad_id_",lastkm,"km",sep="")]%like%",","toignore":=F]
  }
  vect_dt[vect_dt$neighbors=="","toignore":=T]
 
  c <- 1
  # cc <- 0
  first100 <- FALSE
  first300 <- FALSE
  first500 <- FALSE
  first700 <- FALSE
  first1100 <- FALSE
  firstminres <- FALSE

  # if(is.null(arealimit)){
  #   vect_dt <- vect_dt[vect_dt$neighbors!=""]
  # }
  # 
  
  if(!is.null(lastarealimit)){
    if(lastarealimit>=4400*resolution){
      first1100=T
      first700=T
      first500=T
      first300=T
      first100=T
    }else if(lastarealimit>=2800*resolution){
        first700=T
        first500=T
        first300=T
        first100=T
     }else if(lastarealimit>=2000*resolution){
        first500=T
        first300=T
        first100=T
      }else if(lastarealimit>=1200*resolution){
        first300=T
        first100=T
    }else if(lastarealimit>=400*resolution){
        first100=T
    }
    if(lastarealimit>=minres){
      firstminres=T
    }
  }
  
  repeat{
      vect.min <- vect_dt[.(FALSE),.SD[which.min(area)],on="toignore"]
      # cc <- cc+1
      # if(cc%%2000==0){
      #   # if(log==T){
      #   print(paste(cc,vect.min$area,sep="-"))
      #   # gc()
      #   # }
      #   # time <- Sys.time()
      # }
      
    if(nrow(vect.min)==0){
      print("no rows")
      # stop("nrow")
      if(!is.null(arealimit)){
        if(arealimit>=4400*resolution){
          if(first1100==F){
            # print("first1100")
            vect_dt[,toignore:=F]
            first1100=T
            arealimit <- 500
            next()
          }
        }else if(arealimit>=2800*resolution){
          if(first700==F){
            # print("first700")
            vect_dt[,toignore:=F]
            first700=T
            arealimit <- 1200*resolution
            next()
          }
        }else if(arealimit>=2000*resolution){
          if(first500==F){
            # print("first500")
            vect_dt[,toignore:=F]
            first500=T
            arealimit <- 800*resolution
            next()
          }
        }else if(arealimit>=1200*resolution){
          if(first300==F){
            # print("first300")
            vect_dt[,toignore:=F]
            # vect_dt$sd <- exact_extract(rast, st_as_sf(vect_dt), "stdev", progress=F)
            first300=T
            arealimit <- 400*resolution
            next()
          }
        }else if(arealimit>=400*resolution){
          if(first100==F){
            # print("first100")
            vect_dt[,toignore:=F]
            first100=T
            arealimit <- 38*resolution
            next()
          }
        }  
      }
      rm(vect.min)
      break()
    }else if(!is.null(arealimit)&&vect.min$area>=arealimit){
      print("arealimit reached")
      # print("nrow")
      if(arealimit>=4400*resolution){
        if(first1100==F){
          # print("first1100")
          vect_dt[,toignore:=F]
          first1100=T
          arealimit <- 500
          next()
        }
      }else if(arealimit>=2800*resolution){
        if(first700==F){
          # print("first700")
          vect_dt[,toignore:=F]
          first700=T
          arealimit <- 1200*resolution
          next()
        }
      }else if(arealimit>=2000*resolution){
        if(first500==F){
          # print("first500")
          vect_dt[,toignore:=F]
          first500=T
          arealimit <- 800*resolution
          next()
        }
      }else if(arealimit>=1200*resolution){
        if(first300==F){
          # print("first300")
          vect_dt[,toignore:=F]
          # vect_dt$sd <- exact_extract(rast, st_as_sf(vect_dt), "stdev", progress=F)
          first300=T
          arealimit <- 400*resolution
          next()
        }
      }else if(arealimit>=400*resolution){
        if(first100==F){
          # print("first100")
          vect_dt[,toignore:=F]
          first100=T
          arealimit <- 38*resolution
          next()
        }
      }
      rm(vect.min)
      break()
    }else{
      if(first100==F){
        if(vect.min$area>=400*resolution){
          print("first100")
          vect_dt[vect_dt$area<38*resolution,toignore:=F]
          first100=T
          next()
        }
      }else if(first300==F){
        if(vect.min$area>=1200*resolution){
          print("first300")
          vect_dt[vect_dt$area<400*resolution,toignore:=F]
          first300=T
          next()
        }
      }else if(first500==F){
        if(vect.min$area>=2000*resolution){
          print("first500")
          vect_dt[vect_dt$area<200,toignore:=F]
          first500=T
          next()
        }
      }else if(first700==F){
        if(vect.min$area>=2800*resolution){
          print("first700")
          vect_dt[vect_dt$area<300,toignore:=F]
          first700=T
          next()
        }
      }else if(first1100==F){
        if(vect.min$area>=4400*resolution){
          print("first1100")
          vect_dt[vect_dt$area<500,toignore:=F]
          first1100=T
          next()
        }
      }  
      
      # 
      # 
      # if(vect.min$area>=400*resolution){
      #   if(first100==F){
      #     print("first100")
      #     # vect_dt[,toignore:=F]
      #     vect_dt[vect_dt$area<38*resolution,toignore:=F]
      #     # vect_dt[,toignore:=F]
      #     first100=T
      #     next()
      #   }
      #   if(vect.min$area>=1200*resolution){
      #     if(first300==F){
      #       print("first300")
      #       # vect_dt[,toignore:=F]
      #       vect_dt[vect_dt$area<400*resolution,toignore:=F]
      #       # vect_dt$sd <- exact_extract(rast, st_as_sf(vect_dt), "stdev", progress=F)
      #       # vect_dt[,toignore:=F]
      #       first300=T
      #       next()
      #     }
      #     if(vect.min$area>=2000*resolution){
      #       if(first500==F){
      #         print("first500")
      #         # vect_dt[,toignore:=F]
      #         vect_dt[vect_dt$area<200,toignore:=F]
      #         # vect_dt[,toignore:=F]
      #         first500=T
      #         next()
      #       }
      #       if(vect.min$area>=2800*resolution){
      #         if(first700==F){
      #           print("first700")
      #           # vect_dt[,toignore:=F]
      #           vect_dt[vect_dt$area<300,toignore:=F]
      #           # vect_dt[,toignore:=F]
      #           first700=T
      #           next()
      #         }
      #         if(vect.min$area>=4400*resolution){
      #           if(first1100==F){
      #             print("first1100")
      #             # vect_dt[,toignore:=F]
      #             vect_dt[vect_dt$area<500,toignore:=F]
      #             # vect_dt[,toignore:=F]
      #             first1100=T
      #             next()
      #           }
      #         }
      #       }
      #     }
        # }
      # }
    }
    if(firstminres==F){
      if(vect.min$area >= minres){
        print("firstminres")
        # stop()
        vect_dt[,toignore:=F]
        firstminres=T
      }
    }
    # if(vect.min$neighbors==""){
    #   vect_dt[J(vect.min$id),toignore:=T]
    #   rm(vect.min)
    #   rm(vect.min.neighbors.min)
    #   # print("next")
    #   next()
    # }
      
    vect.min.neighbors <- vect_dt[.(as.numeric(str_split(vect.min$neighbors, ",")[[1]]))]
    
    # if(nrow(vect.min.neighbors)==0){
    #   vect_dt[J(vect.min$id),toignore:=T]
    #   rm(vect.min.neighbors)
    #   next()
    # }
    
    if(vect.min$area>=300){
      # vect.min$sd <- exact_extract(rast, st_as_sf(vect.min), "stdev", progress=F)
      # vect.min.neighbors$sd <- exact_extract(rast, st_as_sf(vect.min.neighbors), "stdev", progress=F)
      vect.min.neighbors <- vect.min.neighbors[,.SD[order((1+abs(DN-vect.min$DN))*(1+abs(sd-vect.min$sd)))]]
    }else{
      vect.min.neighbors <- vect.min.neighbors[,.SD[order((abs(DN-vect.min$DN)))]]
    }
    
  
    for(i in 1:nrow(vect.min.neighbors)){
      vect.min.neighbors.min <- vect.min.neighbors[i,]
      nxt <- F
      if(vect.min$area>500){
        if(st_length(st_intersection(st_cast(st_as_sf(vect.min.neighbors.min),"MULTILINESTRING"),st_cast(st_as_sf(vect.min),"MULTILINESTRING")))<0.02*st_length(st_cast(st_as_sf(vect.min),"MULTILINESTRING"))){
          nxt <- T
        }
      }
      if((firstminres==F&&first100==F) && vect.min$area>22*resolution && vect.min$area < 38*resolution && (abs(vect.min.neighbors.min$DN-vect.min$DN)>2.5)
      # if((firstminres==F&&first100==F) && vect.min$area>22*resolution && vect.min$area < 38*resolution && (abs(vect.min.neighbors.min$DN-vect.min$DN)>2.45)
      ){
        # vect_dt[J(vect.min$id),toignore:=T]
        # rm(vect.min)
        # rm(vect.min.neighbors.min)
        nxt <- T
        # next()
        # }else if(firstminres==F&&first300==F && vect.min$area>=38*resolution && vect.min$area < 400*resolution && (abs(vect.min.neighbors.min$DN-vect.min$DN)>2.2||
        #                                                                                                            abs(vect.min.neighbors.min$sd-vect.min$sd)>1.110079)#0.787798)
      # }else if(firstminres==F&&first300==F && vect.min$area>=38*resolution && vect.min$area < 400*resolution && (abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(2.2)^2*(1+0.787798))#0.895225)
      }else if(firstminres==F&&first300==F && vect.min$area>=38*resolution && vect.min$area < 400*resolution && (abs(vect.min.neighbors.min$DN-vect.min$DN)>(2.2))#0.895225)
      # }else if(firstminres==F&&first300==F && vect.min$area>=38*resolution && vect.min$area < 400*resolution && (abs(vect.min.neighbors.min$DN-vect.min$DN)>(2.1))#0.895225)
      ){
        # vect_dt[J(vect.min$id),toignore:=T]
        # rm(vect.min)
        # rm(vect.min.neighbors.min)
        nxt <- T
        # next()
        # }else if(vect.min$area>=minres&&first500==F&&vect.min$area>=100&&vect.min$area<200&&(abs(vect.min.neighbors.min$DN-vect.min$DN)>2.1||
        #                                                                                      abs(vect.min.neighbors.min$sd-vect.min$sd)>1.002652)#0.751989)
      # }else if(vect.min$area>=minres&&first500==F&&vect.min$area>=100&&vect.min$area<200&&(abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(2.1)^2*(1+0.751989))#0.859416)
      }else if(vect.min$area>=minres&&first500==F&&vect.min$area>=100&&vect.min$area<200&&(abs(vect.min.neighbors.min$DN-vect.min$DN)>(2.1))#0.859416)
      # }else if(vect.min$area>=minres&&first500==F&&vect.min$area>=100&&vect.min$area<200&&(abs(vect.min.neighbors.min$DN-vect.min$DN)>(1.96))#0.859416)
      ){
        # vect_dt[J(vect.min$id),toignore:=T]
        # rm(vect.min)
        # rm(vect.min.neighbors.min)
        nxt <- T
        # next()
        # }else if(vect.min$area>=minres&&first700==F&&vect.min$area>=200&&vect.min$area<300&&(abs(vect.min.neighbors.min$DN-vect.min$DN)>2||
        #                                                                                      abs(vect.min.neighbors.min$sd-vect.min$sd)>0.895225)#0.716180)
      # }else if(vect.min$area>=minres&&first700==F&&vect.min$area>=200&&vect.min$area<300&&(abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(2)^2*(1+0.716180))#0.823607)
      }else if(vect.min$area>=minres&&first700==F&&vect.min$area>=200&&vect.min$area<300&&(abs(vect.min.neighbors.min$DN-vect.min$DN)>(2))#0.823607)
      # }else if(vect.min$area>=minres&&first700==F&&vect.min$area>=200&&vect.min$area<300&&(abs(vect.min.neighbors.min$DN-vect.min$DN)>(1.82))#0.823607)
      ){
        # vect_dt[J(vect.min$id),toignore:=T]
        # rm(vect.min)
        # rm(vect.min.neighbors.min)
        nxt <- T
        # next()
      }else if(vect.min$area>=minres&&first1100==F&&vect.min$area>=300&&vect.min$area<500&&((1+abs(vect.min.neighbors.min$DN-vect.min$DN))*(1+abs(vect.min.neighbors.min$sd-vect.min$sd))>1+log(1+(10000/vect.min$area)))){
        nxt <- T
      
      #   }else if(vect.min$area>=minres&&first1100==F&&vect.min$area>=300&&vect.min$area<500&&(abs(vect.min.neighbors.min$DN-vect.min$DN)>1.9||#1.68||
      #                                                                                         abs(vect.min.neighbors.min$sd-vect.min$sd)>0.680371)#0.680371)0,107427
      # # }else if(vect.min$area>=minres&&first1100==F&&vect.min$area>=300&&vect.min$area<500&&(abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(1.9)^2*(1+0.680371))
      # # }else if(vect.min$area>=minres&&first1100==F&&vect.min$area>=300&&vect.min$area<500&&(abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(1.68)^2*(1+0.680371))
      # ){
        # vect_dt[J(vect.min$id),toignore:=T]
        # rm(vect.min)
        # rm(vect.min.neighbors.min)
        # nxt <- T
        # next()
      }else if(vect.min$area>=minres&&vect.min$area>=500&&vect.min$area<10000&&((1+abs(vect.min.neighbors.min$DN-vect.min$DN))*(1+abs(vect.min.neighbors.min$sd-vect.min$sd))>1+log(1+(10000/vect.min$area)))){
        nxt <- T
      }else if(vect.min$area>=minres&&vect.min$area>=10000&&((1+abs(vect.min.neighbors.min$DN-vect.min$DN))*(1+abs(vect.min.neighbors.min$sd-vect.min$sd))>1+log(1+(10000/(10000+((vect.min$area-10000)/2)))))){
        nxt <- T
      }
    
      # }else if(vect.min$area >= minres && vect.min$area >= 500 && (vect.min$area < 3000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>1.6||#1.47||
      #                                                                abs(vect.min.neighbors.min$sd-vect.min$sd)> 0.572944)#0.572944)
      # # }else if(vect.min$area >= minres && vect.min$area >= 500 && vect.min$area < 3000 &&(1+abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(1.6)^2*(1+0.572944))
      # # }else if(vect.min$area >= minres && vect.min$area >= 500 && vect.min$area < 3000 &&(1+abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(1.47)^2*(1+0.572944))
      # ){
      #   # vect_dt[J(vect.min$id),toignore:=T]
      #   # rm(vect.min)
      #   # rm(vect.min.neighbors.min)
      #   nxt <- T
      #   # next()
      #   }else if(vect.min$area >= minres && vect.min$area >= 3000 && (vect.min$area < 6000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>1.3||#1.12||
      #                                                                 abs(vect.min.neighbors.min$sd-vect.min$sd)>0.465517)#0.465517)
      # # }else if(vect.min$area >= minres && vect.min$area >= 3000 && vect.min$area < 6000 &&(abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(1.3)^2*(1+0.465517))
      # # }else if(vect.min$area >= minres && vect.min$area >= 3000 && vect.min$area < 6000 &&(abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(1.12)^2*(1+0.465517))
      # ){
      #   # vect_dt[J(vect.min$id),toignore:=T]
      #   # rm(vect.min)
      #   # rm(vect.min.neighbors.min)
      #   nxt <- T
      #   # next()
      #   }else if(vect.min$area >= minres && vect.min$area >= 6000 && (vect.min$area < 9000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>1||#0.84||
      #                                                                 abs(vect.min.neighbors.min$sd-vect.min$sd)>0.35809)#0.35809)
      # # }else if(vect.min$area >= minres && vect.min$area >= 6000 && vect.min$area < 9000 &&(abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(1)^2*(1+0.35809))
      # # }else if(vect.min$area >= minres && vect.min$area >= 6000 && vect.min$area < 9000 &&(abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(0.84)^2*(1+0.35809))
      # ){
      #   # vect_dt[J(vect.min$id),toignore:=T]
      #   # rm(vect.min)
      #   # rm(vect.min.neighbors.min)
      #   nxt <- T
      #   # next()
      #   } else if(vect.min$area >= minres && vect.min$area >= 9000 && (vect.min$area < 12000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>0.7||#0.49||
      #                                                                  abs(vect.min.neighbors.min$sd-vect.min$sd)>0.250663)#0.250663)
      # # }else if(vect.min$area >= minres && vect.min$area >= 9000 && vect.min$area < 12000 &&(abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(0.7)^2*(1+0.250663))
      # # }else if(vect.min$area >= minres && vect.min$area >= 9000 && vect.min$area < 12000 &&(abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(0.49)^2*(1+0.250663))
      # ){
      #   # vect_dt[J(vect.min$id),toignore:=T]
      #   # rm(vect.min)
      #   # rm(vect.min.neighbors.min)
      #   nxt <- T
      #   # next()
      #   } else if(vect.min$area >= minres && vect.min$area >= 12000 && (vect.min$area < 15000 && abs(vect.min.neighbors.min$DN-vect.min$DN)>0.4||#0.21||
      #                                                                   abs(vect.min.neighbors.min$sd-vect.min$sd)>0.143236)#0.143236)
      # # }else if(vect.min$area >= minres && vect.min$area >= 12000 && vect.min$area < 15000 &&(abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(0.4)^2*(1+0.143236))
      # # }else if(vect.min$area >= minres && vect.min$area >= 12000 && vect.min$area < 15000 &&(abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(0.21)^2*(1+0.143236))
      # ){
      #   # vect_dt[J(vect.min$id),toignore:=T]
      #   # rm(vect.min)
      #   # rm(vect.min.neighbors.min)
      #   nxt <- T
      #   # next()
      #   } else if(vect.min$area >= minres && vect.min$area >= 15000 && (abs(vect.min.neighbors.min$DN-vect.min$DN)>0.1||#0.07||
      #   abs(vect.min.neighbors.min$sd-vect.min$sd)>0.035809)#0.035809)
      # # }else if(vect.min$area >= minres && vect.min$area >= 15000 &&(abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(0.1)^2*(1+0.035809))
      # # }else if(vect.min$area >= minres && vect.min$area >= 15000 &&(abs(vect.min.neighbors.min$DN-vect.min$DN)^2*abs(vect.min.neighbors.min$sd-vect.min$sd)>(0.07)^2*(1+0.035809))
      # ){
      #   # vect_dt[J(vect.min$id),toignore:=T]
      #   # rm(vect.min)
      #   # rm(vect.min.neighbors.min)
      #   nxt <- T
      #   # next()
      # }
      if(nxt==F){
        break()
      }
    }
    if(nxt==T){
      vect_dt[J(vect.min$id),toignore:=T]
      rm(vect.min)
      rm(vect.min.neighbors.min)
      nxt <- F
      next()
    }
    
   
    ids <- c(vect.min$id, vect.min.neighbors.min$id)
    
    vect.df <- vect_dt[J(ids)]
    
    vneighbors <- unique(str_split(paste(vect.df$neighbors,collapse=","), ",")[[1]])
    vneighbors <- vneighbors[!(vneighbors %in% ids)]
    vneighbors <- paste(vneighbors, collapse = ",")
    varea <- sum(vect.df$area)
    # if(simplify==F){
      # if(vect.df[1,area]>(38*resolution)){
        # stop("fasfasfg")
        
        vDN <- sum(vect.df$DN*(vect.df$area/varea))
        # vsd <- sum(vect.df$sd*(vect.df$area/varea))
        # vsd <- sqrt((sum((vect.df$area-1)*vect.df$sd^2))/(sum(vect.df$area)-nrow(vect.df)))
        
        vnpl <-  sum(vect.df$npl)
        vplare <- paste(vect.df[1,]$plare, vect.df[2,]$plare, sep=",")
        vpldn <- paste(vect.df[1,]$pldn, vect.df[2,]$pldn, sep=",")
        
    # vsd <- sd(as.numeric(str_split(vpldn,",")[[1]]))
        
        # for(ikm in kms_all){
          vquadori <- c(vect.df[1,paste("quad_ori_",km,"km",sep=""),with=F][[1]],
                        vect.df[2,paste("quad_ori_",km,"km",sep=""),with=F][[1]])
          vquadori <- unlist(str_split(vquadori,","))
          vquadori <- unique(vquadori)[order(as.numeric(unique(vquadori)))]
          vquadori <- paste(vquadori,collapse=",")
          
          vquadid <- c(vect.df[1,paste("quad_id_",km,"km",sep=""),with=F][[1]],
                       vect.df[2,paste("quad_id_",km,"km",sep=""),with=F][[1]])
          vquadid <- unlist(str_split(vquadid,","))
          vquadid <- unique(vquadid)[order(as.numeric(unique(vquadid)))]
          vquadid <- paste(vquadid,collapse=",") 
        # }
        vect.df <- vect.df[1,]
        vect.df[,neighbors:=vneighbors]
        vect.df[,area:=varea]
        vect.df[,id:=ids[1]]
        vect.df[,npl:=vnpl]
        vect.df[,pldn:=vpldn]
        # vect.df[,plmaxare:=vplmaxare]
        vect.df[,plare:=vplare]
  # vect.df[,sd:=vsd]
        vect.df[,DN:=vDN]
        rm(vDN)
    # rm(vsd)
        vect.df[,paste("quad_id_",km,"km",sep=""):=vquadid]
        vect.df[,paste("quad_ori_",km,"km",sep=""):=vquadori]
        # rm(vquadid)
        # rm(vnpl)
        # rm(vpldn)
        # rm(vquadori)
  #     }else{
  #       # vlayer <- sum(vect.df$DN*(vect.df$area/varea))
  #   # vsd <-  vect.df[2]$sd
  #       vDN <- vect.df[2]$DN
  #       vnpl <-  vect.df[2]$npl
  #       vpldn <- vect.df[2]$pldn
  #       vplare <- vect.df[2]$plare
  #       # vplmaxare <- vect.df[2]$area
  #       # if(vect.df[1,paste("quad_ori_",km,"km",sep=""),with=F][[1]]!=vect.df[2,paste("quad_ori_",km,"km",sep=""),with=F][[1]]){
  #         vquadori <- c(vect.df[1,paste("quad_ori_",km,"km",sep=""),with=F][[1]],
  #                       vect.df[2,paste("quad_ori_",km,"km",sep=""),with=F][[1]])
  #         vquadori <- unlist(str_split(vquadori,","))
  #         vquadori <- unique(vquadori)[order(as.numeric(unique(vquadori)))]
  #         vquadori <- paste(vquadori,collapse=",")
  #       # }else{
  #         # vquadori <- vect.df[1,paste("quad_ori_",km,"km",sep=""),with=F][[1]]
  #       # }
  #       # if(vect.df[1,paste("quad_id_",km,"km",sep=""),with=F][[1]]!=vect.df[2,paste("quad_id_",km,"km",sep=""),with=F][[1]]){
  #         vquadid <- c(vect.df[1,paste("quad_id_",km,"km",sep=""),with=F][[1]],
  #                      vect.df[2,paste("quad_id_",km,"km",sep=""),with=F][[1]])
  #         vquadid <- unlist(str_split(vquadid,","))
  #         vquadid <- unique(vquadid)[order(as.numeric(unique(vquadid)))]
  #         vquadid <- paste(vquadid,collapse=",")
  #       # }else{
  #         # vquadid <- vect.df[1,paste("quad_id_",km,"km",sep=""),with=F][[1]]
  #       # }
  #       
  #       vect.df <- vect.df[1,]
  #       vect.df[,neighbors:=vneighbors]
  #       vect.df[,area:=varea]
  #       vect.df[,id:=ids[1]]
  #       # vect.df[,layer:=vlayer]
  #       vect.df[,npl:=vnpl]
  #       vect.df[,pldn:=vpldn]
  #       # vect.df[,plmaxare:=vplmaxare]
  #       vect.df[,plare:=vplare]
  # # vect.df[,sd:=vsd]
  #       vect.df[,DN:=vDN]
  #       vect.df[,paste("quad_id_",km,"km",sep=""):=vquadid,with=F]
  #       vect.df[,paste("quad_ori_",km,"km",sep=""):=vquadori,with=F]
  #       # rm(vquadid)
  #       # rm(vquadori)
  #       # rm(vnpl)
  #       # rm(vpldn)
  #       # rm(vlayer)
  #     }  
    vneighbors <- str_split(vect_dt$neighbors, ",")
    vneighbors <- lapply(vneighbors, function(nb){nb[nb==ids[2]]<-ids[1]; nb})
    vect_dt[,neighbors:=sapply(vneighbors, paste, collapse=",")]
    # rm(vneighbors)
    
    if(nrow(vect.df)>1){
      stop(paste("ERROR in file:",file_id,", quad:",quad_id,". vect.df > 1 rows!"))
    }
    
    vect.union <- st_as_sf(st_union(st_combine(st_as_sf(vect_dt[J(ids),])),by_feature=T))
    vect.df[,geometry:=NULL]
    vect.shp.agg <- cbind(vect.union,vect.df)
    colnames(vect.shp.agg)[colnames(vect.shp.agg)=="x"] <- "geometry"
    st_geometry(vect.shp.agg) <- "geometry"
    st_crs(vect.shp.agg) <- st_crs(st_crs(st_as_sf(vect_dt)))
    # if(vect.min$area>=300){
    vect.shp.agg$sd <- exact_extract(rast, vect.shp.agg, "stdev", progress=F) 
  
    vect_dt[J(ids[1]),names(vect_dt)[-which(names(vect_dt)=="id")]:=vect.shp.agg[,names(vect_dt)[-which(names(vect_dt)=="id")]]]
    vect_dt <- vect_dt[id!=ids[2]]
    
    vect_dt[J(as.numeric(str_split(vect.shp.agg$neighbors,",")[[1]])),toignore:=F]
    

    c <- c+1
    if(c%%200==0){
      # if(log==T){
        print(paste(file_id,quad_id,c,vect.min$area,nrow(vect_dt),sep="-"))
      # }
      # time <- Sys.time()
    }
    vect.min <- NULL
  }
  
  unlink(paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",file_id,"_",quad_id,".gri",sep=""))
  unlink(paste("./classificador_vol_america/rasters/temp/",file_id,"/","clump_v_r",file_id,"_",quad_id,".grd",sep=""))
  
  

  tryCatch({
    tt <- st_area(st_as_sf(vect_dt))
    rm(tt)
  },error=function(e){
    print(paste("ERROR in file:",file_id,", quad:",quad_id,". MULTIPOLIGONS present"))
    stop(paste("ERROR in file:",file_id,", quad:",quad_id,". MULTIPOLIGONS present"))
  })
  
  rm(vect.df)
  rm(vect.min)
  rm(vect.min.neighbors)
  rm(vect.min.neighbors.min)

  
  
  # n.row <- nrow(vect_dt)
  vect_dt <- clump_vector_remove_repited(vect_dt,quad_id,file_id)
  # print(paste("FINISH quad:",quad_id," file:",file_id," ",n.row-nrow(vect_dt)," repeated rows removed",sep=""))
  
  vect_dt[,neighbors:=NULL]
  vect_dt[,toignore:=NULL]

  saveRDS(vect_dt,paste("./classificador_vol_america/vect/temp/clumped/",file_id,"/",quad_id,".rds",sep=""))

  if(log==T){
  print(paste("FINISH",file_id,quad_id,
            # as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024,
            nrow(vect_dt),
            Sys.time(),
            sep=" - "))
  }
  rm(vect_dt)
  gc()
  return(T)
}


create_grid <- function(ext,row,col){
  # if(file.exists(paste("./classificador_vol_america/vect/grids/grid_",row,"x",col,".gpkg",sep=""))){
  #   return(st_read(paste("./classificador_vol_america/vect/grids/grid_",row,"x",col,".gpkg",sep="")))
  # }
  grid <- st_as_sf(st_make_grid(ext,n=c(row,col)))#cellsize = c(3000,3000)))
  grid$id <- 1:nrow(grid)
  grid$row <- 0
  grid$col <- 0
  for(irow in 1:row){
    grid[grid$id%in%((col*irow-col+1):(col*irow)),"row"] <- irow
  }
  for(icol in 1:col){
    ids <- c()
    for(irow in 1:row){
      ids <- append(ids,((irow-1)*col)+icol)
    }
    grid[grid$id%in%ids,"col"] <- icol
  }
  grid$ori <- 0
  grid[grid$col%%2==1&grid$row%%2==1,"ori"] <- 0
  grid[grid$col%%2==0&grid$row%%2==1,"ori"] <- 1
  grid[grid$col%%2==1&grid$row%%2==0,"ori"] <- 2
  grid[grid$col%%2==0&grid$row%%2==0,"ori"] <- 3
  
  # grid[grid$id%%4==0,"ori"] <- 0
  # grid[grid$id%%4==1,"ori"] <- 1
  # grid[grid$id%%4==2,"ori"] <- 2
  # grid[grid$id%%4==3,"ori"] <- 3
  grid <- st_intersection(grid, ext)
  # st_write(grid, paste("./classificador_vol_america/vect/grids/grid_",row,"x",col,".gpkg",sep=""),overwrite=T)
  grid
}

create_grids <- function(){
  limit <- st_read("./classificador_vol_america/rasters/pnoa/LIMADM_PROVINCIA_BCN_1km.gpkg")
  for (ikm in c(1,2,3,5,7,10,20,30)){
    if(ikm==1){
      quads <- create_grid(limit,100,100)
    }else if(ikm==2){
      quads <- create_grid(limit,50,50)
    }else if(ikm==3){
      quads <- create_grid(limit,30,30)
    }else if(ikm==5){
      quads <- create_grid(limit,20,20)
    }else if(ikm==7){
      quads <- create_grid(limit,14,14)
    }else if(ikm==10){
      quads <- create_grid(limit,10,10)
    }else if(ikm==20){
      quads <- create_grid(limit,5,5)
    }else if(ikm==30){
      quads <- create_grid(limit,3,3)
    }
  }
}



clump_vector_ini <- function(){
  # stop("toclump")
  print("clump_vector_ini")
  if(!file.exists("./classificador_vol_america/temp/clump_vector_current.RData")){
    clump_vector_current <- "ini"
    save(clump_vector_current,file="./classificador_vol_america/temp/clump_vector_current.RData")
    clump_vector_ini()
  }else{
    load("./classificador_vol_america/temp/clump_vector_current.RData")
    if(clump_vector_current=="ini"){
      clump_vector_set()
    }else if(clump_vector_current=="set"){
      restartSession(command=source("./classificador_vol_america/scripts/clump_vector_do_launch.R"))
    }else if(clump_vector_current=="clumped_big_files_pendinglast"){
      restartSession(command=source("./classificador_vol_america/scripts/clump_vector_big_files_last.R"))
    }else if(clump_vector_current=="clumped_big_files_last"){
      restartSession(command=source("./classificador_vol_america/scripts/clump_vector_big_files_last.R"))
    }
  }
}

set_quad_ori_kms <- function(vects_dt,kms,limit,log=T){
  library(sf)
  for(ikm in kms){
    vects_dt <- set_quad_ori_km_first(vects_dt,ikm,limit,log)
    vects_dt <- check_vects_not_assigned_to_ori(vects_dt,ikm)
  }
  return(vects_dt)
 
}

get_cuts_from_limit <- function(limit,km){
  library(sf)
  # limit <- read_sf("./classificador_vol_america/vect/grids/limit.gpkg")
  bbox <- st_bbox(limit)
  height <- st_length(st_cast(st_union(st_as_sf(data.frame(lon=bbox[c("xmax","xmax")],
                                                           lat=bbox[c("ymin","ymax")]),
                                                coords=c("lon","lat"),
                                                crs=st_crs(limit))),
                              "LINESTRING"))
  width <- st_length(st_cast(st_union(st_as_sf(data.frame(lon=bbox[c("xmax","xmin")],
                                                           lat=bbox[c("ymin","ymin")]),
                                               coords=c("lon","lat"),
                                               crs=st_crs(limit))),
                              "LINESTRING"))
  ncutsx <- round(as.numeric(width)/(1000*km))
  if(ncutsx==0){
    ncutsx <- 1
  }
  ncutsy <- round(as.numeric(height)/(1000*km))
  if(ncutsy==0){
    ncutsy <- 1
  }
  
  xcuts<-seq(bbox["xmin"],bbox["xmax"],(bbox["xmax"]-bbox["xmin"])/ncutsx)
  ycuts<-seq(bbox["ymin"],bbox["ymax"],(bbox["ymax"]-bbox["ymin"])/ncutsy)
  return(list("xcuts"=xcuts,"ycuts"=ycuts,"ncutsx"=ncutsx,"ncutsy"=ncutsy))
}

set_quad_ori_km_first <- function(vects_dt,ikm,limit,log=T){
  if(log==T){
    print(paste(ikm,"km"))
  }
 xmins <- sapply(vects_dt$geometry,function(v){st_bbox(v)$xmin})
  ymins <- sapply(vects_dt$geometry,function(v){st_bbox(v)$ymin})
  xmaxs <- sapply(vects_dt$geometry,function(v){st_bbox(v)$xmax})
  ymaxs <- sapply(vects_dt$geometry,function(v){st_bbox(v)$ymax})
  
  cuts <- get_cuts_from_limit(limit,ikm)
  
  vects_dt[, `:=`(xmin=as.integer(cut(xmins,breaks=cuts$xcuts,labels=1:cuts$ncutsx,include.lowest=T)),
             ymin=as.integer(cut(ymins,breaks=cuts$ycuts,labels=1:cuts$ncutsy,include.lowest=T)),
             xmax=as.integer(cut(xmaxs,breaks=cuts$xcuts,labels=1:cuts$ncutsx)),
             ymax=as.integer(cut(ymaxs,breaks=cuts$ycuts,labels=1:cuts$ncutsy)))]  
  
  # setkey(vects_dt,c("xmin","xmax","ymin","ymax"))
  
  vects_dt$ori_xminymin <- 0
  vects_dt[xmin%%2==1&ymin%%2==1,ori_xminymin:=0]
  vects_dt[xmin%%2==1&ymin%%2!=1,ori_xminymin:=1]
  vects_dt[xmin%%2!=1&ymin%%2==1,ori_xminymin:=2]
  vects_dt[xmin%%2!=1&ymin%%2!=1,ori_xminymin:=3]
  vects_dt$ori_xmaxymax <- 0
  vects_dt[xmax%%2==1&ymax%%2==1,ori_xmaxymax:=0]
  vects_dt[xmax%%2==1&ymax%%2!=1,ori_xmaxymax:=1]
  vects_dt[xmax%%2!=1&ymax%%2==1,ori_xmaxymax:=2]
  vects_dt[xmax%%2!=1&ymax%%2!=1,ori_xmaxymax:=3]
  vects_dt$ori_xmaxymin <- 0
  vects_dt[xmax%%2==1&ymin%%2==1,ori_xmaxymin:=0]
  vects_dt[xmax%%2==1&ymin%%2!=1,ori_xmaxymin:=1]
  vects_dt[xmax%%2!=1&ymin%%2==1,ori_xmaxymin:=2]
  vects_dt[xmax%%2!=1&ymin%%2!=1,ori_xmaxymin:=3]
  vects_dt$ori_xminymax <- 0
  vects_dt[xmin%%2==1&ymax%%2==1,ori_xminymax:=0]
  vects_dt[xmin%%2==1&ymax%%2!=1,ori_xminymax:=1]
  vects_dt[xmin%%2!=1&ymax%%2==1,ori_xminymax:=2]
  vects_dt[xmin%%2!=1&ymax%%2!=1,ori_xminymax:=3]
  
 
  ids <- data.table(setNames(as.data.frame(expand.grid(1:cuts$ncutsx,1:cuts$ncutsy)),c("x","y")))
  ids[,id:=.GRP,by=list(x,y)]
  # vects_dt[,id_xminymin:=.GRP, by=list(xmin, ymin)]
  vects_dt <- merge(vects_dt,setNames(ids,c("x","y","id_xminymin")),by.x=c("xmin","ymin"),by.y=c("x","y"))
  vects_dt <- merge(vects_dt,setNames(ids,c("x","y","id_xmaxymin")),by.x=c("xmax","ymin"),by.y=c("x","y"))
  vects_dt <- merge(vects_dt,setNames(ids,c("x","y","id_xminymax")),by.x=c("xmin","ymax"),by.y=c("x","y"))
  vects_dt <- merge(vects_dt,setNames(ids,c("x","y","id_xmaxymax")),by.x=c("xmax","ymax"),by.y=c("x","y"))
  
  vects_dt[,paste("quad_ori_",ikm,"km",sep=""):=apply(.SD,1,function(x){
    paste(unique(x)[order(as.numeric(unique(x)))],collapse=",")
  }),.SDcols=c("ori_xminymin",
               "ori_xminymax",
               "ori_xmaxymin",
               "ori_xmaxymax")]
  
  vects_dt[,paste("quad_id_",ikm,"km",sep=""):=apply(.SD,1,function(x){
    paste(unique(x)[order(as.numeric(unique(x)))],collapse=",")
  }),.SDcols=c("id_xminymin",
               "id_xminymax",
               "id_xmaxymin",
               "id_xmaxymax")]
 
  vects_dt[,paste("quad_row_",ikm,"km",sep=""):=apply(.SD,1,function(x){
    paste(x[as.numeric(order(x))][1]:x[as.numeric(order(x))][length(x)],collapse=",")
    }),.SDcols=c("ymin","ymax")]
  
  vects_dt[,paste("quad_col_",ikm,"km",sep=""):=apply(.SD,1,function(x){
    paste(x[as.numeric(order(x))][1]:x[as.numeric(order(x))][length(x)],collapse=",")
  }),.SDcols=c("xmin","xmax")]
  
  vects_dt$id_xmaxymax <- NULL
  vects_dt$id_xminymax <- NULL
  vects_dt$id_xmaxymin <- NULL
  vects_dt$id_xminymin <- NULL
  vects_dt$ori_xminymin <- NULL
  vects_dt$ori_xmaxymax <- NULL
  vects_dt$ori_xmaxymin <- NULL
  vects_dt$ori_xminymax <- NULL
  vects_dt$xmin <- NULL
  vects_dt$xmax <- NULL
  vects_dt$ymin <- NULL
  vects_dt$ymax <- NULL
  
  return(vects_dt)
  
}

check_vects_not_assigned_to_ori <- function(vects,km){
  nas <- as.data.frame(vects)[is.na(vects[,paste("quad_ori_",km,"km",sep="")]),"id"] 
  for(naid in nas){
    print(paste("naid", naid))
    if(naid==as.data.frame(vects)[,"id"][1]){
      for(imes in ((naid+1):as.data.frame(vects)[,"id"][nrow(vects)])[((naid+1):as.data.frame(vects)[,"id"][nrow(vects)])%in%as.data.frame(vects)[,"id"]]){
        # print(imas)
        if(!is.na(as.data.frame(vects[vects$id==imes,])[,paste("quad_ori_",km,"km",sep="")])){
          vects[vects$id==naid,paste("quad_ori_",km,"km",sep="")] <- as.data.frame(vects[vects$id==imes,])[,paste("quad_ori_",km,"km",sep="")]
          vects[vects$id==naid,paste("quad_id_",km,"km",sep="")] <- as.data.frame(vects[vects$id==imes,])[,paste("quad_id_",km,"km",sep="")]
          vects[vects$id==naid,paste("quad_row_",km,"km",sep="")] <- as.data.frame(vects[vects$id==imes,])[,paste("quad_row_",km,"km",sep="")]
          vects[vects$id==naid,paste("quad_col_",km,"km",sep="")] <- as.data.frame(vects[vects$id==imes,])[,paste("quad_col_",km,"km",sep="")]
          break()
        }
      }
    }else{
      for(imenos in rev(as.data.frame(vects)[,"id"][1]:(naid-1))[rev(as.data.frame(vects)[,"id"][1]:(naid-1))%in%as.data.frame(vects)[,"id"]]){
        # print(imenos)
        if(!is.na(as.data.frame(vects[vects$id==imenos,])[,paste("quad_ori_",km,"km",sep="")])){
          vects[vects$id==naid,paste("quad_ori_",km,"km",sep="")] <- as.data.frame(vects[vects$id==imenos,])[,paste("quad_ori_",km,"km",sep="")]
          vects[vects$id==naid,paste("quad_id_",km,"km",sep="")] <- as.data.frame(vects[vects$id==imenos,])[,paste("quad_id_",km,"km",sep="")]
          vects[vects$id==naid,paste("quad_row_",km,"km",sep="")] <- as.data.frame(vects[vects$id==imenos,])[,paste("quad_row_",km,"km",sep="")]
          vects[vects$id==naid,paste("quad_col_",km,"km",sep="")] <- as.data.frame(vects[vects$id==imenos,])[,paste("quad_col_",km,"km",sep="")]
          break()
        }
      }
    }
  }
  rm(nas)
  rm(naid)
  rm(imes)
  rm(imenos)
  gc()
  vects
}

add_colums_clump <- function(vects){
  vects$id <- 1: nrow(vects)
  if(!"npl"%in%colnames(vects)){
    vects$npl <- 0
  }
  if(!"pldn"%in%colnames(vects)){
    vects$pldn <- ""
  }
  # vects$pldn <- NA
  if(!"plare"%in%colnames(vects)){
    vects$plare <- ""
  }
  if(!"DN"%in%colnames(vects)){
    vects$DN <- NA
  }
  if(!"sd"%in%colnames(vects)){
    vects$sd <- NA
  }
  # vects$plare <- NA
  # vects$plmaxare <- NA
  # columns <- c("id","npl", "pldn", "plmaxare")
  # vects <- vects[,append(colnames(vects),columns)]
  vects
}

clump_vector_set_km_all <- function(km,readdir="./classificador_vol_america/vect/vectorised/split/set/"){
  clump_vector_set_km_move(km,readdir)
  clump_vector_set_km_merge(km)
}

clump_vector_set_km_move <- function(km,
                                readdir="./classificador_vol_america/vect/vectorised/split/set/"){
  print(paste("clump_vector_set_km_move",km,readdir))
  dir.create("./classificador_vol_america/vect/set",showWarnings=F)
  readdir <- paste(readdir,"/",sep="")
  files_list <- list.files(readdir,
                           pattern="\\.rds$")
  
  # tt <- unique(sapply(files_list,function(file,readdir){
  #   vect <- readRDS(paste(readdir,file,sep="")) 
  #   unique(vect[,paste("quad_id_",km,"km",sep=""),with=F])
  # },readdir))
  
  # names(files_list) <- sapply(files_list,function(x){
  #   as.numeric(sub("id","",sub(".rds","",x)))
  # }) 
  if(length(files_list)>0){
    dir.create(paste("./classificador_vol_america/vect/set/km",km,sep=""),showWarnings=F)
    cl <- create_cluster_clump_(detectCores(),"clump_vector_set")
    parLapplyLB(cl,files_list,function(file,km,readdir){
      print(file)
      vect <- readRDS(paste(readdir,file,sep="")) 
      spl_oris <- split(vect,by=paste("quad_ori_",km,"km",sep=""))
      lapply(spl_oris,function(spl_ori,km,file){
        ori <- spl_ori[,paste("quad_ori_",km,"km",sep=""),with=F][[1]][1]
        ori <- str_split(ori,",")[[1]]
        ori <- paste(ori[order(as.numeric(ori))],collapse=",")
        dir.create(paste("./classificador_vol_america/vect/set/km",km,"/ori",ori,sep=""),showWarnings=F)
        spl_ids <- split(spl_ori,by=paste("quad_id_",km,"km",sep=""))
        lapply(spl_ids,function(spl_id,km,ori,file){
          id <- spl_id[,paste("quad_id_",km,"km",sep=""),with=F][[1]][1]
          id <- str_split(id,",")[[1]]
          id <- paste(id[order(as.numeric(id))],collapse=",")
          unlink(paste("./classificador_vol_america/vect/set/km",km,"/ori",ori,"/id",id,"/",file,sep=""))
          dir.create(paste("./classificador_vol_america/vect/set/km",km,"/ori",ori,"/id",id,sep=""),showWarnings=F)
          saveRDS(spl_id,paste("./classificador_vol_america/vect/set/km",km,"/ori",ori,"/id",id,"/",file,sep=""))
        },km,ori,file)
      },km,file)
      # unlink(paste(readdir,
                   # file,sep=""))
    },km,readdir)
    stopCluster(cl)
    rm(cl)
    rm(files_list)
    unlink("./classificador_vol_america/logs/clump_vector_set.txt")
  }
}

clump_vector_set_km_merge <- function(km){
  oridirs <- list.dirs(paste("./classificador_vol_america/vect/set/km",km,"/",sep=""),full.names=TRUE,recursive=F)
  cl <- create_cluster_clump_(detectCores(),"clump_vector_set")
  parLapplyLB(cl,oridirs,function(oridir,km){
    print(oridir)
    iddirs <- list.dirs(oridir,full.names=F,recursive=F)
    lapply(iddirs,function(iddir,oridir,km){
      files <- list.files(paste(oridir,"/",iddir,sep=""),pattern="\\.rds$")
      merged <- rbindlist(lapply(files,function(f,oridir,iddir){
        readRDS(paste(oridir,"/",iddir,"/",f,sep=""))
      },oridir,iddir),use.names=T,fill=T)
      unlink(paste(oridir,"/",iddir,".rds",sep=""))
      saveRDS(merged,paste(oridir,"/",iddir,".rds",sep=""))
      unlink(paste(oridir,"/",iddir,sep=""),recursive=T)
    },oridir,km)  
  },kms[1])
  stopCluster(cl)
  rm(cl)
  unlink("./classificador_vol_america/logs/clump_vector_set.txt")
}


clump_vector_set <- function(){
  files_list <- list.files("./classificador_vol_america/vect/vectorised/split/simplified/",
                           pattern="\\.rds$")
  names(files_list) <- sapply(files_list,function(x){
    as.numeric(sub(".rds","",x))
  })  
  # done <- list.files("./classificador_vol_america/vect/vectorised/split/set/",
  # pattern="\\.rds$")
  # files_list <- files_list[!files_list%in%done]
  print(paste("clump vector SET ",length(files_list), " files",sep=""))
  if(length(files_list)>0){
    dir.create("./classificador_vol_america/vect/vectorised/split/set/",showWarnings=F)
    # source("./classificador_vol_america/scripts/clump_vector.R")
    # load("./classificador_vol_america/temp/resolution.RData")
    limit <- st_read("./classificador_vol_america/vect/limit.gpkg")
    if(!file.exists("./classificador_vol_america/temp/kms.RData")){
      load("./classificador_vol_america/temp/resolution.RData")
      # kms <- resolution*c(4,8,12,20,28,36,44,52,68,92,124,172,236,356,604,1052,1532,2012,3076,4195)
      kms <- resolution*c(12,20,28,36,44,52,68,76,92,124,172,236,356,604,1052,1532,2012,3076,4195)
      limit_width <- as.numeric(round(sqrt(st_area(st_as_sfc(st_bbox(limit))))))
      kms <- kms[kms<limit_width/(resolution*6000)]
      kms <- kms[order(kms)]
      save(kms,file="./classificador_vol_america/temp/kms.RData")
      rm(limit_width)
    }else{
      load("./classificador_vol_america/temp/kms.RData")
    }
    library(parallel)
    unlink("./classificador_vol_america/logs/clump_vector_set.txt")
    cl <- create_cluster_clump_(detectCores(),"clump_vector_set")
    parLapplyLB(cl,files_list,function(file,kms){
      print(file)
      vect <- readRDS(paste("./classificador_vol_america/vect/vectorised/split/simplified/",
                            file,sep=""))
      vect$layer <- NULL
      vect <- add_colums_clump(vect)
      vect <- set_quad_ori_kms(vect,kms[1],limit)
      unlink(paste("./classificador_vol_america/vect/vectorised/split/set/",
                   file,sep=""))
      saveRDS(vect,paste("./classificador_vol_america/vect/vectorised/split/set/",
                         file,sep=""))
      # unlink(paste("./classificador_vol_america/vect/vectorised/split/simplified/",
      #              file,sep=""))
    },kms)
    stopCluster(cl)
    rm(cl)
    unlink("./classificador_vol_america/logs/clump_vector_set.txt")
    rm(limit)
  }
  rm(files_list)
  # 
  # files <- list.files("./classificador_vol_america/vect/vectorised/split/set/")
  # tt <- unique(unlist(sapply(files,function(file){
  #   unique(readRDS(paste("./classificador_vol_america/vect/vectorised/split/set/",file,sep=""))[,quad_id_3km])
  # })))
  # 
  clump_vector_set_km_all(kms[1])
  
  kms_done <- c()
  save(kms_done, file="./classificador_vol_america/temp/kms_done.RData")
  oris_done <- c()
  save(oris_done, file="./classificador_vol_america/temp/oris_done.RData")
  # ids_done <- c()
  # save(ids_done, file="./classificador_vol_america/temp/ids_done.RData")
  
  clump_vector_current <- "set"
  save(clump_vector_current,file="./classificador_vol_america/temp/clump_vector_current.RData")
  clump_vector_ini()
}


clump_vector_do_small_files <- function(){
  print("clump_vector_do_small_files")
  if(file.exists("./classificador_vol_america/vect/clumped/global_clmp.rds")){
    return()
  }
  rast <- raster("./classificador_vol_america/rasters/original/original.tif")
  load("./classificador_vol_america/temp/kms.RData")
  vects <-readRDS("./classificador_vol_america/vect/clumped/global_clmp_temp.rds")
  kms <- kms[-1]
  for (ikm in kms){
    if(file.exists(paste("./classificador_vol_america/vect/clumped/global_clmp_",ikm,"km.rds"))){
      next()
    }
    print(paste(ikm,"km",sep=""))
    # vects <-readRDS("./classificador_vol_america/vect/clumped/global_clmp_temp.rds")
    vects <- clump_vector_do_small_files.launch_km(vects,kms[ikm],rast)
    dir.create("./classificador_vol_america/vect/clumped/kms",showWarnings=F)
    saveRDS(vects, paste("./classificador_vol_america/vect/clumped/kms/global_clmp_",ikm,"km.rds", sep=""))
    saveRDS(vects,"./classificador_vol_america/vect/clumped/global_clmp_temp.rds",overwrite=T)
  }
  saveRDS(vects, "./classificador_vol_america/vect/clumped/global_clmp.rds")
  unlink("./classificador_vol_america/vect/clumped/kms",recursive=T)
  # for(ikm in kms[-1]){
  #   unlink(paste("./classificador_vol_america/vect/clumped/global_clmp_",ikm,"km.rds", sep=""))
  # }
  unlink("./classificador_vol_america/vect/clumped/global_clmp_temp.rds")
  vects
}
clump_vector_do_small_files.launch_km <- function(vects,ikm,rast,file_id){
  clump_vector_do_small_files.launch_km_specifyarea(vects,ikm,rast,100*ikm,file_id)
}
clump_vector_do_small_files.launch_km_specifyarea <- function(vects_dt,ikm,rast,arealimit,file_id,method=NULL,bigarea=F,simplify=F,parallel=T,log=T){
  vects_dt[,id_base:=1:nrow(vects_dt)]
  setkey(vects_dt,id_base)
  oris <- as.numeric(unique(unlist(str_split(vects_dt[[which(colnames(vects_dt)==paste("quad_ori_",ikm,"km",sep=""))]],","))))
  unlink(paste("./classificador_vol_america/vect/temp/clumped/",file_id,sep=""),recursive=T)
  unlink(paste("./classificador_vol_america/vect/temp/",file_id,sep=""),recursive=T)
  unlink(paste("./classificador_vol_america/rasters/temp/",file_id,sep=""),recursive=T)
  dir.create(paste("./classificador_vol_america/vect/temp/",file_id,sep=""))
  dir.create(paste("./classificador_vol_america/rasters/temp/",file_id,sep=""))
  for(ori in oris){
    vects_ori <- vects_dt[sapply(vects_dt[[which(colnames(vects_dt)==paste("quad_ori_",ikm,"km",sep=""))]],function(x,ori){
      if(ori%in%str_split(x,",")[[1]]){
        return(T)
      }else{
        return(F)
      }
    },ori),]
    vects_rest <- vects_dt[!id_base%in%vects_ori$id_base,]
    # if(bigarea==T){
    #   saveRDS(vects_rest,
    #           paste("./classificador_vol_america/vect/temp/",file_id,"/",
    #                 file_id,"_vects_rest.rds",
    #                 sep=""))
    #   rm(vects_rest)
    #   # gc()
    # }
    rm(vects_dt)
    ids_ori <- unique(vects_ori[[which(colnames(vects_ori)==paste("quad_id_",ikm,"km",sep=""))]])[sapply(str_split(unique(vects_ori[[which(colnames(vects_ori)==paste("quad_id_",ikm,"km",sep=""))]]),","),length)==1]
    vects_ori[,(paste("quad_id_",ikm,"_km_ori_",ori,sep="")):=unlist(sapply(str_split(vects_ori[[which(colnames(vects_ori)==paste("quad_id_",ikm,"km",sep=""))]],","),function(x,ids_ori){
      x[x%in%ids_ori][1]
    },ids_ori))]
    
    if(length(unique(vects_ori[[which(colnames(vects_ori)==paste("quad_id_",ikm,"_km_ori_",ori,sep=""))]]))>1){
      vects_quad_ori_list <- split(vects_ori, vects_ori[[which(colnames(vects_ori)==paste("quad_id_",ikm,"_km_ori_",ori,sep=""))]])
      vects_quad_ori_list <- vects_quad_ori_list[order(sapply(vects_quad_ori_list,nrow),decreasing=T)]
      n.cores <- get_core_number_(vects_quad_ori_list)
      if(n.cores<2){
        print(paste("cores",n.cores))
        n.cores <- detectCores()
      }
      # print(n.cores)
      
      vv <- split(vects_quad_ori_list, ceiling(seq_along(vects_quad_ori_list)/n.cores))
      vects_quad_ori_list <- list()
      for(i in 1:length(vv)){
        if(i%%2!=0){
          vv[[i]] <- vv[[i]][order(sapply(vv[[i]],nrow),decreasing=T)]
        }else{
          vv[[i]] <- vv[[i]][order(sapply(vv[[i]],nrow),decreasing=F)]
        }
        vects_quad_ori_list <- append(vects_quad_ori_list,vv[[i]])
      }
      rm(vv)
      vv <- list()
      for(i in 1:detectCores()){
        vv <- append(vv,vects_quad_ori_list[i])
        c <- 1
        while((c*detectCores())+i<=length(vects_quad_ori_list)){
          vv <- append(vv,vects_quad_ori_list[(c*detectCores())+i])
          c <- c+1
        }
      }
      vects_quad_ori_list <- vv
      rm(vv)
      vects_quad_ori_list <- split(vects_quad_ori_list, cut(seq_along(vects_quad_ori_list),detectCores(),labels=F))
      rm(vects_ori)
      rm(ids_ori)
      
      print(paste(file_id, " ori:",ori," length:",sum(sapply(vects_quad_ori_list,length)),sep=""))
      
      if(parallel==T){
        unlink("./classificador_vol_america/logs/clump_vector_do_small_files.launch_km_specifyarea.txt")
        cl <- create_cluster_clump_(n.cores,"clump_vector_do_small_files.launch_km_specifyarea")
        parLapplyLB(cl, vects_quad_ori_list, function(vv,rast,quad_id_col,arealimit,method,file_id,ikm,log){
          # lapply(vects_quad_ori_list, function(vv,rast,quad_id_col,arealimit,method,file_id){
          lapply(vv,function(v,rast,quad_id_col,arealimit,method,file_id,ikm,log){
            if(is.null(v)){
              return()
            }
            quad_id <- v[[which(colnames(v)==quad_id_col)]][1]
            clump_vector_simplify(v,rast,quad_id,file_id,ikm,log=log,prepare=T)
          },rast,quad_id_col,arealimit,method,file_id,ikm,log)
        },rast,paste("quad_id_",ikm,"_km_ori_",ori,sep=""),arealimit,method,file_id,ikm,log)
        stopCluster(cl)
        unlink("./classificador_vol_america/logs/clump_vector_do_small_files.launch_km_specifyarea.txt")
        rm(cl)
      }else{
        lapply(vects_quad_ori_list, function(vv,rast,quad_id_col,arealimit,method,file_id,ikm,log){
          # lapply(vects_quad_ori_list, function(vv,rast,quad_id_col,arealimit,method,file_id){
          lapply(vv,function(v,rast,quad_id_col,arealimit,method,file_id,ikm,log){
            if(is.null(v)){
              return()
            }
            quad_id <- v[[which(colnames(v)==quad_id_col)]][1]
            # quad_id <- v[,quad_id_col]
            clump_vector_simplify(v,rast,quad_id,file_id,ikm,log=log,prepare=T)
          },rast,quad_id_col,arealimit,method,file_id,ikm,log)
        },rast,paste("quad_id_",ikm,"_km_ori_",ori,sep=""),arealimit,method,file_id,ikm,log)
        
      }
      
      rm(vects_quad_ori_list)
    }else{
      print(paste(file_id, " ori:",ori," length:",1,sep=""))
      
      clump_vector_simplify(vects_ori,
                            rast,
                            vects_ori[[which(colnames(vects_ori)==paste("quad_id_",ikm,"_km_ori_",ori,sep=""))]][1],
                            file_id,
                            ikm,
                            log=log,
                            prepare=T)
    }
    vects_ori <- lapply(paste("./classificador_vol_america/vect/temp/clumped/",file_id,"/",
                              list.files(paste("./classificador_vol_america/vect/temp/clumped/",file_id,"/",sep="")),
                              sep=""),readRDS)
    if(length(vects_ori)==1){
      vects_ori <- vects_ori[[1]]
    }else{
      vects_ori <- rbindlist(vects_ori,use.names=T)
    }
    unlink(paste("./classificador_vol_america/vect/temp/clumped/",file_id,sep=""),recursive=T)
    vects_ori[,area:=NULL]
    vects_ori[,(paste("quad_id_",ikm,"_km_ori_",ori,sep="")):=NULL]
    if(bigarea==T){
      vects_rest <- readRDS(paste("./classificador_vol_america/vect/temp/clumped//",file_id,"/",
                                  file_id,"_vects_rest.rds",
                                  sep=""))
    }
    vects_dt <- rbindlist(list(vects_ori,vects_rest),use.names=T)
    
    vects_dt[,paste("quad_ori_",ikm,"km",sep=""):=sapply(str_split(vects_dt[,paste("quad_ori_",ikm,"km",sep=""),with=F][[1]],
                                                              ","),function(ori){
                                                                paste(unique(ori)[order(as.numeric(unique(ori)))],collapse=",")
                                                              })] 
    vects_dt[,paste("quad_id_",ikm,"km",sep=""):=sapply(str_split(vects_dt[,paste("quad_id_",ikm,"km",sep=""),with=F][[1]],
                                                             ","),function(id){
                                                               paste(unique(id)[order(as.numeric(unique(id)))],collapse=",")
                                                             })] 
    
    
    setkey(vects_dt,"id_base")
    rm(vects_ori)
    rm(vects_rest)
  }
  vects_dt[,id_base:=NULL]
  unlink(paste("./classificador_vol_america/vect/temp/clumped/",file_id,sep=""),recursive=T)
  unlink(paste("./classificador_vol_america/vect/temp/",file_id,sep=""),recursive=T)
  unlink(paste("./classificador_vol_america/rasters/temp/",file_id,sep=""),recursive=T)
  vects_dt
}


clump_vector_combine.finalise <- function(){
  
  
}

get_potential_core_number <- function(vects_quad_ori_list,vects=NULL){
  free.mem <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))
  if(!is.null(vects)){
    free.mem <- free.mem + (as.numeric(object.size(vects))/1024)
  }
  n.cores <- floor((free.mem/1024)/(8*sqrt((as.numeric(object.size(vects_quad_ori_list))/1024))))
  rm(free.mem)
  n.cores
}
get_core_number_ <- function(vects_quad_ori_list){
  n.cores <- get_potential_core_number(vects_quad_ori_list)
  if(detectCores()<n.cores){
    n.cores <- detectCores()
  }
  n.cores
}
create_cluster_clump_ <- function(n.cores,outfile=NULL){
  # print("cl")
  if(Sys.info()['sysname']=="Windows"){
    n.cores <- n.cores -2
    if(n.cores<=0){
      n.cores <- 1
    }
    cl <- makeCluster(n.cores, outfile=paste("./classificador_vol_america/logs/",outfile,".txt",sep=""))
    clusterExport(cl, list("clump_vector"))
    clusterEvalQ(cl, list(library(maptools), library(sf), library(stringr), library(raster), library(exactextractr)))
    
  }else if(Sys.info()['sysname']=="Linux"){
    cl <- makeForkCluster(n.cores, outfile=paste("./classificador_vol_america/logs/",outfile,".txt",sep=""))
    clusterSetRNGStream(cl, 1001)
  }
  cl
}

library(methods)

ori_km_fileweight_pncores_list <- setRefClass("ori_km_fileweight_pncores_list", 
                                              fields = list(list = "list"),
                                              methods = list(
                                                add = function(km,ori,vects){
                                                  new <- ori_km_fileweight_pncores(ori=ori,km=km)
                                                  new.set_fileweight_pnocres(vects)
                                                  list <- append(list,new)
                                                },
                                                get_by_km = function(km){
                                                  unlist(lapply(list, function(l){
                                                    if(l$km==km){
                                                      return(l)  
                                                    }else{
                                                      return(NULL)
                                                    }
                                                  }))
                                                } 
                                              ))

ori_km_fileweight_pncores <- setRefClass("ori_km_fileweight_pncores", 
                                         fields = list(ori = "numeric", 
                                                       km = "numeric", 
                                                       fileweight = "numeric", 
                                                       pncores = "numeric"), 
                                         methods = list(
                                           set_fileweight_pnocres = function(vects)
                                           {
                                             vects_ori_list <- split(vects[as.data.frame(vects)[,paste("quad_ori_",km,"km",sep="")]==ori,], 
                                                                     as.data.frame(vects)[as.data.frame(vects)[,paste("quad_ori_",km,"km",sep="")]==ori,paste("quad_id_",km,"km",sep="")])
                                             fileweight <<- as.numeric(object.size(vects_ori_list))/1024
                                             pncores <<- get_potential_core_number(vects_ori_list,vects)
                                             rm(vects_ori_list)
                                             gc()
                                           }
                                         ))