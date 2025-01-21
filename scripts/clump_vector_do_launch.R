library(stringr)
library(parallel)
library(rstudioapi)
source("./scripts/clump_vector.R")
print(paste(Sys.time(),"clump vector do launch"))

load("./temp/kms.RData")
load("./temp/kms_done.RData")
load("./temp/oris_done.RData")
# load("./temp/ids_done.RData")

kms_all <- kms
kms <- kms[!kms%in%kms_done]

if(length(kms)==0){
  stop("finished kms")  
  lastkm <- kms_all[length(kms_all)]
  lastarealimit <- lastkm*(1+log(lastkm,3)/10)*100
  vect <- readRDS("./vect/set/all.rds")
  km <- "all"
  ori <- "all"
  id <- "all"
  vect$quad_ori_allkm <- 1
  vect$quad_id_allkm <- 1
  vect$quad_col_allkm <- 1
  vect$quad_row_allkm <- 1
  
  time <- Sys.time()
  print(paste("Clump vector do launch km:",km," ori:",ori," id:",id," nrow:",nrow(vect)," ",Sys.time(),sep=""))
  dir.create(paste("./vect/temp/clumped/",paste(km,"_",ori,sep=""),sep=""),showWarnings=F)
  unlink(paste("./vect/temp/clumped/",paste(km,"_",ori,sep=""),"/",id,".rds",sep=""))
  
  rast <- raster("./rasters/original/original_normalised.tif")
  tryCatch({
    clump_vector(vect,rast,quad_id=id,file_id=gsub("\\.","-",paste(km,"_",ori,sep="")),
                 method="closest",log=T,prepare=T,km=km,
                 parallelextract=T,forceddnextract=F,lastkm=lastkm,lastarealimit=lastarealimit)
  }, error = function(e) {
    # error not related to timeout
    print(paste("ERROR in km: ",km,", ori: ",ori,", id: ",id,". ", conditionMessage(e)))
    # message(paste("ERROR in km: ",km,", ori: ",ori,", id: ",id,". ", conditionMessage(e)))
    stop(e)
    # stop(e)
  })
  rm(rast)
  vect <- readRDS(paste("./vect/temp/clumped/",gsub("\\.","-",paste(km,"_",ori,sep="")),"/",id,".rds",sep=""))
  saveRDS(vect,"./vect/clumped/all.rds")
  rm(vect)
  gc()
  current_global <- "clumped"
  save(current_global,file="./temp/current_global.RData")
  unlink("./temp/smoothen_raster_current.RData")
  unlink("./temp/kms.RData")
  unlink("./temp/kms_done.RData")
  unlink("./temp/oris_done.RData")
  unlink("./temp/clump_tomovedone.RData")
  library(rstudioapi)
  restartSession(command=source("./scripts/continue.R"))

}else{
  km <- kms[1]
}
if(km==kms_all[1]){
  lastkm <- NULL
  lastarealimit <- NULL
}else{
  lastkm <- kms_all[which(kms_all==km)-1]
  lastarealimit <- lastkm*(1+log(lastkm,3)/10)*100  
}

if(which(kms_all==km)%%4==0){
  forceddnextract <- T
}else{
  forceddnextract <- F
}
# rm(kms)
# rm(kms_done)

oris <- list.dirs(paste("./vect/set/km",km,sep=""),
                  full.names=F,recursive=F)
oris <- unique(unlist(str_split(sub("ori","",oris),",")))
oris <- oris[order(oris)]
oris <- oris[!oris%in%oris_done]

if(length(oris)==0){
  # stop("km")
  # print(paste(""))
  # stop("new km")
  if(length(kms)>1){
    dirs <- list.dirs(paste("./vect/set/km",km,sep=""))
    
    lapply(dirs,function(dir,newkm,oldkm){
      print(dir)
      dir <- paste(dir,"/",sep="")
      files_list <- list.files(dir,pattern="\\.rds$")
      if(length(files_list)==0|grepl("/tomove",dir,fixed=TRUE)){
        return(T)
      }
      dir.create(paste(dir,"tomove/",sep=""))
      limit <- st_read("./vect/limit.gpkg")
      
      unlink("./logs/clump_vector_set_quad_ori_kms_dir.txt")
      cl <- create_cluster_clump_(detectCores(),"clump_vector_set_quad_ori_kms_dir")
      parLapplyLB(cl,files_list,function(file,newkm,dir,limit,oldkm){
        print(file)
        vect <- readRDS(paste(dir,file,sep=""))
        vect <- set_quad_ori_kms(vect,newkm,limit)
        # vect[,paste("quad_id_",oldkm,"km",sep="")] <- NULL
        # vect[,paste("quad_ori_",oldkm,"km",sep="")] <- NULL
        # vect[,paste("quad_col_",oldkm,"km",sep="")] <- NULL
        # vect[,paste("quad_row_",oldkm,"km",sep="")] <- NULL
        unlink(paste(dir,"tomove/",file,sep=""))
        saveRDS(vect,paste(dir,"tomove/",file,sep=""))
        # unlink(paste(dir,file,sep=""))
      },newkm,dir,limit,oldkm)
      stopCluster(cl)
      rm(cl)
      unlink("./logs/clump_vector_set_quad_ori_kms_dir.txt")
    },newkm=kms[-which(kms==km)][1],oldkm=km)
    dirs <- list.dirs(paste("./vect/set/km",km,sep=""))
    dirs <- dirs[grepl("/tomove",dirs,fixed=T)]
    lapply(dirs,function(dir,km,kms){
      clump_vector_set_km_move(kms[-which(kms==km)][1],readdir=dir)
    },km,kms)
    clump_vector_set_km_merge(kms[-which(kms==km)][1])
  }else{
    dirs <- list.dirs(paste("./vect/set/km",km,sep=""))
    vects <- lapply(dirs,function(dir){
      rbindlist(lapply(list.files(dir,pattern="\\.rds$",full.names=T),readRDS),use.names=T)
    })
    vects <- rbindlist(vects[sapply(vects,nrow)>0],use.names=T)
    saveRDS(vects,"./vect/set/all.rds")
    # unlink(paste("./vect/set/km",km,sep=""))
  }
  
  if(is.null(kms_done)){
    kms_done <- km
  }else{
    kms_done <- unique(append(kms_done,km))
  }
  oris_done <- NULL
  save(oris_done,file="./temp/oris_done.RData")
  save(kms_done,file="./temp/kms_done.RData")
  # unlink(paste("./vect/set/",km,sep=""))
  rm(list=ls())
  gc()
  # stop("km done")
  restartSession(command=source("./scripts/clump_vector_do_launch.R"))
}else{
  ori <- oris[1]
}
rm(oris)

# test <- function(){
#     km <- 3
#     oris <- 0:3
#     lapply(oris,function(ori,km){
#       oridirs <- list.dirs(paste("./vect/set/km",km,sep=""),
#                            full.names=T,recursive=F)[sapply(str_split(sub("ori","",
#                                                                           list.dirs(paste("./vect/set/km",km,sep=""),
#                                                                                     full.names=F,recursive=F)),","),
#                                                             function(o,ori){
#                                                               ori%in%o
#                                                             },ori)]
#       lapply(oridirs,function(oridir,km,ori){
#         files <- list.files(oridir)[list.files(oridir)!="done"]
#         cl <- create_cluster_clump_(detectCores(),"test_check_errors")
#         parSapplyLB(cl,files,function(file,oridir){
#           # print(file)
#           tryCatch({
#             vect <- readRDS(paste(oridir,"/",file,sep="")) 
#           }, error = function(e) {
#             print(paste("error ",file,sep=""))
#           })
#           if(is.null(vect)){
#             return()
#           }
#           if(nrow(vect)==0){
#             print(paste("0 lines ",file,sep=""))
#           }
#         },oridir)
#         stopCluster(cl)
#         gc()
#       },km,ori)
#       
#     },km)
# 
#   
# }

# test <- function(){
#   km <- 3
#   ori <- 3
#   oridirs <- list.dirs(paste("./vect/set/km",km,sep=""),
#                        full.names=T,recursive=F)[sapply(str_split(sub("ori","",
#                                                                       list.dirs(paste("./vect/set/km",km,sep=""),
#                                                                                 full.names=F,recursive=F)),","),
#                                                         function(o,ori){
#                                                           ori%in%o
#                                                         },ori)]
# 
# 
#   # oridirs <- oridirs[-1]
#   lapply(oridirs,function(oridir){
#     print(oridir)
#     dir.create(paste(oridir,"/done",sep=""),showWarnings=F)
#     files <- list.files(oridir)[list.files(oridir)!="done"]
#     unlink("./logs/clump_vector_movefromdone.txt")
#     cl <- create_cluster_clump_(detectCores(),"clump_vector_movefromdone")
#     parLapplyLB(cl,files,function(file,oridir){
#       print(file)
#       vect <- readRDS(paste(oridir,"/",file,sep=""))
#       vect <- st_as_sf(vect)
#       # print(nrow(vect))
#       # plot(vect[,1])
#       limit <- st_read("./vect/limit.gpkg",quiet=T)
#       limit$lm <- 1
#       limit <- limit[,c("lm")]
#       # tt <- st_intersection(limit,vect)
#       vect <- st_intersection(vect,limit)
#       vect$lm <- NULL
#       # print(nrow(vect))
#       # plot(vect[,1])
#       vect <- as.data.table(vect)
#       rm(limit)
#       gc()
# 
#       vect <- st_as_sf(vect)
#       multi <- vect[st_geometry_type(vect)=="MULTIPOLYGON",]
#       mono <- vect[st_geometry_type(vect)=="POLYGON",]
#       multi <- st_cast(multi,"POLYGON")
#       mono <- as.data.frame(mono)
#       multi <- as.data.frame(multi)
#       vect <- rbind(multi,mono)
#       # print(nrow(vect))
# 
#       unlink(paste(oridir,"/done/",file,sep=""))
#       saveRDS(vect,paste(oridir,"/done/",file,sep=""))
#       unlink(paste(oridir,"/",file,sep=""))
#     },oridir)
#     stopCluster(cl)
#     rm(cl)
#     gc()
#     unlink("./logs/clump_vector_movefromdone.txt")
# 
#     files <- list.files(paste(oridir,"/done",sep=""))
#     unlink("./logs/clump_vector_movefromdone.txt")
#     cl <- create_cluster_clump_(detectCores(),"clump_vector_movefromdone")
#     parLapplyLB(cl,files,function(file,oridir){
#       print(file)
#       vect <- readRDS(paste(oridir,"/done/",file,sep=""))
#       unlink(paste(oridir,"/",file,sep=""))
#       saveRDS(vect,paste(oridir,"/",file,sep=""))
#       unlink(paste(oridir,"/done/",file,sep=""))
#       # gc()
#     },oridir)
#     stopCluster(cl)
#     rm(cl)
#     gc()
#     unlink("./logs/clump_vector_movefromdone.txt")
#   })
# }

if(file.exists("./temp/clump_tomovedone.RData")){
  load("./temp/clump_tomovedone.RData")
  if(!is.null(clump_tomovedone)&&clump_tomovedone==T){
    print(paste("clump vector km:",km," ori:",ori," move from done ", Sys.time(),sep=""))
    oridirs <- list.dirs(paste("./vect/set/km",km,sep=""),
                         full.names=T,recursive=F)[sapply(str_split(sub("ori","",
                                                                        list.dirs(paste("./vect/set/km",km,sep=""),
                                                                                  full.names=F,recursive=F)),","),
                                                          function(o,ori){
                                                            ori%in%o
                                                          },ori)]
    lapply(oridirs,function(oridir){
      # print(oridir)
      unlink("./logs/clump_vector_movefromdone.txt")
      cl <- create_cluster_clump_(detectCores(),"clump_vector_movefromdone")
      parLapplyLB(cl,list.files(paste(oridir,"/done",sep="")),function(file,oridir){
        print(file)
        saveRDS(readRDS(paste(oridir,"/done/",file,sep="")),paste(oridir,"/",file,sep=""))
        unlink(paste(oridir,"/done/",file,sep=""))
      },oridir)
      stopCluster(cl)
      rm(cl)
      gc()
      unlink("./logs/clump_vector_movefromdone.txt")
    })
    clump_tomovedone <- F
    save(clump_tomovedone,file=("./temp/clump_tomovedone.RData"))
    
    if(is.null(oris_done)){
      oris_done <- ori
    }else{
      oris_done <- unique(append(oris_done,ori))
    }
    save(oris_done,file="./temp/oris_done.RData")
    rm(list=ls())
    gc()
    # stop("done moved")
    restartSession(command=source("./scripts/clump_vector_do_launch.R"))
  }
  rm(clump_tomovedone)
}

oridirs <- list.dirs(paste("./vect/set/km",km,sep=""),
            full.names=T,recursive=F)[sapply(str_split(sub("ori","",
                                                           list.dirs(paste("./vect/set/km",km,sep=""),
                                                                              full.names=F,recursive=F)),","),
                                             function(o,ori){
  ori%in%o
},ori)]

ids <- unique(unlist(sapply(oridirs,function(oridir){
  ids <- sub(".rds","",list.files(oridir,pattern="\\.rds$"))
  ids <- sub("id","",ids)
  ids <- ids[!grepl(",",ids,fixed=TRUE)]
  ids
})))
ids <- ids[order(as.numeric(ids))]


files <- setNames(unlist(sapply(oridirs,function(oridir,ids){
  list.files(oridir,full.names=T)
},ids)),unlist(sapply(oridirs,function(oridir,ids){
  sub("id","",sub(".rds","",list.files(oridir,full.names=F)))
},ids)))[setNames(unlist(sapply(oridirs,function(oridir,ids){
  ii <- str_split(sub("id","",sub(".rds","",list.files(oridir,full.names=F))),",")
  sapply(ii,function(i){any(i%in%ids)})
},ids)),NULL)]

print(paste("Clump km:",km," ori:",ori," ",length(ids)," ids ",Sys.time(),sep=""))




clump_vector_do__ <- function(id,files,rast,km,ori,parallel=T,forceddnextract=F,lastkm,lastarealimit){
  # print(id)
  tryCatch({
    filesid <- files[sapply(str_split(names(files),","),function(ii,id){id%in%ii},id)]
    vect <- lapply(filesid,function(file){
    # vect <- rbindlist(lapply(filesid,function(file){
      v <- readRDS(file)
      v$area <- NULL
      v$contained <- NULL
      v
    })
    vect <- vect[sapply(vect,nrow)>0]
    vect <- rbindlist(vect,use.names=T)
    vect
  }, error = function(e) {
    print(paste("ERROR in km: ",km,", ori: ",ori,", id: ",id,". ", conditionMessage(e)))
    stop(e)
  })

  # vect <- st_as_sf(vect)
  # nrow1 <- nrow(vect)
  # # plot(vect[,1])
  # limit <- st_read("./vect/limit.gpkg",quiet=T)
  # limit$lm <- 1
  # limit <- limit[,c("lm")]
  # # tt <- st_intersection(limit,vect)
  # vect <- st_intersection(vect,limit)
  # vect$lm <- NULL
  # nrow2 <- nrow(vect)
  # # print(nrow(vect))
  # # plot(vect[,1])
  # vect <- as.data.table(vect)
  # rm(limit)
  # gc()
  # 
  # vect <- st_as_sf(vect)
  # multi <- vect[st_geometry_type(vect)=="MULTIPOLYGON",]
  # mono <- vect[st_geometry_type(vect)=="POLYGON",]
  # multi <- st_cast(multi,"POLYGON")
  # mono <- as.data.frame(mono)
  # multi <- as.data.frame(multi)
  # vect <- rbind(multi,mono)
  # nrow3 <- nrow(vect)
  # print(paste("id: ",id," nrows: ",nrow1,"-",nrow2,"-",nrow3,sep=""))
  # rm(nrow1)
  # rm(nrow2)
  # rm(nrow3)
  # rm(mono)
  # rm(multi)
  # gc()
  
  time <- Sys.time()
  if(nrow(vect)==0){
    unlink(paste("./vect/temp/clumped/",gsub("\\.","-",paste(km,"_",ori,sep="")),"/",id,".rds",sep=""))
    filesid <- files[sapply(str_split(names(files),","),function(ii,id){id%in%ii},id)]
    lapply(filesid,function(file){
      unlink(file)
    })
    print(paste("Clump vector finished km:",km," ori:",ori," id:",id," nrow:",nrow(vect)," ",
                Sys.time(),difftime(time,Sys.time(),units="mins"),sep=""))
    
    return (T)
  }
  
  print(paste("Clump vector do launch km:",km," ori:",ori," id:",id," nrow:",nrow(vect)," ",Sys.time(),sep=""))
  dir.create(paste("./vect/temp/clumped/",paste(km,"_",ori,sep=""),sep=""),showWarnings=F)
  unlink(paste("./vect/temp/clumped/",paste(km,"_",ori,sep=""),"/",id,".rds",sep=""))

  source("./scripts/clump_vector.R")
  tryCatch({
    clump_vector(vect,rast,quad_id=id,file_id=gsub("\\.","-",paste(km,"_",ori,sep="")),
                 # method="closest",log=F,prepare=T,km=km,
                 arealimit=km*(1+log(km,3)/10)*100,method="closest",log=F,prepare=T,km=km,
                 # method="closest",log=F,prepare=T,km=km,
                 parallelextract=!parallel,forceddnextract=forceddnextract,lastkm,lastarealimit)
  }, error = function(e) {
      # error not related to timeout
    print(paste("ERROR in km: ",km,", ori: ",ori,", id: ",id,". ", conditionMessage(e)))
    # message(paste("ERROR in km: ",km,", ori: ",ori,", id: ",id,". ", conditionMessage(e)))
    stop(e)
      # stop(e)
  })

  dir.create(paste("./rasters/temp/",gsub("\\.","-",paste(km,"_",ori,sep="")),"/",sep=""),showWarnings=F)
  
  vect <- readRDS(paste("./vect/temp/clumped/",gsub("\\.","-",paste(km,"_",ori,sep="")),"/",id,".rds",sep=""))
  # st_write(st_as_sf(vect),"./test/log_10000_10000_thr.gpkg")

  vect$area <- NULL
  vect$neighbors <- NULL

  vect[,paste("quad_ori_",km,"km",sep=""):=sapply(str_split(vect[,paste("quad_ori_",km,"km",sep=""),with=F][[1]],
                                                            ","),function(ori){
                                                              paste(unique(ori)[order(as.numeric(unique(ori)))],collapse=",")
                                                            })]
  vect[,paste("quad_id_",km,"km",sep=""):=sapply(str_split(vect[,paste("quad_id_",km,"km",sep=""),with=F][[1]],
                                                           ","),function(id){
                                                             paste(unique(id)[order(as.numeric(unique(id)))],collapse=",")
                                                           })]

  unlink(paste("./vect/temp/clumped/",gsub("\\.","-",paste(km,"_",ori,sep="")),"/",id,".rds",sep=""))

  # TODO: check if file has been created with "_" at the end and the original has been deleted
  spl_oris <- split(vect,by=paste("quad_ori_",km,"km",sep=""))

  lapply(spl_oris,function(spl_ori,km){
      tryCatch({
        ori <- spl_ori[,paste("quad_ori_",km,"km",sep=""),with=F][[1]][1]
        ori <- str_split(ori,",")[[1]]
        ori <- paste(ori[order(ori)],collapse=",")
        tryCatch({
          if(!dir.exists(paste("./vect/set/km",km,"/ori",ori,sep=""))){
            dir.create(paste("./vect/set/km",km,"/ori",ori,sep=""),showWarnings=F)
          }
          if(!dir.exists(paste("./vect/set/km",km,"/ori",ori,"/done",sep=""))){
            dir.create(paste("./vect/set/km",km,"/ori",ori,"/done",sep=""),showWarnings=F)
          }
          spl_ids <- split(spl_ori,by=paste("quad_id_",km,"km",sep=""))
          # print(spl_ids)
          lapply(spl_ids,function(spl_id,km,ori){
            tryCatch({
                id <- spl_id[,paste("quad_id_",km,"km",sep=""),with=F][[1]][1]
                id <- str_split(id,",")[[1]]
                id <- paste(id[order(as.numeric(id))],collapse=",")
                # print(paste("move id to done - km:",km," ori:",ori," id:",id," nrow:",nrow(spl_id),sep=""))
                tryCatch({
                  unlink(paste("./vect/set/km",km,"/ori",ori,"/done/id",id,".rds",sep=""))
                  # dir.create(paste("./vect/set/km",km,"/ori",ori,"/id",id,sep=""),showWarnings=F)
                  saveRDS(spl_id,paste("./vect/set/km",km,"/ori",ori,"/done/id",id,".rds",sep=""))
                  unlink(paste("./vect/set/km",km,"/ori",ori,"/id",id,".rds",sep=""))
                  # saveRDS(spl_id,paste("./vect/set/km",km,"/ori",ori,"/id",id,".rds",sep=""))
                  # unlink(paste("./vect/set/km",km,"/ori",ori,"/id",id,"_.rds",sep=""))
                  # print(paste("moved id to done - km:",km," ori:",ori," id:",id,sep=""))
                },error=function(e){
                  print(paste("ERROR km:",km," ori:",ori," id:",id," ",conditionMessage(e),sep=""))
                  print(spl_id)
                  print(paste("./vect/set/km",km,"/ori",ori,"/done/id",id,".rds",sep=""))
                  # stop(paste("ERROR km:",km," ori:",ori," id:",id," ",conditionMessage(e),sep=""))
                })
            },error=function(e){
              print(paste("ERROR km:",km," ori:",ori," ",conditionMessage(e),sep=""))
              # stop(paste("ERROR km:",km," ori:",ori," ",conditionMessage(e),sep=""))
            })
          },km,ori)
        },error=function(e){
          print(paste("ERROR km:",km," ori:",ori," ",conditionMessage(e),sep=""))
          # stop(paste("ERROR km:",km," ori:",ori," ",conditionMessage(e),sep=""))
        })
      },error=function(e){
        print(paste("ERROR km:",km," ",conditionMessage(e),sep=""))
        # stop(paste("ERROR km:",km," ",conditionMessage(e),sep=""))
      })
    },km)
  # },error=function(e){
    # print(paste("ERROR id:",id,sep=""))
    ## stop(paste("ERROR id:",id," ",conditionMessage(e),sep=""))
  # })
  print(paste("Clump vector finished km:",km," ori:",ori," id:",id," nrow:",nrow(vect)," ",
              Sys.time(),difftime(time,Sys.time(),units="mins"),sep=""))
  rm(vect)
  print(1)
  # gc()
  return(T)
}

rast <- raster("./rasters/original/original.tif")
# rast <- raster("./rasters/original/original_normalised.tif")
if(length(ids)>0){
  tryCatch({
    if(length(ids)>1){
      n.cores <- detectCores()
      unlink("./logs/clump_vector_do_launch_nrows.txt")
      cl <- create_cluster_clump_(n.cores,"clump_vector_do_launch_nrows")
      nrows <- parSapplyLB(cl,ids,function(id,files){
      # nrows <- sapply(ids,function(id,files){
          print(id)
        filesid <- files[sapply(str_split(names(files),","),function(ii,id){id%in%ii},id)]
        # vect <- rbindlist(lapply(filesid,function(file){
        vect <- lapply(filesid,function(file){
            # print(file)
          v <- readRDS(file)
          v <- v[,c("id","geometry")]
          # v$contained <- NULL
          # v$DN <- NULL
          # v$area <- NULL
          # v$sd <- NULL
          v
        })
        # }),use.names=T)
        vect <- vect[sapply(vect,nrow)>0]
        vect <- rbindlist(vect,use.names=T)
        nrow(vect)
      },files)
      stopCluster(cl)
      rm(cl)
      gc()
      unlink("./logs/clump_vector_do_launch_nrows.txt")
      
      ids <- ids[order(nrows,decreasing=T)]
      ids <- split(ids,ceiling(seq_along(ids)/n.cores))
      for(ii in 1:length(ids)){
        if(ii%%2!=0){
          ids[[ii]] <- rev(ids[[ii]])
        }
      }
      ids <- lapply((1:n.cores),function(n.core,ids){sapply(ids,function(ii,n.core){
        ii[n.core]
      },n.core)},ids)
      ids <- lapply(ids,function(ii){
        ii[!is.na(ii)]
      })
      ids <- ids[sapply(ids,length)>0]
      # print(ids)
      if(length(ids)<n.cores){
        n.cores <- length(ids)
      }  
      unlink("./logs/clump_vector_do_launch.txt")
      cl <- create_cluster_clump_(n.cores,"clump_vector_do_launch")
      # res <- lapply(ids,function(ids,files,rast,km,ori,lastkm,lastarealimit){
      res <- parLapplyLB(cl,ids,function(ids,files,rast,km,ori,lastkm,lastarealimit){
        # print(id)
        if(length(ids)>1){
          # id,files,rast,km,ori,parallel=T,forceddnextract=F,lastkm,lastarealimit
          res <- lapply(ids,clump_vector_do__,files,rast,km,ori,T,forceddnextract,lastkm,lastarealimit)
        }else{
          res <- clump_vector_do__(ids[1],files,rast,km,ori,T,forceddnextract=forceddnextract,lastkm,lastarealimit)
          # res <- clump_vector_do__(ids[1][1],files,rast,km,ori,forceddnextract=forceddnextract,lastkm,lastarealimit)
        }
        print(2)
        # print(res)
        return(res)
        # clump_vector_do__(id,files,rast,km,ori)
      },files,rast,km,ori,lastkm,lastarealimit)
      # print(22)
      stopCluster(cl)
      rm(cl)
      gc()
      unlink("./logs/clump_vector_do_launch.txt")
      stop()
    }else{
      lapply(ids,function(id,files,rast,km,ori,lastkm,lastarealimit){
        clump_vector_do__(ids,files,rast,km,ori,T,forceddnextract=forceddnextract,lastkm,lastarealimit)
        # clump_vector_do__(id,files,rast,km,ori,parallel=F,forceddnextract=forceddnextract,lastkm=lastkm,lastarealimit=lastarealimit)
      },files,rast,km,ori,lastkm,lastarealimit)
    }
  },error = function(e) {
    print("error")
    stopCluster(cl)
    rm(cl)
    rm(list=ls())
    gc()
    log <- read.delim("./logs/clump_vector_do_launch.txt")
    write.table(log,paste("./logs/failed-",Sys.time(),"-clump_vector_do_launch.txt"))
    rm(log)
    unlink("./logs/clump_vector_do_launch.txt")
    stop(paste("ERROR", Sys.time()))
  })
  print(3)
  if(file.exists("./logs/clump_vector_do_launch.txt")){
    log <- read.delim("./logs/clump_vector_do_launch.txt")
    write.table(log,paste("./logs/done-km:",km,"-ori:",ori,"-clump_vector_do_launch.txt"))
    rm(log)
    unlink("./logs/clump_vector_do_launch.txt") 
    unlink(paste("./rasters/temp/",gsub("\\.","-",paste(km,"_",ori,sep="")),"/",sep=""),recursive=T)
  }
  print(4)
}else{
  print(44)
  if(file.exists("./logs/clump_vector_do_launch.txt")){
    log <- read.delim("./logs/clump_vector_do_launch.txt")
    write.table(log,paste("./logs/done-km:",km,"-ori:",ori,"-clump_vector_do_launch.txt"))
    rm(log)
    unlink("./logs/clump_vector_do_launch.txt")  
  }
  unlink(paste("./rasters/temp/",gsub("\\.","-",paste(km,"_",ori,sep="")),"/",sep=""),recursive=T)
}
print(5)
clump_tomovedone <- T
save(clump_tomovedone,file=("./temp/clump_tomovedone.RData"))

rm(list=ls())
gc()
restartSession(command=source("./scripts/clump_vector_do_launch.R"))
