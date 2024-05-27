# library(stringr)
# library(raster)
library(sf)
# files_list <- list.files("./classificador_vol_america/vect/vectorised/split/simplified/", pattern="\\.rds$")
# names(files_list) <- as.numeric(sapply(files_list,function(f){sub(".rds","",f)}))
# dir.create("./classificador_vol_america/vect/vectorised/split/merged",showWarnings=F)

files <- list.files("./classificador_vol_america/vect/vectorised/split/simplified/",
                    pattern="*.rds")
# dir.create("./classificador_vol_america/vect/vectorised/split/simplified/csv/")
library(sfheaders)
lapply(files[1:10],function(f){
  print(paste("Merging", f))
  sf <- st_as_sf(readRDS(paste("./classificador_vol_america/vect/vectorised/split/simplified/",f,sep="")))
  df <- sf_to_df(sf,fill=T)
  write.table(df,"./classificador_vol_america/vect/vectorised/merged.csv",append=T)
  # unlink(paste("./classificador_vol_america/vect/vectorised/split/simplified/",f,".rds",sep=""))
  })



tt <- read.csv("./classificador_vol_america/vect/vectorised/merged.csv")

ttt <- sf_polygon(obj=tt,polygon_id="polygon_id",linestring_id="linestring_id",x="x",y="y",keep=T)

#create grid with n blocks
if(file.exists("./classificador_vol_america/temp/vectorise_raster_merge_grid.RData")){
  load(file="./classificador_vol_america/temp/vectorise_raster_merge_grid.RData")
}else{
  source("./classificador_vol_america/scripts/clump_vector.R")
  load(file="./classificador_vol_america/temp/resolution.RData")
  library(sf)
  limit <- readRDS("./classificador_vol_america/vect/limit.rds")
  area <- as.numeric(st_area(limit))
  vectorise_raster_grid <- create_grid(limit,round(sqrt(area)/(resolution*2000)),round(sqrt(area)/(resolution*2000)))
  vectorise_raster_grid <- data.table(vectorise_raster_grid)
  free.mem <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024
  n <- ceiling(1.5*sqrt(sqrt(free.mem)))
  firstkm <- 1
  vectorise_raster_grid[,paste("quad_row_",firstkm,"km",sep=""):=vectorise_raster_grid$row]
  vectorise_raster_grid[,paste("quad_col_",firstkm,"km",sep=""):=vectorise_raster_grid$col]
  vectorise_raster_grid[,paste("quad_id_",firstkm,"km",sep=""):=vectorise_raster_grid$id]
  vectorise_raster_grid[,paste("quad_ori_",firstkm,"km",sep=""):=vectorise_raster_grid$ori]
  vectorise_raster_grid <- set_quad_ori_km_notfirst(vectorise_raster_grid,n,firstkm)
  vectorise_raster_merge_grid <- vectorise_raster_grid
  rm(vectorise_raster_grid)
  save(vectorise_raster_merge_grid,file="./classificador_vol_america/temp/vectorise_raster_merge_grid.RData")
  rm(vectorise_raster_merge_grid)
  rm(limit)
  rm(area)
  rm(firstkm)
  rm(free.mem)
  restartSession(command=source("./classificador_vol_america/scripts/vectorise_rasters_merge.R"))
}

#merge files in n blocks
if(length(files_list)>0){
  n <- max(as.numeric(sub("km","",sub("quad_id_","",colnames(vectorise_raster_merge_grid)[startsWith(colnames(vectorise_raster_merge_grid),"quad_id_")]))))
  files_list_split <- split(files_list, sapply(names(files_list),function(nm,n,vectorise_raster_merge_grid){
    vectorise_raster_merge_grid[quad_id_1km==nm][[which(colnames(vectorise_raster_merge_grid)==paste("quad_id_",n,"km",sep=""))]]
  },n,vectorise_raster_merge_grid))
  rm(files_done)
  rm(files_list)
  rm(n)
  if(length(files_list_split)>0){
    # unlink("./classificador_vol_america/logs/vectorise_rasters_merge.txt")
    source("./classificador_vol_america/scripts/clump_vector.R")
    lapply(names(files_list_split),function(name,files_list_split){
      print(name)
      files <- files_list_split[[name]]
      merged <- rbindlist(lapply(files,function(f){
        readRDS(paste("./classificador_vol_america/vect/vectorised/split/simplified/",
                      f,sep=""))
      }),use.names=T)
      unlink(paste("./classificador_vol_america/vect/vectorised/split/merged/",
                           # sub(".rds","",files[1]),"-",sub(".rds","",files[length(files)]),
                           name,
                           ".rds",
                           sep=""))
      saveRDS(merged,paste("./classificador_vol_america/vect/vectorised/split/merged/",
                           # sub(".rds","",files[1]),"-",sub(".rds","",files[length(files)]),
                           name,
                           ".rds",
                           sep=""))
      lapply(files,function(f){
        unlink(paste("./classificador_vol_america/vect/vectorised/split/simplified/",f,sep=""))
      })
      rm(merged)
      return(T)
    },files_list_split)
    rm(files_list_split)
    # unlink("./classificador_vol_america/logs/vectorise_rasters_merge.txt")
    restartSession(command=source("./classificador_vol_america/scripts/vectorise_rasters_merge.R"))
  }
  rm(files_list_split)
}
rm(vectorise_raster_merge_grid)

#simplify merged files
files_list <- list.files("./classificador_vol_america/vect/vectorised/split/merged",pattern="\\.rds$")
names(files_list) <- as.numeric(sapply(files_list,function(f){sub(".rds","",f)}))
if(length(files_list)>0){
  dir.create("./classificador_vol_america/vect/vectorised/split/merged/simplified",showWarnings=F)
  rast <- raster("./classificador_vol_america/rasters/original/original.tif")
  crs <- st_crs(readRDS("./classificador_vol_america/vect/limit.rds"))
  load("./classificador_vol_america/temp/resolution.RData")
  source("./classificador_vol_america/scripts/vectorise_raster_simplify_function.R")
  lapply(files_list,function(file,rast,crs,resolution){
    simplify_file(file,rast,crs,resolution*9,
                  readdir="./classificador_vol_america/vect/vectorised/split/merged/",
                  savedir="./classificador_vol_america/vect/vectorised/split/merged/simplified/",
                  dosmall=T,minpercent=10,bigarea=T)
    unlink(paste("./classificador_vol_america/vect/vectorised/split/merged/",file,sep=""))
  },rast,crs,resolution)
}





# library(sqldf)
# data1 <- file("./classificador_vol_america/vect/vectorised/split/merged/229.rds")
# data2 <- file("./classificador_vol_america/vect/vectorised/split/merged/228.rds")
# merged <- sqldf("select * from data1 union select * from data2", dbname = tempfile())

#final file merge
files_list <- list.files("./classificador_vol_america/vect/vectorised/split/merged/simplified/",pattern="\\.rds$")
if(length(files_list)>0){
  unlink("./classificador_vol_america/vect/vectorised/merged.gpkg")
  merged <- rbindlist(lapply(files_list,function(f){
    readRDS(paste("./classificador_vol_america/vect/vectorised/split/merged/simplified/",f,sep=""))
  }))
  st_write(merged,"./classificador_vol_america/vect/vectorised/merged.gpkg")
  sapply(files_list,function(f){
    unlink(paste("./classificador_vol_america/vect/vectorised/split/merged/simplified/",f,sep=""))
  })
}








# 
# files_list <- list.files("./classificador_vol_america/vect/vectorised/split/merged")
# names(files_list) <- unlist(lapply(str_split(files_list,"-"),function(f){
#   as.numeric(gsub("_","",sub(".rds","",f)))[1]
# }))
# #TODO: si hi ha arxius que se solapen, eliminar el gran i deixar els altres
# #(vol dir que ha fallat mentre es guardava)
# files_list <- files_list[order(as.numeric(names(files_list)),decreasing=F)]
# 
# free.mem <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1024
# n <- ceiling(free.mem/6000)
# if(n<2){
#   n <- 2
# }
# # files_list_split <-split(files_list[!startsWith(names(files_list),".")],
# #                          ceiling(seq_along(files_list[!startsWith(files_list,"_"))])/(ceiling(free.mem/5000))))
# files_list_split <-split(files_list[!startsWith(files_list,"_")],
#                          ceiling(seq_along(files_list[!startsWith(files_list,"_")])/(n)))
# ii <- 1
# while(length(files_list)>1){
#   if(length(files_list_split)>0){
#     
#    lapply(files_list_split,function(files,ii){
#           if(is.null(files)){
#             return(F)
#           }else if(length(files)==1){
#             file.rename(from=paste("./classificador_vol_america/vect/vectorised/split/merged/",files[1],sep=""),
#                         to=paste("./classificador_vol_america/vect/vectorised/split/merged/_",files[1],sep=""))
#             return(T)
#           }else if(length(files)==0){
#             return(T)
#           }
#           newname <- paste(str_split(gsub("_","",sub(".rds","",files[1])),"-")[[1]][1],
#                            "-",
#                            str_split(gsub("_","",sub(".rds","",files[length(files)])),"-")[[1]][2],
#                            ".rds",sep="")
#           print(newname)
#           merged <- readRDS(paste("./classificador_vol_america/vect/vectorised/split/merged/",files[1],sep=""))
#           for(i in 2:length(files)){
#             # print(files[i])
#             merged <- rbind(merged,readRDS(paste("./classificador_vol_america/vect/vectorised/split/merged/",
#                                                  files[i],sep="")))
#           }
#           saveRDS(merged,paste("./classificador_vol_america/vect/vectorised/split/merged/",
#                                paste(rep("_",ii),collapse=""),
#                                newname,
#                                sep=""))
#           lapply(files,function(f){
#             unlink(paste("./classificador_vol_america/vect/vectorised/split/merged/",f,sep=""))
#           })
#           rm(merged)
#           gc()
#           # restartSession(command=source("./classificador_vol_america/scripts/vectorise_rasters_merge.R"))
#           return(T)
#         },ii)
#     restartSession(command=source("./classificador_vol_america/scripts/vectorise_rasters_merge.R"))
#   }
#   ii <- ii+1
#   files_list <- list.files("./classificador_vol_america/vect/vectorised/split/merged")
#   names(files_list) <- unlist(lapply(str_split(files_list,"-"),function(f){
#     as.numeric(gsub("_","",sub(".rds","",f)))[1]
#   }))
#   files_list <- files_list[order(as.numeric(names(files_list)),decreasing=F)]
#   n <- ceiling(free.mem/12000)/(ii-1)
#   if(n<2){
#     n <- 2
#   }
#   # files_list_split <-split(files_list, ceiling(seq_along(files_list)/(n)))
#   files_list_split <- split(files_list[!startsWith(files_list,paste(rep("_",ii),collapse=""))],
#                             ceiling(seq_along(files_list)/(n)))
#   files_list_split <- lapply(files_list_split,function(f){
#     if(length(f)==0){
#       return(NULL)
#     }else{
#       return(f)
#     }
#   })
#   files_list_split <- files_list_split[sapply(files_list_split,function(f)!is.null(f))]
#   if(length(files_list_split)>1){
#     if(length(files_list_split[[length(files_list_split)]])==1){
#     files_list_split[[length(files_list_split)-1]] <- append(files_list_split[[length(files_list_split)-1]],
#                                                            files_list_split[[length(files_list_split)]])
#     files_list_split[length(files_list_split)] <- NULL
#     }
#   }
#   if(length(files_list_split)>1){
#     files_list_split <- files_list_split[sapply(files_list_split,function(f)!is.null(f))]
#   }
# }
# 
# 
# 
# files_list <- split(files_list, cut(seq_along(files_list),detectCores(),labels=F))
# cl <- create_cluster_clump_(detectCores(),"vectorise_rasters_merge")
# merged <- parLapplyLB(cl,files_list,function(files){
#   merged <- readRDS(paste("./classificador_vol_america/vect/vectorised/split/simplified/",
#                           files[1],sep=""))
#   for(i in 2:length(files)){
#     print(files[i])
#     # file <- st_read(paste("./classificador_vol_america/vect/vectorised/split/simplified/",
#     #                       files_list[i],sep=""))
#     file <- readRDS(paste("./classificador_vol_america/vect/vectorised/split/simplified/",
#                           files[i],sep=""))
#     merged <- rbind(merged,file)
#     rm(file)
#     # gc()
#   }
#   merged
# })
# stopCluster(cl)
# for(i in 2:length(files_list)){
#   print(files_list[i])
#   # file <- st_read(paste("./classificador_vol_america/vect/vectorised/split/simplified/",
#   #                       files_list[i],sep=""))
#   file <- readRDS(paste("./classificador_vol_america/vect/vectorised/split/simplified/",
#                         files_list[i],sep=""))
#   merged <- rbind(merged,file)
#   rm(file)
#   # gc()
# }
# # vects_list <- lapply(paste("./classificador_vol_america/vect/vectorised/split/simplified/",
# #                            list.files("./classificador_vol_america/vect/vectorised/split/simplified/", pattern="\\.gpkg$"),
# #                            sep=""),st_read)
# # vect <- do.call(rbind,vects_list)
# rm(vects_list)
# st_write(vect,"./classificador_vol_america/vect/vectorised/merged.gpkg")
# rm(vect)
# #remove all files in "./classificador_vol_america/vect/vectorised/split/"
# unlink("./classificador_vol_america/vect/vectorised/split/",recursive=T)
# gc()

vectorise_raster_current <- "merged"
save(vectorise_raster_current,file="./classificador_vol_america/temp/vectorise_raster_current.RData")
unlink("./classificador_vol_america/temp/vectorise_raster_merge_grid.RData")
vectorise_raster_ini()