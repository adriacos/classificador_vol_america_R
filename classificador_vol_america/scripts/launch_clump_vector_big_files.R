source("./classificador_vol_america/scripts/clump_vector.R")
print("launch_clump_vector_big_files")

load("./classificador_vol_america/temp/current_km.RData")
load("./classificador_vol_america/temp/current_ori.RData")
load("./classificador_vol_america/temp/clump_vector_bigarea.RData")

unlink("./classificador_vol_america/vect/clumped/temp/")

# load("./classificador_vol_america/temp/vects_ori_list.RData")

if(length(vects_quad_ori_list)==0){
  vects_ori <- st_read("./classificador_vol_america/vect/clumped/global_clmp_temp.rds",
                       query = paste("SELECT * FROM \"global_clmp_temp_temp\"
                             WHERE '"+current_ori+"' IN (SELECT value FROM STRING_SPLIT(quad_ori_',"+current_km+",'km, ','))"))
  ids_ori <- unique(as.data.frame(vects_ori)[,paste("quad_id_",current_km,"km",sep="")])[sapply(str_split(unique(as.data.frame(vects_ori)[,paste("quad_id_",current_km,"km",sep="")]),","),length)==1]
  vects_ori[,paste("quad_id_",current_km,"_km_ori_",current_ori,sep="")] <- unlist(sapply(str_split(as.data.frame(vects_ori)[,paste("quad_id_",current_km,"km",sep="")],","),function(x,ids_ori){
    x[x%in%ids_ori][1]
  },ids_ori))
  
  vects_quad_ori_list <- split(vects_ori, as.data.frame(vects_ori)[,paste("quad_id_",current_km,"_km_ori_",current_ori,sep="")])
  rm(vects_ori)
  rm(ids_ori)
  rm(vects_quad_ori_list)
  save(vects_quad_ori_list,file="./classificador_vol_america/temp/vects_quad_ori_list.RData")
  gc()
  restartSession(command=source("./classificador_vol_america/scripts/launch_clump_vector_big_files.R"))
}else{
  ids_done <- sub(".rds","",list.files("./classificador_vol_america/vect/clumped/temp/"))
  vects_quad_ori_list <- vects_quad_ori_list[!names(vects_quad_ori_list)%in%ids_done]
  rm(ids_done)
}

      vects_quad_ori_list <- vects_quad_ori_list[order(sapply(vects_quad_ori_list,nrow),decreasing=T)]
      
      vv <- split(vects_quad_ori_list, ceiling(seq_along(vects_quad_ori_list)/detectCores()))
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
      print(paste("length ",sum(sapply(vects_quad_ori_list,length)),sep=""))
      
      if(clump_vector_bigarea==T){
        lapply(vects_quad_ori_list, function(vv,rast,quad_id_col,file_id){
          lapply(vv,function(v,rast,quad_id_col,file_id){
            if(is.null(v)){
              return()
            }
            quad_id <- v[[which(colnames(v)==quad_id_col)]][1]
            crop(rast,
                 st_transform(st_as_sf(v),st_crs(rast)),
                 filename=paste("./classificador_vol_america/rasters/temp/",file_id,"/","/clump_v_r",
                                        file_id,"_",quad_id,sep=""),
                 overwrite=T)
            return()
          },rast,quad_id_col,file_id)
        },rast,paste("quad_id_",ikm,"_km_ori_",ori,sep=""),file_id)
      }
      

      rast <- raster("./classificador_vol_america/rasters/original/original.tif")
      n.cores <- get_core_number_(vects_quad_ori_list)
tryCatch(
  {
    cl <- create_cluster_clump_(n.cores)
    parLapplyLB(cl, vects_quad_ori_list, function(vv,rast,quad_id_col,arealimit){
      lapply(vv,function(v,rast,quad_id_col,arealimit){
        if(is.null(v)){
          return()
        }
        quad_id <- as.data.frame(v)[,quad_id_col][1]
        clump_vector(v,rast,quad_id,arealimit)
      },rast,quad_id_col,arealimit)
    },rast,paste("quad_id_",current_km,"_km_ori_",current_ori,sep=""),100*current_km)
    # 
    # parLapplyLB(cl, vects_quad_ori_list, function(v,rast,quad_id_col,arealimit){
    #   if(is.null(v)){
    #     return()
    #   }
    #   quad_id <- as.data.frame(v)[,quad_id_col][1]
    #   clump_vector(v,rast,quad_id,arealimit)
    # },rast,paste("quad_id_",current_km,"_km_ori_",current_ori,sep=""),100*current_km)
    # stopCluster(cl)
    # parLapplyLB(cl, vects_quad_ori_list, clump_vector,
    #                            rast,
    #                            paste("quad_id_",current_km,"km",sep=""),
    #                            150*current_km)
    stopCluster(cl)
    rm(cl)
  },
  error=function() {
    print("error")
    stopCluster(cl)
    rm(cl)
    rm(vects_ori_list)
    gc()
    restartSession(command=source("./classificador_vol_america/scripts/launch_clump_vector_big_files.R"))
  }
)
vects_ori <- do.call(rbind,lapply(paste("./classificador_vol_america/vect/clumped/temp/",
                                        list.files("./classificador_vol_america/vect/clumped/temp/"),
                                        sep=""),readRDS))
unlink("./classificador_vol_america/vect/clumped/temp",recursive=T)
vects_ori$sd <- NULL
vects_ori$DN <- NULL
vects_ori$area <- NULL
vects_ori[,paste("quad_id_",current_km,"_km_ori_",current_ori,sep="")] <- NULL
# dir.create("./classificador_vol_america/vect/clumped/kms",showWarnings=F)
# saveRDS(vects, paste("./classificador_vol_america/vect/clumped/kms/global_clmp_",current_km,"km_ori",current_ori,".rds", sep=""))
# saveRDS(vects,"./classificador_vol_america/vect/clumped/global_clmp_temp.rds",overwrite=T)
vects_rest <- st_read("./classificador_vol_america/vect/clumped/global_clmp_temp.gpkg",
                     query = paste("SELECT * FROM \"global_clmp_temp_temp\"
                             WHERE '"+current_ori+"' NOT IN (SELECT value FROM STRING_SPLIT(quad_ori_',"+current_km+",'km, ','))"))

vects <- rbind(vects_ori,vects_rest)
rm(vects_ori)
rm(vects_rest)
saveRDS(vects,"./classificador_vol_america/vect/clumped/global_clmp_temp.rds",overwrite=T)

oris_done <- append(oris_done,ori)
save(oris_done, file="oris_done.RData")
vects_quad_ori_list <- list()
save(vects_ori_list,file="./classificador_vol_america/temp/vects_quad_ori_list.RData")
rm(vects_quad_ori_list)
rm(vects_ori)
restartSession(command=source("./classificador_vol_america/scripts/clump_vector_combine.do_big.files_launch_km.R"))