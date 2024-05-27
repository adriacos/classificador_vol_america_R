library(parallel)
library(data.table)
simplify_file <- function(file,rast,crs,arealimit,
                          readdir="./classificador_vol_america/vect/vectorised/split/ori/",
                          savedir="./classificador_vol_america/vect/vectorised/split/simplified/",
                          dosmall=F,bigarea=F,parallel=T,log=T){
  vect <- readRDS(paste(readdir,file,sep=""))
  if(nrow(vect)==0){
    unlink(paste(readdir,file,sep=""))
    return()
  }
  # if(class(vect)[1]=="sf"){
  #   vect <- st_transform(vect,crs)
  #   # vect <- as.data.table(vect)
  # }
  
  print(paste("simplify file - ",file, " - ", Sys.time()," nrow=",nrow(vect),sep=""))
  
  # if(minpercent>0){
  #   if(class(vect)[1]!="sf"){
  #     vect <- st_as_sf(vect)
  #   }
  #   area <- st_area(vect)
  #   print(100*length(area[as.numeric(area)<=arealimit])/nrow(vect))
  #   if(100*length(area[as.numeric(area)<=arealimit])/nrow(vect)<minpercent){
  #     unlink(paste(savedir,file,sep=""))
  #     saveRDS(vect,paste(savedir,file,sep=""))
  #     unlink(paste(readdir,file,sep=""))
  #     return(NULL)
  #   }
  #   vect <- as.data.table(vect)
  #   rm(area)
  # }
  # 
  # print(nrow(vect))
  if(class(vect)[1]=="sf"){
    # vect <- st_transform(vect,crs)
    vect <- as.data.table(vect)
  }
  
  if(!"layer"%in%colnames(vect)){
    if(paste("X",sub(".rds","",file),sep="")%in%colnames(vect)){
      vect$layer <- as.data.frame(vect)[,paste("X",sub(".rds","",file),sep="")]
      vect[,paste("X",sub(".rds","",file),sep="")] <- NULL
    }
  }
  
  load("./classificador_vol_america/temp/resolution.RData")
  source("./classificador_vol_america/scripts/clump_vector.R")
  nquads <- nrow(vect)/(4*1800)
  if(nquads<1){
    nquads <- 1
  }#else if(nquads<detectCores()){
  #   nquads <- detectCores()
  # }
  # else if(nquads<4*detectCores()){
  #   nquads <- 4*detectCores()
  # }
  dir.create("./classificador_vol_america/vect/clumped/temp/",showWarnings=F)
  if(nquads==1){
    if(dosmall==T){
      unlink(paste("./classificador_vol_america/vect/temp/clumped/",sub(".rds","",file),file,sep=""))
      unlink(paste(savedir,file,sep=""))
      vect$quad_ori_1km <- 1
      vect$quad_id_1km <- 1
      vect <- add_colums_clump(vect)
      clump_vector_simplify(vect,rast,sub(".rds","",file),sub(".rds","",file),km=1,log=log,prepare=T)
      # clump_vector(vect,rast,sub(".rds","",file),sub(".rds","",file),arealimit,method="closest",log=log,simplify=T,parallel=parallel,km=1)
      vect <- readRDS(paste("./classificador_vol_america/vect/temp/clumped/",sub(".rds","",file),"/",file,sep=""))
      # unlink(paste("./classificador_vol_america/vect/clumped/temp/",sub(".rds","",file),"/",file,sep=""))
      print(paste(file, " finished. nrow:",nrow(vect),sep=""))
      vect <- vect[,c("geometry")]
      unlink(paste(savedir,file,sep=""))
      saveRDS(vect,paste(savedir,file,sep=""))
      unlink(paste("./classificador_vol_america/vect/temp/clumped/",sub(".rds","",file),"/",sep=""),recursive = T)
      unlink(paste(readdir,file,sep=""))
    }else if(dosmall==F){
      dir.create(paste(readdir,"/small",sep=""),showWarnings=F)
      unlink(paste(readdir,"/small/",file,sep=""))
      saveRDS(vect,paste(readdir,"/small/",file,sep=""))
      unlink(paste(readdir,file,sep=""))
    }
  }else if(round(nquads)<=4&dosmall==F){
    dir.create(paste(readdir,"/small",sep=""),showWarnings=F)
    unlink(paste(readdir,"/small/",file,sep=""))
    saveRDS(vect,paste(readdir,"/small/",file,sep=""))
    unlink(paste(readdir,file,sep=""))
  }else{
    unlink(paste(savedir,file,sep=""))
    vect_width <- as.numeric(round(sqrt(st_area(st_as_sfc(st_bbox(vect$geometry))))))
    ikm <- (vect_width/(2000*resolution))/ceiling(sqrt(nquads))
    limit <- st_as_sfc(st_bbox(vect$geometry))
    vect <- set_quad_ori_kms(vect,ikm,limit,log=F)
    if(max(aggregate(vect[[which(colnames(vect)==paste("quad_id_",ikm,"km",sep=""))]],
                     list(vect[[which(colnames(vect)==paste("quad_id_",ikm,"km",sep=""))]]),
                     length)$x)>(4*1800)){
      nquads <- nquads*(max(aggregate(vect[[which(colnames(vect)==paste("quad_id_",ikm,"km",sep=""))]],
                                      list(vect[[which(colnames(vect)==paste("quad_id_",ikm,"km",sep=""))]]),
                                      length)$x)/(4*1800))
      
      if((vect_width/(2000*resolution))/ceiling(sqrt(nquads))!=ikm){
        ikm <- (vect_width/(2000*resolution))/ceiling(sqrt(nquads))
        vect <- set_quad_ori_kms(vect,ikm,limit,log=F)  
      }
    }
    rm(nquads)
    vect <- add_colums_clump(vect)
    vect <- clump_vector_do_small_files.launch_km_specifyarea(vect,ikm,rast,arealimit,file_id=sub(".rds","",file),method="closest",bigarea=bigarea,simplify=T,parallel=parallel,log=log)
    print(paste(file, " finished. nrow:",nrow(vect),sep=""))
    vect <- vect[,c("geometry")]
    unlink(paste(savedir,file,sep=""))
    saveRDS(vect,paste(savedir,file,sep=""))  
    unlink(paste(readdir,file,sep=""))
  }
}
