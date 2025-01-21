library(parallel)
library(data.table)
simplify_file <- function(file,rast,crs,arealimit,
                          readdir="./vect/vectorised/split/ori/",
                          savedir="./vect/vectorised/split/simplified/",
                          dosmall=F,bigarea=F,parallel=T,log=T,outfile="./logs/simplify_file.tx"){
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
  cat(paste("simplify file - ",file, " - ", Sys.time()," nrow=",nrow(vect),sep=""),file=outfile,append=T,sep="\n")
  
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
  
  load("./temp/resolution.RData")
  source("./scripts/clump_vector.R")
  # corrfactornquads <- 1
  # if(sd(st_area(st_as_sf(vect)))>400){
    # corrfactornquads <- 0.5
  # }
    # nquads <- nrow(vect)/(4C*1600)
  # }else{
  nquads <- nrow(vect)/(4*1800)
  # }
  if(nquads<1){
    nquads <- 1
    
  }
  #else if(nquads<detectCores()){
  print(as.character(nquads))
  cat(as.character(nquads),file=outfile,append=T,sep="\n")
  #   nquads <- detectCores()
  # }
  # else if(nquads<4*detectCores()){
  #   nquads <- 4*detectCores()
  # }
  dir.create("./vect/clumped/temp/",showWarnings=F)
  if(nquads==1){
    if(dosmall==T){
      unlink(paste("./vect/temp/clumped/",sub(".rds","",file),file,sep=""))
      unlink(paste(savedir,file,sep=""))
      vect$quad_ori_1km <- 1
      vect$quad_id_1km <- 1
      vect <- add_colums_clump(vect)
      clump_vector_simplify(vect,rast,sub(".rds","",file),sub(".rds","",file),km=1,log=log,prepare=T,outfile=outfile)
      # clump_vector(vect,rast,sub(".rds","",file),sub(".rds","",file),arealimit,method="closest",log=log,simplify=T,parallel=parallel,km=1)
      vect <- readRDS(paste("./vect/temp/clumped/",sub(".rds","",file),"/",file,sep=""))
      # unlink(paste("./vect/clumped/temp/",sub(".rds","",file),"/",file,sep=""))
      print(paste(file, " finished. nrow:",nrow(vect),sep=""))
      vect <- vect[,c("geometry")]
      unlink(paste(savedir,file,sep=""))
      saveRDS(vect,paste(savedir,file,sep=""))
      unlink(paste("./vect/temp/clumped/",sub(".rds","",file),"/",sep=""),recursive = T)
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
    print(paste("ikm:",ikm))
    cat(paste("ikm:",ikm),file=outfile,append=T,sep="\n")
    if(max(aggregate(vect[[which(colnames(vect)==paste("quad_id_",ikm,"km",sep=""))]],
                     list(vect[[which(colnames(vect)==paste("quad_id_",ikm,"km",sep=""))]]),
                     length)$x)>(4*2100)){
      nquads <- nquads*(max(aggregate(vect[[which(colnames(vect)==paste("quad_id_",ikm,"km",sep=""))]],
                                      list(vect[[which(colnames(vect)==paste("quad_id_",ikm,"km",sep=""))]]),
                                      length)$x)/(4*1800))
      
      if((vect_width/(2000*resolution))/ceiling(sqrt(nquads))!=ikm){
        ikm <- (vect_width/(2000*resolution))/ceiling(sqrt(nquads))
        vect <- set_quad_ori_kms(vect,ikm,limit,log=F)  
      }
      print(paste("ikm:",ikm))
      cat(paste("ikm:",ikm),file=outfile,append=T,sep="\n")
      
      if(max(aggregate(vect[[which(colnames(vect)==paste("quad_id_",ikm,"km",sep=""))]],
                       list(vect[[which(colnames(vect)==paste("quad_id_",ikm,"km",sep=""))]]),
                       length)$x)>(4*2100)){
        nquads <- nquads*(max(aggregate(vect[[which(colnames(vect)==paste("quad_id_",ikm,"km",sep=""))]],
                                        list(vect[[which(colnames(vect)==paste("quad_id_",ikm,"km",sep=""))]]),
                                        length)$x)/(4*1800))
        
        if((vect_width/(2000*resolution))/ceiling(sqrt(nquads))!=ikm){
          ikm <- (vect_width/(2000*resolution))/ceiling(sqrt(nquads))
          vect <- set_quad_ori_kms(vect,ikm,limit,log=F)  
        }
        print(paste("ikm:",ikm))
        cat(paste("ikm:",ikm),file=outfile,append=T,sep="\n")
      }
    }
    rm(nquads)
    vect <- add_colums_clump(vect)
    vect <- clump_vector_do_small_files.launch_km_specifyarea(vect,ikm,rast,arealimit,file_id=sub(".rds","",file),method="closest",bigarea=bigarea,simplify=T,parallel=parallel,log=log,outfile=outfile)
    print(paste(file, " finished. nrow:",nrow(vect),sep=""))
    cat(paste(file, " finished. nrow:",nrow(vect),sep=""),file=outfile)
    vect <- vect[,c("geometry")]
    unlink(paste(savedir,file,sep=""))
    saveRDS(vect,paste(savedir,file,sep=""))  
    unlink(paste(readdir,file,sep=""))
    rm(vect)
    rm(ikm)
    gc()
  }
}
