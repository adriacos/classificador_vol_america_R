
source("./classificador_vol_america/scripts/project.R")
source("./classificador_vol_america/scripts/read_data.R")


# library(sf)
# library(tidyverse)
library(terra)

restore <- function(){
  rasters <- read_rasters_all()
  merged <- rasters[grepl("_", names(rasters))]
  rasters <- rasters[names(rasters)!=names(merged)]          
  ids_done <- unlist(str_split(names(merged),"_"))  
  rasters <- rasters[!names(rasters)%in%ids_done]
  
}

read_rasters_all <- function(){
  # dir <- "./classificador_vol_america/rasters/all"
  dir <- "./classificador_vol_america/rasters/pnoa"
  files <- paste(dir, list.files(dir, pattern = "\\.tif$"), sep="/")
  ids <- sapply(files,function(file){
    file <- sub(dir,"",file)
    file <- sub("/","",file)
    # file <- sub("","",file)
    file <- sub("\\.tif$","",file)
    # as.numeric(file)
    file
  })
  files <- files[order(ids)]
  ids <- sapply(files,function(file){
    file <- sub(dir,"",file)
    file <- sub("/","",file)
    # file <- sub("","",file)
    file <- sub("\\.tif$","",file)
    # as.numeric(file)
    file
  })
  rasters <- lapply(files, raster)
  names(rasters) <- ids
  names(ids) <- NULL
  rasters
  # rasters <- rasters[names(rasters)%in%1:3000]
} 

merge_rasters_1by1 <- function(rasters){
  merged <- rasters[[1]]
  ids <- names(rasters[1])
  # rasts <- rasters[-1]
  for(i in 1:length(rasters)){
    print(i)
    r2 <- rasters[[i]]
    ids <- append(ids, names(rasters[i]))
    newname <- paste(paste(ids, collapse="_"),".tif",sep="")
    print(newname)
    print(oldname)
    # if(i==2){
    #   oldname <- newname
    # }
    template <- projectRaster(from = r2, to= merged, alignOnly=TRUE)
    print("template")
    aligned <- projectRaster(from=r2, to=template)
    alignedlocation <- aligned@file@name
    print("aligned")
    rm(template)
    gc()
    oldlocation <- merged@file@name
    merged <- merge(merged, aligned, paste("./classificador_vol_america/rasters/pnoa/",newname,sep=""))
    newlocation <- merged@file@name
    if(oldlocation!=newlocation){
      unlink(oldlocation)
      unlink(sub(".grd",".gri",oldlocation))
      print("deleted old temp file")
    }
    print("merged")
    unlink(alignedlocation)
    unlink(sub(".grd",".gri",alignedlocation))
    rm(aligned)
   
    gc()
    if(i%%2==0){
      writeRaster(merged, paste("./classificador_vol_america/rasters/pnoa/",newname,sep=""),overwrite=T)
      print("saved")
      # rm(merged)
      unlink(paste("./classificador_vol_america/rasters/pnoa/",oldname,sep=""))
      print("deleted")
      oldname <- newname
      gc()
    }
   

  }
}


merge_rasters <- function(rasters){
  r1 <- rasters[[1]]
  rasts <- rasters[-1]
  templates <- lapply(rasts, function(x) projectRaster(from = x, to= r1, alignOnly=TRUE))
  aligned <- mapply(function(x, y) projectRaster(from=x, to=y), rasts, templates)
  rasts <- append(r1,aligned)
  names(rasts) <- names(rasters)
  
  rm(templates)
  rm(aligned)
  rm(r1)
  gc()
  
  
  while(length(rasts)>3){
    rasts <- split(rasts, cut(seq_along(rasts), round(length(rasts)/3), labels = FALSE))
    rasts <- lapply(rasts, function(rr){
      merged <- rr[[1]]
      r2 <- rr[-1]
      for(i in 1:length(r2)){
        print(paste(i, names(rr[i])))
        merged <- merge(merged, r2[[i]])
      }
      merged
    })
  }
  merged <- rasts[[1]]
  r2 <- rasts[-1]
  for(i in 1:length(r2)){
    print(paste(i, names(rasts[i])))
    merged <- merge(merged, r2[[i]])
  }
  rm(rasts)
  gc()
  #
  # #dividir-ho en varies parts (tantes com cores) i fer cadascuna en paral·lel i després ajuntar-ho
  # for(i in 1:length(rasts)){
  #   print(i)
  #   merged <- merge(merged, aligned[[i]])
  # }
  writeRaster(merged, "./classificador_vol_america/rasters/global_1_300.tif")
  merged
}


merge_clumped <- function(){
  merge_vectors()
}

merge_vectors <- function(){
  if (!file.exists("./classificador_vol_america/vect/global/global_corrected.shp")){
    print("no exists")
    merge_vectors_1()
  }else{
    print("exists")
    merge_vectors_2()
  }
}

merge_vectors_1 <- function(){
  vector_list <- get_clumped_vectors()
  for(i in 1:length(vector_list)){
    vector_list[[i]]$ori <- names(vector_list)[i]
    vector_list[[i]]$n_quad <- vector_list[[i]]$ori
  }
  
  vector_list <- lapply(vector_list, reproject_EPSG_25831_vect)
  
  for(i in 1:length(vector_list)){
    print(i)
    v1 <- vector_list[[i]]
    for(ii in 1:length(vector_list)){
      if(i==ii){next()}
      v2 <- vector_list[[ii]]
      if((extent(v1)@ymin < extent(v2)@ymax && extent(v1)@ymax > extent(v2)@ymax && 
          (round(extent(v1)@xmin,2) == round(extent(v2)@xmin,2) || 
           round(extent(v1)@xmax,2) == round(extent(v2)@xmin, 2) || 
           round(extent(v1)@xmin,2) == round(extent(v2)@xmax, 2))) ||
         (extent(v1)@xmin < extent(v2)@xmax && extent(v1)@xmax > extent(v2)@xmax && 
          (round(extent(v1)@ymin,2) == round(extent(v2)@ymin,2) || 
           round(extent(v1)@ymax,2) == round(extent(v2)@ymin, 2) || 
           round(extent(v1)@ymin,2) == round(extent(v2)@ymax, 2))) ||
         (extent(v1)@xmax > extent(v2)@xmin && extent(v1)@xmin < extent(v2)@xmin &&
          extent(v1)@ymin < extent(v2)@ymax && extent(v1)@ymax > extent(v2)@ymin) 
         ||
         (extent(v1)@xmax > extent(v2)@xmin && extent(v1)@xmin < extent(v2)@xmin &&
          extent(v1)@ymax > extent(v2)@ymin && extent(v1)@ymin < extent(v2)@ymin)
         ||
         (extent(v1)@xmin < extent(v2)@xmax && extent(v1)@xmax > extent(v2)@xmax &&
          extent(v1)@ymax > extent(v2)@ymin && extent(v1)@ymin < extent(v2)@ymin)
         ||
         (extent(v1)@xmin < extent(v2)@xmax && extent(v1)@xmax > extent(v2)@xmax &&
          extent(v1)@ymin < extent(v2)@ymax && extent(v1)@ymax > extent(v2)@ymin)
         ){
      ##if(gIntersects(v1, v2, byid = F)==T){
        #vector_list[[i]] <- v1-v2
        vector_list[[i]]$n_quad <- paste(vector_list[[i]]$n_quad[1], v2$ori[1], sep=",")
        vector_list[[ii]]$n_quad <- paste(vector_list[[ii]]$n_quad[1], v1$ori[1], sep=",")
        #v1 <- vector_list[[i]]  
      }
    }
  }
  
  rm(v1)
  rm(v2)
  rm(i)
  rm(ii)
  gc()
  
  for(i in 1:length(vector_list)){
    if(!"DN" %in% names(vector_list[[i]])){
      vector_list[[i]]$DN <- NA
    }
    if(!"sd" %in% names(vector_list[[i]])){
      vector_list[[i]]$sd <- NA
    }
    if(!"npl" %in% names(vector_list[[i]])){
      vector_list[[i]]$npl <- NA
    }
    if(!"plare" %in% names(vector_list[[i]])){
      vector_list[[i]]$plare <- NA
    }
  }
  
  vects <- do.call(rbind, vector_list)
  rm(vector_list)
  
  sf <- st_as_sf(vects)
  sf <- st_cast(sf, "POLYGON")
  #sf <- st_remove_holes(sf, max_area = 0)
  vects <- as(sf, "Spatial")
  rm(sf)

  writeOGR(vects, "./classificador_vol_america/vect/global", "global_", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  stop("Vectors have been saved in classificador_vol_america/vect/global/global_.shp. This file must be corrected and saved again as global_corrected")
}

merge_vectors_2 <- function(){
  vects <- readOGR("./classificador_vol_america/vect/global/global_corrected.shp")
  
  vects <- reproject_EPSG_25831_vect(vects)
  
  iii <- 1:length(vects)
  vects$neighbors <- ""
  vects$border <- F
  vects$id <- 1:length(vects)
  if(vects[1,]$id==0){
    vects$id <- vects$id +1
  }
  
  v <- vect(vects)
  vects$area <- expanse(v)
  rm(v)
  
  vects$toremove <- F
  
  find_neighbors<- function(i, vects){
    print(i)
    v <- vects[i,]
    vects.n_quad <- vects[vects$ori %in% str_split(v$n_quad, ",")[[1]],]
    for(ii in 1:nrow(vects.n_quad)){
      if (v$id==vects.n_quad[ii,]$id){
        next()
      }
      if(gIntersects(v, vects.n_quad[ii,], byid = F)==T){
        print("a")
        if(v$neighbors == ""){
          v$neighbors <- vects.n_quad[ii,]$id
        }else{
          v$neighbors <- paste(v$neighbors, vects.n_quad[ii,]$id, sep=",")
        }
        if(v$ori != vects.n_quad[ii,]$ori){
          print("c")
          v$border <- T
          #vects[ii,"border"] <- T
          
          if(v$area<=vects.n_quad[ii,]$area){
            possibleError <- tryCatch(
              expr = {
                v <- v - vects.n_quad[ii,]
              },
              error=function(e) {e}
            )
            if(inherits(possibleError, "error")) {
              if(grepl("cannot derive coordinates from non-numeric matrix", possibleError)) {
                print("r")
                print(paste(i, possibleError, sep="-"))
                v$toremove <- T
              }else{
               print(paste(i, possibleError, sep="-"))
               stop(possibleError)
              }
            }
          }
          
          #if(v$area>=vects.n_quad[ii,]$area){
            #v <- v - vects.n_quad[ii,]       
          #}
        }
        #potser amb buffer?
      # }else if(gTouches(v, vects.n_quad[ii,], byid = F)==T){
      #   print("b")
      #   if(v$neighbors == ""){
      #     v$neighbors <- vects.n_quad[ii,]$id
      #   }else{
      #     v$neighbors <- paste(v$neighbors, vects.n_quad[ii,]$id, sep=",")
      #   }
      #   if(v$ori != vects.n_quad[ii,]$ori){
      #     v$border <- T
      #     #vects[ii,"border"] <- T
      #   }
      }
    }
    return(v)
  }

  # cluster <- makeCluster(detectCores()-1, outfile="log_merge.txt")
  # clusterExport(cluster, list("iii", "find_neighbors", "vects"))
  # clusterEvalQ(cluster, list(library(sp), library(rgeos), library(stringr)))
  # t <- parSapplyLB(cluster, iii, find_neighbors, vects)
  # stopCluster(cluster)
  # # 
  
  
  vects <- do.call(rbind, sapply(iii, find_neighbors, vects))
  
  # sf <- st_as_sf(vects)
  # sf <- st_cast(sf, "POLYGON")
  # vects <- as(sf, "Spatial")
  # rm(sf)
  
  gc()
  
  for(id in vects$id){
    print(id)
    if(vects[vects$id==id,]$toremove==T){
      vects <- vects[vects$id!=id,]
    }
  }
  library(nngeo)
  sf <- st_as_sf(vects)
  sf <- st_remove_holes(sf, max_area = 0)
  vv <- as(sf, "Spatial")
  
  
  writeOGR(vects, "./classificador_vol_america/vect/global", "global", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  
  file.remove("./classificador_vol_america/vect/global/global_.shp")
  file.remove("./classificador_vol_america/vect/global/global_.dbf")
  file.remove("./classificador_vol_america/vect/global/global_.shx")
  file.remove("./classificador_vol_america/vect/global/global_.prj")
  
  vects <- reproject_EPSG_4258_vect(vects)
  vects
}

merge_vectors_3 <- function(ids=NULL){
  #vector_list <- get_clumped_cut_by_quad(ids)
  
  ids <- as.character(ids)
  #vector_list <- lapply(get_clumped_vectors(ids), reproject_EPSG_25831_vect)
  vector_list <- get_clumped_vectors(ids)
                        
  if(is.null(ids)){
    ids <- get_clumped_ids()
  }
  for(i in 1:length(vector_list)){
    vector_list[[i]]$ori <- names(vector_list)[i]
    vector_list[[i]]$n_quad <- vector_list[[i]]$ori
  }
  
  for(i in 1:length(vector_list)){
    print(i)
    v1 <- vector_list[[i]]
    for(ii in 1:length(vector_list)){
      if(i==ii){next()}
      v2 <- vector_list[[ii]]
      
      # if(((extent(v1)@xmin == extent(v2)@xmin && extent(v1)@xmax == extent(v2)@xmax)&&
      #    (extent(v1)@ymin == extent(v2)@ymax || extent(v1)@ymax == extent(v2)@ymin))||
      #    ((extent(v1)@ymin == extent(v2)@ymin && extent(v1)@ymax == extent(v2)@ymax)&&
      #    (extent(v1)@xmin == extent(v2)@xmax || extent(v1)@xmax == extent(v2)@xmin))||
      #    (extent(v1)@ymax == extent(v2)@ymin && extent(v1)@xmin == extent(v2)@xmax)||
      #    (extent(v1)@ymin == extent(v2)@ymax && extent(v1)@xmin == extent(v2)@xmax)||
      #    (extent(v1)@ymin == extent(v2)@ymax && extent(v1)@xmax == extent(v2)@xmin)||
      #    (extent(v1)@ymax == extent(v2)@ymin && extent(v1)@xmax == extent(v2)@xmin))
      #   {
        if((extent(v1)@ymin < extent(v2)@ymax && extent(v1)@ymax > extent(v2)@ymax && 
            (round(extent(v1)@xmin,2) == round(extent(v2)@xmin,2) || 
             round(extent(v1)@xmax,2) == round(extent(v2)@xmin, 2) || 
             round(extent(v1)@xmin,2) == round(extent(v2)@xmax, 2))) ||
           (extent(v1)@xmin < extent(v2)@xmax && extent(v1)@xmax > extent(v2)@xmax && 
            (round(extent(v1)@ymin,2) == round(extent(v2)@ymin,2) || 
             round(extent(v1)@ymax,2) == round(extent(v2)@ymin, 2) || 
             round(extent(v1)@ymin,2) == round(extent(v2)@ymax, 2))) ||
           (extent(v1)@xmax > extent(v2)@xmin && extent(v1)@xmin < extent(v2)@xmin &&
            extent(v1)@ymin < extent(v2)@ymax && extent(v1)@ymax > extent(v2)@ymin) 
           ||
           (extent(v1)@xmax > extent(v2)@xmin && extent(v1)@xmin < extent(v2)@xmin &&
            extent(v1)@ymax > extent(v2)@ymin && extent(v1)@ymin < extent(v2)@ymin)
           ||
           (extent(v1)@xmin < extent(v2)@xmax && extent(v1)@xmax > extent(v2)@xmax &&
            extent(v1)@ymax > extent(v2)@ymin && extent(v1)@ymin < extent(v2)@ymin)
           ||
           (extent(v1)@xmin < extent(v2)@xmax && extent(v1)@xmax > extent(v2)@xmax &&
            extent(v1)@ymin < extent(v2)@ymax && extent(v1)@ymax > extent(v2)@ymin)
        ){
        print("a")
        #if(gIntersects(v1, v2, byid = F)==T){
          vector_list[[i]] <- v1-v2
          vector_list[[i]]$n_quad <- paste(vector_list[[i]]$n_quad[1], v2$ori[1], sep=",")
          vector_list[[ii]]$n_quad <- paste(vector_list[[ii]]$n_quad[1], v1$ori[1], sep=",")
          v1 <- vector_list[[i]]  
        
      }
    }
  }
  rm(v1)
  rm(v2)
  rm(i)
  rm(ii)
  gc()
  
  
  
  for(i in 1:length(vector_list)){
    if(!"DN" %in% names(vector_list[[i]])){
      vector_list[[i]]$DN <- NA
    }
    if(!"sd" %in% names(vector_list[[i]])){
      vector_list[[i]]$sd <- NA
    }
    if(!"npl" %in% names(vector_list[[i]])){
      vector_list[[i]]$npl <- NA
    }
    if(!"plare" %in% names(vector_list[[i]])){
      vector_list[[i]]$plare <- NA
    }
  }
  
  vects <- do.call(rbind, vector_list)
  rm(vector_list)
  
  
  
  sf <- st_as_sf(vects)
  #sf <- st_cast(sf, "POLYGON")
  sf <- st_make_valid(sf)
  library(nngeo)
  sf <- st_remove_holes(sf, max_area = 0)
  # sf$id <- 1:length(sf)
  # nbrs <- st_touches(sf)
  vects <- as(sf, "Spatial")
  
  
  
  vects <- find_neighbors_(vects)
  writeOGR(vects, "./classificador_vol_america/vect/global", "global_3", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  #vects <- reproject_EPSG_4258_vect(vects)
  vects
}

find_neighbors_ <- function(vects){
  iii <- 1:length(vects)
  vects$neighbors <- ""
  vects$border <- F
  vects$id <- 1:length(vects)
  
  
  find_neighbors__<- function(i, vects){
    print(i)
    v <- vects[i,]
    vects.n_quad <- vects[vects$ori %in% str_split(v$n_quad, ",")[[1]],]
    
    for(ii in 1:nrow(vects.n_quad)){
      if (v$id==vects.n_quad[ii,]$id){
        next()
      }
      if(gIntersects(v, vects.n_quad[ii,], byid = F)==T){
      #if(gTouches(v, vects.n_quad[ii,], byid = F)==T){
        #if(gTouches(gBuffer(v, byid=TRUE, width=0), gBuffer(vects.n_quad[ii,], byid=TRUE, width=0), byid = F)==T){
        print("a")
        if(v$neighbors == ""){
          v$neighbors <- vects.n_quad[ii,]$id
        }else{
          v$neighbors <- paste(v$neighbors, vects.n_quad[ii,]$id, sep=",")
        }
        if(v$ori != vects.n_quad[ii,]$ori){
          print("b")
          v$border <- T
        }
      }
    }
    return(v)
  }
  vects <- do.call(rbind, sapply(iii, find_neighbors__, vects))
  vects
}


get_clumped_cut_by_quad <- function(ids=NULL){
  if(is.null(ids)){
    ids <- get_clumped_ids()
  }
  ids <- as.character(ids)
  vector_list <- lapply(get_clumped_vectors(ids), reproject_EPSG_25831_vect)
  
  for(i in 1:length(vector_list)){
    vector_list[[i]]$ori <- names(vector_list)[i]
    vector_list[[i]]$n_quad <- vector_list[[i]]$ori
  }
  
  quads <- reproject_EPSG_25831_vect(get_quad_vect(ids))
  cut_by_quad_ <- function(id, vector_list, quads){
    return(crop(vector_list[[id]], quads[quads$id==id,]))
  }
  vector_list <- lapply(ids, cut_by_quad_, vector_list, quads)
}





