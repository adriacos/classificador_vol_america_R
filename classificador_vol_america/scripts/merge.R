
source("./classificador_vol_america/scripts/project.R")
source("./classificador_vol_america/scripts/read_data.R")


# library(sf)
# library(tidyverse)

merge_rasters <- function(rasters){
  r1 <- rasters[[1]]
  rasts <- rasters[-1]
  templates <- lapply(rasts, function(x) projectRaster(from = x, to= r1, alignOnly=TRUE))
  aligned <- mapply(function(x, y) projectRaster(from=x, to=y), rasts, templates)
  merged <- r1
  #dividir-ho en varies parts (tantes com cores) i fer cadascuna en paral·lel i després ajuntar-ho
  for(i in 1:length(rasts)){
    print(i)
    merged <- merge(merged, aligned[[i]])
  }
  writeRaster(merged, "./classificador_vol_america/rasters/all/global.tif")
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
      #if(gIntersects(v1, v2, byid = F)==T){
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
  vects <- as(sf, "Spatial")
  rm(sf)

  writeOGR(vects, "./classificador_vol_america/vect/global", "global_", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  stop("Vectors have been saved in classificador_vol_america/vect/global/global_.shp. This file must be corrected and saved again as global_corrected")
}

merge_vectors_2 <- function(){
  vects <- readOGR("./classificador_vol_america/vect/global/global_corrected.shp")
  
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
          if(v$area>=vects.n_quad[ii,]$area){
            v <- v - vects.n_quad[ii,]       
          }
        }
        #potser amb buffer?
      }else if(gTouches(v, vects.n_quad[ii,], byid = F)==T){
        print("b")
        if(v$neighbors == ""){
          v$neighbors <- vects.n_quad[ii,]$id
        }else{
          v$neighbors <- paste(v$neighbors, vects.n_quad[ii,]$id, sep=",")
        }
        if(v$ori != vects.n_quad[ii,]$ori){
          v$border <- T
          #vects[ii,"border"] <- T
        }
      }
    }
    return(v)
  }
  
  # cluster <- makeCluster(detectCores(), outfile="log_clump.txt")
  # clusterExport(cluster, list("iii", "find_neighbors", "vect"))
  # clusterEvalQ(cluster, list(library(sp), library(rgeos), library(stringr)))
  # neighbors <- parSapply(cluster, iii, find_neighbors, vect)
  # stopCluster(cluster)
  # 
  
  t <- sapply(iii, find_neighbors, vects)
  vects <- do.call(rbid, sapply(iii, find_neighbors, vects))
  
  # sf <- st_as_sf(vects)
  # sf <- st_cast(sf, "POLYGON")
  # vects <- as(sf, "Spatial")
  # rm(sf)
  
  gc()
  
  writeOGR(vects, "./classificador_vol_america/vect/global", "global", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  
  file.remove("./classificador_vol_america/vect/global/global_.shp")
  file.remove("./classificador_vol_america/vect/global/global_.dbf")
  file.remove("./classificador_vol_america/vect/global/global_.shx")
  file.remove("./classificador_vol_america/vect/global/global_.prj")
  
  vects <- reproject_EPSG_4258_vect(vects)
  vects
}