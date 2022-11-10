
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
  merge_vectors(get_clumped_vectors())
}

merge_vectors <- function(vector_list){
  vector_list <- lapply(vector_list, reproject_EPSG_25831_vect)
  quads <- get_quad_vect(names(vector_list))
  vector_list <- lapply(names(vector_list), function(id, vector_list, quads){
    crop(vector_list[[id]], quads[quads$id==id,])
  }, vector_list, quads)
  
  for(i in 1:length(vector_list)){
    vector_list[[i]]$ori <- names(vector_list)[i]
    vector_list[[i]]$n_quad <- vector_list[[i]]$ori
  }
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
  writeOGR(vects, "./classificador_vol_america/vect/global", "global", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  vects <- reproject_EPSG_4258_vect(vects)
  return(vects)
  
  # vector_list <- get_clumped_vectors()
  # 
  # for(i in 1:length(vector_list)){
  #   vector_list[[i]]$ori <- names(vector_list)[i]
  # }
  # 
  # combos_v <- matrix(NA, 0, 2)
  # 
  # for(i in 1:length(vector_list)){
  #   v1 <- vector_list[[i]]
  #   for(ii in 1:length(vector_list)){
  #     if(i==ii){next()}
  #     v2 <- vector_list[[ii]]
  #     if(gIntersects(v1, v2, byid = FALSE)){
  #       combos_v <- rbind(combos_v, c(v1$ori[[1]], v2$ori[[1]]))
  #     }
  #     v1 <- vector_list[[i]]
  #   }
  # }
  # 
  # for(k in 1:length(vector_list)){
  #   vector_list[[k]]$id <- as.numeric(row.names(vector_list[[k]]))
  #   if(vector_list[[k]][1,]$id==0){
  #     vector_list[[k]]$id <- vector_list[[k]]$id +1
  #   }
  # }
  # 
  # 
  # for (k in 1:nrow(combos_v)){
  #   vect1 <- vector_list[combos_v[k,1]][[1]]
  #   vect2 <- vector_list[combos_v[k,2]][[1]]
  #   print(paste("vect1:", vect1$ori[1], "vect2:", vect2$ori[1]))
  #   for(i in 1:nrow(vect1)){
  #     v1 <- vect1[vect1$id==i,]
  #     print(paste("v1:", i))
  #     for(ii in 1:nrow(vect2)){
  #       v2 <- vect2[vect2$id==ii,]
  #       print(paste("v2:", ii))
  #       if(gIntersects(v1, v2, byid = FALSE)){
  #         vect1 <- vect1[!(vect1$id==i),]
  #         vect1 <- rbind(vect1, v1-v2)
  #         v1 <- vect1[vect1$id==i,]
  #       }
  #     }
  #   }
  #   vector_list[combos_v[k,1]][[1]] <- vect1
  # }
  # rm(v1)
  # rm(v2)
  # rm(vect1)
  # rm(vect2)
  # rm(i)
  # rm(ii)
  # rm(k)
  # 
  # 
  # vects <- do.call(rbind, vector_list)
  # writeOGR(vects, "./classificador_vol_america/vect/global", "global", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  # return(vects)
  
  
  
  #mirar quins vectors_list són contigus amb quins, apuntar-ho
  #per a cada vector_list, agafar tots els vect
  #per a cada vect, restar-li tots els vects dels vectors_list adjacents
  #ajuntar tots els vector_list
  
  vector_list <- lapply(vector_list, reproject_EPSG_25831_vect)
  
  
  
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
  
  #cal guardar l'arxiu, corregir-lo amb qgis (o mirar com corregir-lo amb R) i tornar-lo a carregar
  
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
  
  #provar de tallar els vects al quadrat estricte i fer el gTouches com al clump original però amb un buffer de 0
  # o bé fer el que segueix:

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
      }else if(gTouches(v, vects.n_quad[ii,], byid = F, checkValidity=TRUE)==T){
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
  
  vects <- sapply(iii, find_neighbors, vects)
  vects <- do.call(rbid, sapply(iii, find_neighbors, vects))
  
  sf <- st_as_sf(vects)
  sf <- st_cast(sf, "POLYGON")
  vects <- as(sf, "Spatial")
  rm(sf)
  
  gc()
  
  writeOGR(vects, "./classificador_vol_america/vect/global", "global", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  vects <- reproject_EPSG_4258_vect(vects)
  vects
}



# merge_clumped <- function(){
#   vect <- do.call(rbind, get_clumped_vectors())
#   vect <- cut_overlapping_vectors(vect)
#   vect
# }
# 
# 
# 
# 
# 
# 
# cut_overlapping_vectors <- function(vect) {
#   vect <- vect[order(vect$area),]
#   for(i in 1:length(vect)){
#     print(i)
#     v1 <- vect[i, ]
#     for(ii in 1:length(vect)){
#       if(i==ii){next()}
#       v2 <- vect[ii,]
#       v1 <- v1 - v2
#       vect.rest <- vect[-i,]
#       vect.t <- rbind(vect.rest, v1)
#     }
#   }
#   return(vect.t)
#   
#   #mirar de millorar-ho dividint la part comuna, etc.
#   polygons <- st_as_sf(polygons)
#   # 
#   # # pol = st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
#   # # b = st_sfc(pol, pol + c(.8, .2), pol + c(.2, .8))
#   # 
#   # independent <- b %>% st_sf %>% st_intersection %>% subset(n.overlaps<=1)
#   # overlap <- b %>% st_sf %>% st_intersection %>% subset(n.overlaps>1) %>% st_union()
#   # partition <- b %>% st_centroid %>% st_union %>% st_voronoi %>% st_cast
#   # 
#   # merge_list <- st_within(partition %>% st_intersection(overlap), b)
#   # 
#   # merged_list <- lapply(1:length(merge_list), function(i){st_sf(st_intersection(partition[i], b[merge_list[[i]]]))})
#   # 
#   # new_b <- do.call(rbind, merged_list)
#   # return(as(new_b, "Spatial"))
#   # 
#   
#   centroids <- polygons %>% st_centroid
#   
#   # Voronoi tesselation
#   voronoi <- 
#     centroids %>% 
#     st_geometry() %>%
#     st_union() %>%
#     st_voronoi() %>%
#     st_collection_extract()
#   
#   # Put them back in their original order
#   voronoi <-
#     voronoi[unlist(st_intersects(centroids,voronoi))]
#   
#   # Keep the attributes
#   result <- centroids
#   
#   # Intersect voronoi zones with buffer zones
#   st_geometry(result) <-
#     mapply(function(x,y) st_intersection(x,y),
#            #st_buffer(st_geometry(centroids),dist), 
#            polygons$geometry,
#            voronoi,
#            SIMPLIFY=FALSE) %>%
#     st_sfc(crs=st_crs(centroids))
#   
#   result
# }