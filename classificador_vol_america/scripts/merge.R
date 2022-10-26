

# library(sf)
# library(tidyverse)

merge_rasters <- function(rasters){
  r1 <- rasters[[1]]
  rasts <- rasters[-1]
  templates <- lapply(rasts, function(x) projectRaster(from = x, to= r1, alignOnly=TRUE))
  aligned <- mapply(function(x, y) projectRaster(from=x, to=y), rasts, templates)
  merged <- r1
  for(i in 1:length(rasts)){
    merged <- merge(merged, aligned[[i]])
  }
  merged
}

merge_clumped <- function(){
  merge_vectors(get_clumped_vectors())
}

merge_vectors <- function(vector_list){
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
  
  
  for(i in 1:length(vector_list)){
    v1 <- vector_list[[i]]
    for(ii in 1:length(vector_list)){
      if(i==ii){next()}
      v2 <- vector_list[[ii]]
      vector_list[[i]] <- v1-v2
      v1 <- vector_list[[i]]
    }
  }
  vects <- do.call(rbind, vector_list)
  writeOGR(vects, "./classificador_vol_america/vect/global", "global", driver = "ESRI Shapefile", overwrite_layer = TRUE)
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