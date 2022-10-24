

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
  vect <- do.call(rbind, get_clumped_vectors())
  v <- cut_overlapping_vectors(vect)
  vect
}



library(sf)
library(tidyverse)

cut_overlapping_vectors <- function(polygons) {
  b <- st_as_sf(polygons)
  
  
  
  # pol = st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  # b = st_sfc(pol, pol + c(.8, .2), pol + c(.2, .8))
  
  independent <- b %>% st_sf %>% st_intersection %>% subset(n.overlaps<=1)
  overlap <- b %>% st_sf %>% st_intersection %>% subset(n.overlaps>1) %>% st_union()
  partition <- b %>% st_centroid %>% st_union %>% st_voronoi %>% st_cast
  
  merge_list <- st_within(partition %>% st_intersection(overlap), b)
  
  merged_list <- lapply(1:length(merge_list), function(i){st_sf(st_intersection(partition[i], b[merge_list[[i]]]))})
  
  new_b <- do.call(rbind, merged_list)
  return(as(new_b, "Spatial"))
  
  
  centroids <- polygons %>% st_centroid
  
  # Voronoi tesselation
  voronoi <- 
    centroids %>% 
    st_geometry() %>%
    st_union() %>%
    st_voronoi() %>%
    st_collection_extract()
  
  # Put them back in their original order
  voronoi <-
    voronoi[unlist(st_intersects(centroids,voronoi))]
  
  # Keep the attributes
  result <- centroids
  
  # Intersect voronoi zones with buffer zones
  st_geometry(result) <-
    mapply(function(x,y) st_intersection(x,y),
           #st_buffer(st_geometry(centroids),dist), 
           polygons$geometry,
           voronoi,
           SIMPLIFY=FALSE) %>%
    st_sfc(crs=st_crs(centroids))
  
  result
}