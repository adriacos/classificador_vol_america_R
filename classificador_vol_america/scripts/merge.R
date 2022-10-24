
merge_clumped <- function(){
  vect <- do.call(rbind, get_clumped_vectors())
  vect <- cut_overlapping_vectors(vect)
  vect
}



library(sf)
library(tidyverse)

cut_overlapping_vectors <- function(polygons) {
  
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