library(exactextractr)
source("./classificador_vol_america/scripts/read_data.R")

auto_class_BCN <- function(vect, id){
  rast <- read_1956_diba_rast(id)
  ex <- exact_extract(rast, vect, "getmode")
  vect$clss_aut <- ex
  save_auto_class_BCN_vect(vect, id)
  vect
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

save_auto_class_BCN_vect <- function(vect, id){
  writeOGR(vect, "./classificador_vol_america/vect/10km/classified/automatically", paste(id, "_class_auto", sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE) 
} 