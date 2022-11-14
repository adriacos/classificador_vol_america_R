library(exactextractr)
source("./classificador_vol_america/scripts/read_data.R")
source("./classificador_vol_america/scripts/project.R")


auto_class_BCN <- function(vect){
  rast <- read_1956_diba_rast()
  vect <- reproject_EPSG_25831_vect(vect)
  
  ex <- getclass(vect, rast)
  #ex <- exact_extract(rast, vect, "getmode")
  vect$clss_aut <- ex
  save_auto_class_BCN_vect(vect)
  vect
}

getclass <- function(vects, rast){
  
  vects$id___ <- as.numeric(row.names(vects))
  if(vects[1,]$id___==0){
    vects$id___ <- vects$id___ +1
  }
  sapply(vects$id___, getclass_, vects, rast)
}

getclass_ <- function(id, vects, rast){
  getclass__(vects[vects$id___==id,], rast)
}

getclass__ <- function(vect, rast) {
  r <- crop(rast, vect)
  r <- mask(r, vect)
  #uniqv <- unique(r)
  tab <- table(values(r))
  if(which.max(tab)/sum(tab)>=0.66){
    c <- as.numeric(names(which.max(tab)))
    if(length(c)==0){
      return(NA)
    }else{
      return(c)
    }
  }else{
    return NA
  }
}

save_auto_class_BCN_vect <- function(vect){
  writeOGR(vect, "./classificador_vol_america/vect/classified/automatically", "global", driver = "ESRI Shapefile", overwrite_layer = TRUE) 
} 