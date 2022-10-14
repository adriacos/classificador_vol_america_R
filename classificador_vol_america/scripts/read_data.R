
read_parcels <- function(){
  print("read_parcels")
  parcels_done <- get_parcels_done()
  if(length(parcels_done) == 0){
    readIFN4()
  }else{
    readIFN4(parcels_done)
  }
}

get_vectors_classified_file_names <- function(){
  print("get_vectors_classified_file_names")
  files <- list.files("./classificador_vol_america/vect/classified", pattern = "\\.shp$")
  files <- sub(".shp","",files)
  files
}
get_vectors_in_progress_classification_file_names <- function(){
  files <- list.files("./classificador_vol_america/vect/classified/inprogress", pattern = "\\.shp$")
  files <- sub(".shp","",files)
  files
}

get_vectors_metrics_file_names <- function(){
  files <- list.files("./classificador_vol_america/vect/metrics/", pattern = "\\.shp$")
  files <- sub(".shp","",files)
  files <- sub("_mtcs","",files)
  files
}

get_vectors_clumped_file_names <- function(){
  files <- list.files("./classificador_vol_america/vect/clumped/", pattern = "\\.shp$")
  files <- sub(".shp","",files)
  files <- sub("_clmp","",files)
  files
}

get_vectors_clumped_file_names_not_metrics <- function(){
  print("get_vectors_clumped_file_names_not_metrics")
  metrics <- get_vectors_metrics_file_names()
  files <- get_vectors_clumped_file_names()
  files <- files[!(files %in% metrics)]
  files
}

get_vectors_metrics_file_names_not_classified <- function(){
  print("get_vectors_metrics_file_names_not_classified")
  classified <- get_vectors_classified_file_names()
  files <- get_vectors_metrics_file_names()
  files <- files[!(files %in% classified)]
  files
}

get_raster_names_done <- function(){
  print("get_raster_names_done")
  list.files("./classificador_vol_america/rasters", pattern = "\\.tif$")
}

get_raster_names_done_not_smoothen <- function(){
  print("get_raster_names_done_not_smoothen")
  done <- get_parcels_smoothen_done()
  files <- get_raster_names_done()
  return(files[!(substring(files,1,7) %in% done)])
}

get_names_smoothen_vectorised_not_clumped <- function(){
  print("get_names_smoothen_vectorised_not_clumped")
  vectorised <- get_names_smoothen_vectorised()
  clumped <- get_vectors_classified_file_names()
  not_done <- vectorised[!(vectorised %in% clumped)]
  not_done
}

get_names_smoothen_vectorised <- function(){
  print("get_names_smoothen_vectorised")
  files <- get_files_smoothen_vectorised()
  names <- sub(".shp","",files)
  names
}

get_files_smoothen_vectorised <- function(){
  print("get_files_smoothen_vectorised")
  list.files("./classificador_vol_america/vect/vectorised", pattern = "\\.shp$")
}

get_raster_names_smoothen_done <- function(){
  print("get_raster_names_smoothen_done")
  files <- list.files("./classificador_vol_america/rasters/smoothen", pattern = "\\_smth.tif$")
  names <- sub("_smth.tif","",files)
  names
}
get_parcels_smoothen_done <- function(){
  print("get_parcels_smoothen_done")
  files <- get_raster_names_smoothen_done()
  files <- substring(files,1,7)
  files
}

get_parcels_done <- function(){
  print("get_parcels_done")
  files <- get_raster_names_done()
  files <- substring(files,1,7)
  files
}

read_plot <- function(plot_id){
  print("read_plot")
  ifn4 <- readIFN4()
  ifn4[ifn4$plot_id==plot_id,]
}

readIFN4 <- function(notIn_ids=NULL){
  print("readIFN4")
  #TODO: use read.csv.sql filtering by not in notIn_ids
  ifn4 <- read.csv("./classificador_vol_america/data/20220330_nfi_data.csv")[ 
    ,c('plot_id',
       'coords_longitude', 
       'coords_latitude', 
       'admin_province',
       'admin_region',
       'topo_altitude_asl',
       'admin_aut_community',
       'admin_municipality')]
  if(!is.null(notIn_ids)){
    ifn4 <- ifn4[!(ifn4$plot_id %in% notIn_ids),]
  }
  ifn4
}