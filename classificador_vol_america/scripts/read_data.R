
read_quad_ids_not_exported <- function(notin){
  all <- read_quad_ids(notin)
  exported <- read_quad_ids_exported()
  all[!(all %in% exported)]
}

read_quad_ids <- function(notin){
  ids <- get_quad_vect()$id
  ids <- ids[!(ids %in% notin)]
  ids
}

get_quad_vect <- function(ids=NULL){
  if(is.null(ids)){
    return(readOGR("C:/Users/a.cos/Documents/Tesi/DADES/Quadricula/1kmx1km_BCN.gpkg"))
    #readOGR("C:/Users/a.cos/Documents/Tesi/DADES/Quadricula/1kmx1km_BCN.gpkg") 
  }else{
    vect <- readOGR("C:/Users/a.cos/Documents/Tesi/DADES/Quadricula/1kmx1km_BCN.gpkg")
    #vect <- readOGR("C:/Users/a.cos/Documents/Tesi/DADES/Quadricula/1kmx1km_BCN.gpkg") 
    return(vect[vect$id %in% ids,])
  }
  
}

read_quad_ids_exported <- function(){
  if (file.size("./classificador_vol_america/ids_done.txt") > 0){
    return(read.table("./classificador_vol_america/rasters/ids_exported.txt"))
  }else{
    return(NULL)
  }
}


# read_plots <- function(){
#   print("read_plots")
#   plots_done <- get_plots_done()
#   if(length(plots_done) == 0){
#     readIFN4()
#   }else{
#     readIFN4(plots_done)
#   }
# }

get_done_ids <- function(){
  if (file.size("./classificador_vol_america/ids_done.txt") > 0){
    return(read.table("./classificador_vol_america/ids_done.txt"))
  }else{
    return(NULL)
  }
}

get_classified_ids <- function(){
  print("get_classified_ids")
  files <- list.files("./classificador_vol_america/vect/classified", pattern = "\\.shp$")
  files <- sub(".shp","",files)
  files
}
get_in_progress_classification_ids <- function(){
  files <- list.files("./classificador_vol_america/vect/classified/inprogress", pattern = "\\.shp$")
  files <- sub(".shp","",files)
  files
}

get_metrics_ids <- function(){
  files <- list.files("./classificador_vol_america/vect/metrics/", pattern = "\\.shp$")
  files <- sub(".shp","",files)
  files <- sub("_mtcs","",files)
  files
}

get_clumped_ids <- function(){
  files <- list.files("./classificador_vol_america/vect/clumped/", pattern = "\\.shp$")
  files <- sub(".shp","",files)
  files <- sub("_clmp","",files)
  files
}

get_clumped_not_metrics_ids <- function(){
  print("get_clumped_not_metrics_ids")
  metrics <- get_metrics_ids()
  files <- get_clumped_ids()
  files <- files[!(files %in% metrics)]
  files
}

get_metrics_not_classified_ids <- function(){
  print("get_metrics_not_classified_ids")
  classified <- get_classified_ids()
  files <- get_metrics_ids()
  files <- files[!(files %in% classified)]
  files
}

get_exported_ids <- function(){
  print("get_exported_ids")
  list.files("./classificador_vol_america/rasters", pattern = "\\.tif$")
}

get_exported_not_smoothen_ids <- function(){
  print("get_exported_not_smoothen_ids")
  done <- get_smoothen_ids()
  files <- get_exported_ids()
  return(files[!(substring(files,1,7) %in% done)])
}

get_vectorised_not_clumped_ids <- function(){
  print("get_vectorised_not_clumped_ids")
  vectorised <- get_vectorised_ids()
  clumped <- get_clumped_ids()
  not_done <- vectorised[!(vectorised %in% clumped)]
  not_done
}

get_vectorised_ids <- function(){
  print("get_vectorised_ids")
  files <- get_vectorised_files()
  ids <- sub(".shp","",files)
  ids
}

get_vectorised_files <- function(){
  print("get_vectorised_files")
  list.files("./classificador_vol_america/vect/vectorised", pattern = "\\.shp$")
}

get_smoothen_ids <- function(){
  print("get_smoothen_ids")
  files <- list.files("./classificador_vol_america/rasters/smoothen", pattern = "\\_smth.tif$")
  ids <- sub("_smth.tif","",files)
  ids
}
# get_smoothen_files <- function(){
#   print("get_plots_smoothen_done")
#   files <- get_raster_ids_smoothen_done()
#   files <- substring(files,1,7)
#   files
# }

# get_plots_done <- function(){
#   print("get_plots_done")
#   files <- get_exported_ids()
#   files <- substring(files,1,7)
#   files
# }

# read_plot <- function(plot_id){
#   print("read_plot")
#   ifn4 <- readIFN4()
#   ifn4[ifn4$plot_id==plot_id,]
# }
# 
# readIFN4 <- function(notIn_ids=NULL){
#   print("readIFN4")
#   #TODO: use read.csv.sql filtering by not in notIn_ids
#   ifn4 <- read.csv("./classificador_vol_america/data/20220330_nfi_data.csv")[ 
#     ,c('plot_id',
#        'coords_longitude', 
#        'coords_latitude', 
#        'admin_province',
#        'admin_region',
#        'topo_altitude_asl',
#        'admin_aut_community',
#        'admin_municipality')]
#   if(!is.null(notIn_ids)){
#     ifn4 <- ifn4[!(ifn4$plot_id %in% notIn_ids),]
#   }
#   ifn4
# }