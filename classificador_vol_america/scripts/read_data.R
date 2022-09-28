
read_parcels <- function(){
  parcels_done <- get_parcels_done()
  if(length(parcels_done)){
    readIFN4()
  }else{
    readIFN4(parcels_done)
  }
}

get_parcels_done <- function(){
  files <- list.files("./classificador_vol_america/rasters")
  files <- substring(files,1,7)
  files
}

readIFN4 <- function(notIn_ids){
  print("readIFN4")
  #TODO: use read.csv.sql filtering by not in notIn_ids
  ifn4 <- read.csv("./classificador_vol_america/data/20220330_nfi_data.csv")[ 
    ,c('plot_id',
       'coords_longitude', 
       'coords_latitude', 
       'admin_province', 
       'admin_municipality')]
  if(!is.null(notIn_ids)){
    ifn4 <- ifn4[!(ifn4$plot_id %in% notIn_ids),]
  }
  ifn4
}