

readData <- function(){
  
  print(getIdsAlreadyDone())
  
  ifn4 <- readIFN4(getIdsAlreadyDone())
  
  ifn4$percForestal <- NA
  ifn4$percAgricola <- NA
  ifn4$percPrados <- NA
  ifn4$percResidencial <- NA
  ifn4$percIndustrial <- NA
  ifn4$percInproductivo <- NA
  ifn4$percAgua <- NA
  ifn4$percOtros <- NA
  ifn4$tipoMosaico <- NA
  
  ifn4
}

getIdsAlreadyDone <- function(){
  readIFN_VA_Class()[, c("plot_id")]
}
readIFN_VA_Class <- function(){
  read.csv("./data/ifn_va_class.csv")
}

readIFN4 <- function(notIn_ids){
  #TODO: use read.csv.sql filtering by not in notIn_ids
  ifn4 <- read.csv("./data/20220330_nfi_data.csv")[ ,c('plot_id','coords_longitude', 'coords_latitude')]
  ifn4 <- ifn4[!(ifn4$plot_id %in% notIn_ids),]
  ifn4 <- ifn4[1:10,]
  ifn4
}