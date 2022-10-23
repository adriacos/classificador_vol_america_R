
source("./classificador_vol_america/scripts/read_data.R")

insertIFN_VA_Class <- function(data){
  print("insert")
  csv <- NULL
  try(csv<- read.csv("data/ifn_va_class.csv"))
  if (!is.null(csv)){
    write.table(data, file="data/ifn_va_class.csv", append=TRUE, col.names=FALSE, row.names=FALSE, sep=",")
  } else{
    write.csv(data, "data/ifn_va_class.csv", row.names = FALSE)
  }
}

updateIFN_VA_Class <-function(register){
  print("update")
  ifn_va_class <- read.csv("data/ifn_va_class.csv")
  print(ifn_va_class[ifn_va_class$plot_id==register$plot_id,])
  print(register)
  ifn_va_class[ifn_va_class$plot_id==register$plot_id,] <- register
  ifn_va_class[ifn_va_class$plot_id==register$plot_id,]$sys_dt_started <-
    toString(register$sys_dt_started)
  ifn_va_class[ifn_va_class$plot_id==register$plot_id,]$sys_dt_done <-
    toString(register$sys_dt_done)
 
  write.csv(ifn_va_class, "data/ifn_va_class.csv", row.names = FALSE)
}

save_classified_vector <- function(name, vectors){
  writeOGR(vectors, "./classificador_vol_america/vect/classified/", paste(name, "_class", sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE) 
}
save_in_progress_classification_vector <- function(name, vectors){
  writeOGR(vectors, "./classificador_vol_america/vect/classified/inprogress/", name, driver = "ESRI Shapefile", overwrite_layer = TRUE)
}

save_metrics_vector <- function(name, vectors){
  writeOGR(vectors, "./classificador_vol_america/vect/metrics/", paste(name, "_mtcs", sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE) 
}

save_id_exported <- function(id){
  ids <- read_quad_ids_exported()
  if(is.null(ids)){
    write.table(id, "./classificador_vol_america/rasters/ids_exported.txt")
  }else{
    ids <- append(ids, id)
    write.table(ids, "./classificador_vol_america/rasters/ids_exported.txt")
  }
}

save_id_done <- function(id){
  ids <- get_done_ids()
  if(is.null(ids)){
    write.table(id, "./classificador_vol_america/ids_done.txt")
  }else{
    ids <- append(ids, id)
    write.table(ids, "./classificador_vol_america/ids_done.txt")
  }
}
