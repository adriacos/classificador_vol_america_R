
insertIFN_VA_Class <- function(data){
  write.table(data, file="data/ifn_va_class.csv", append=TRUE, col.names=FALSE, sep=",")
}