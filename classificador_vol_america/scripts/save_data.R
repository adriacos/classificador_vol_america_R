
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
