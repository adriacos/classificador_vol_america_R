library(nnet)


train <- function(){
  name <- "P_00001-Barcelona-Guardiola de Berguedà"
  
  vect <- readOGR("./classificador_vol_america/vect/classified/P_00002-Barcelona-Bagà_class.shp")
  vect_pred <- readOGR("./classificador_vol_america/vect/metrics/P_00001-Barcelona-Guardiola de Berguedà_class.shp")
  #data <- as(vect, "data.frame")
  #model = multinom(class ~ mn+std+elv+slp+mtp+apt+mpt+rpl+lat+lng+are+per+shp+n_mn_mn+n_sd_mn+n_mxdf_mn+n_mn_mdn+n_mn_std+n_mn_slp+n_mn_shp+n_std_shp, 
  #                 data = data, trace = FALSE)
  
  fitControl <- trainControl(method = "repeatedcv",   
                             number = 10,     # number of folds
                             repeats = 10,
                             search = "random")
  #lambdaGrid <- expand.grid(lambda = 10^seq(10, -2, length=100))
  
  
  model = multinom(class ~ mn+std+elv+slp+mtp+apt+mpt+rpl+are+per+shp+n_mn_mn+n_sd_mn+n_mxdf_mn+n_mn_mdn+n_mn_std+n_mn_slp+n_mn_shp+n_std_shp, 
                   data = vect, trace = FALSE,
                   trControl = fitControl,
                   preProcess = c('scale', 'center'),
                   #tuneGrid = lambdaGrid,
                   na.action = na.omit 
                   )
  
  pred <- predict(model, newdata=vect_pred, type="prob")
  
  vect_pred$pred_class <- colnames(pred)[apply(pred, 1, which.is.max)]
  
  # good <- nrow(vect[!is.na(vect$pred_class) & vect$class==vect$pred_class,])
  # na <- nrow(vect[is.na(vect$pred_class),])
  # bad <- nrow(vect[!is.na(vect$pred_class) & vect$class!=vect$pred_class,])
  # total <- nrow(vect)
  # 
  # print(paste("total: ", total, ", good: ", good, ", na: ", na, ", bad: ", bad, sep=""))
  
  writeOGR(vect_pred, "./classificador_vol_america/vect/classified", paste(name, "_pred", sep=""), driver = "ESRI Shapefile", overwrite_layer = TRUE) 
  
}
