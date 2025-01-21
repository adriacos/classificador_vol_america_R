# library(nnet)


train <- function(){
  source("./classificador_vol_america/scripts/model.R")
  library(xgboost)
  vect_dt <- readRDS("./classificador_vol_america/vect/class_auto/all.rds")
  
  vect_dt <- vect_dt[,colnames(vect_dt)[!colnames(vect_dt)%in%c("npl","pldn","plare","tmp","geometry","neighbors")],with=F]
  vect_dt <- vect_dt[,colnames(vect_dt)[!colnames(vect_dt)%in%colnames(vect_dt)[grepl("^nmndif",colnames(vect_dt))]],with=F]
  vect_dt <- vect_dt[,colnames(vect_dt)[!colnames(vect_dt)%in%colnames(vect_dt)[grepl("^nsd",colnames(vect_dt))]],with=F]
  vect_dt <- vect_dt[,colnames(vect_dt)[!colnames(vect_dt)%in%c("quad_ori_allkm","quad_id_allkm","quad_row_allkm","quad_col_allkm")],with=F]
  
  i <- 1:200
  # for(i in 1:200){
    # print(i)
    vect_dt <- vect_dt[,colnames(vect_dt)[!colnames(vect_dt)%in%paste("quad_ori_",i,"km",sep="")],with=F]
    vect_dt <- vect_dt[,colnames(vect_dt)[!colnames(vect_dt)%in%paste("quad_id_",i,"km",sep="")],with=F]
    vect_dt <- vect_dt[,colnames(vect_dt)[!colnames(vect_dt)%in%paste("quad_row_",i,"km",sep="")],with=F]
    vect_dt <- vect_dt[,colnames(vect_dt)[!colnames(vect_dt)%in%paste("quad_col_",i,"km",sep="")],with=F]
  # }
  
  gc()
  
  vect_dt$class_auto_90 <- reclass(vect_dt$class_auto_90)
  vect_dt$class_auto_66 <- reclass(vect_dt$class_auto_66)
  vect_dt$class_auto_00 <- reclass(vect_dt$class_auto_00)
  
  table(vect_dt$class_auto_90)
  vect_dt$class_auto_90 <- reclass_to_modeltrain(vect_dt$class_auto_90)
  vect_dt$class_auto_66 <- reclass_to_modeltrain(vect_dt$class_auto_66,saveclasses=F)
  vect_dt$class_auto_00 <- reclass_to_modeltrain(vect_dt$class_auto_00,saveclasses=F)
  
  # vect_dt$class_auto_90 <- reclass_from_modeltrain(vect_dt$class_auto_90)
  # table(vect_dt$class_auto_90)
  
  vect_nn <- vect_dt[!is.na(vect_dt$class_auto_90),]
  vect_na <-vect_dt[is.na(vect_dt$class_auto_90),]
  
  train <- do.call(rbind,lapply(split(vect_nn,vect_nn$class_auto_90),function(vc,cl_90_table){
    fact <- (cl_90_table[which(names(cl_90_table)==vc$class_auto_90[[1]])]/(max(cl_90_table)))*3
    vc[sample(nrow(vc),min(ceiling(nrow(vc)/fact),floor(3*nrow(vc)/4))), ]
  },table(vect_nn$class_auto_90)))
  test <- vect_nn[!vect_nn$id%in%train$id,]
  rm(vect_nn)
  gc()
  
  train_x = data.matrix(train[,colnames(train)[!colnames(train)%in%c("id","class_auto_90","class_auto_66","class_auto_00")],with=F])
  train_y = as.vector(train[,c("class_auto_90"),with=F])[[1]]
  test_x = data.matrix(test[,colnames(test)[!colnames(test)%in%c("id","class_auto_90","class_auto_66","class_auto_00")],with=F])
  test_y = test[,c("class_auto_90"),with=F][[1]]
  xgb_train = xgb.DMatrix(data=train_x,label=train_y)
  xgb_test = xgb.DMatrix(data=test_x,label=test_y)
  # rm(train_x)
  # rm(train_y)
  # rm(test_x)
  # rm(test_y)
  gc()
  
  watchlist <- list(train=xgb_train,test=xgb_test)
  library(parallel)
  
  model <- xgb.train(data=xgb_train,max.depth=3,watchlist=watchlist,
                     nrounds=round(2100*(nrow(train)/28510)),
                     verbose=1,nthread=detectCores(),objective="multi:softprob",
                     num_class=length(unique(train_y)),early_stopping_rounds=round(120*(nrow(train)/28510)))
  final = xgboost(data=xgb_train,max.depth=3,nrounds=which.min(model$evaluation_log$test_mlogloss),
                  verbose=0,nthread=detectCores(),objective="multi:softprob",
                  num_class=length(unique(train_y)))
  
  # final = xgboost(data=xgb_train,max.depth=3,nrounds=191,
  #                 verbose=0,nthread=detectCores(),objective="multi:softmax",
  #                 num_class=length(unique(train_y)))
  
  
  # pred <- predict(final,test_x)
  # test$class_pred <- pred
  # # tt <- test[!is.na(test$class_auto_90),]
  # print(paste("error: ",nrow(test[test$class_auto_90!=test$class_pred,])/(nrow(test)),sep=""))
  # 
  # test$class_auto_90 <- reclass_from_modeltrain(test$class_auto_90)
  # test$class_auto_66 <- reclass_from_modeltrain(test$class_auto_66)
  # test$class_auto_00 <- reclass_from_modeltrain(test$class_auto_00)
  # test$class_pred <- reclass_from_modeltrain(test$class_pred)
  # 
  # # saveRDS(test,"./classificador_vol_america/vect/class_auto/test.rds")
  # 
  # library(data.table)
  # geoms <- data.table(st_as_sf(readRDS("./classificador_vol_america/vect/class_auto/all.rds"))[,c("id","geometry"),with=F])
  # # st_as_sf(geoms)
  # test <- merge(test,geoms)
  # rm(geoms)
  # # rm(final)
  # gc()
  # saveRDS(test[,c("id","geometry","class_auto_90","class_auto_66","class_auto_00","success_90","class_pred")],"./classificador_vol_america/vect/class_auto/test.rds")
  # # test <- readRDS("./classificador_vol_america/vect/class_auto/test.rds")
  # 
  # test_f <- test[class_auto_90!=class_pred]
  # test_s <- test[class_auto_90==class_pred]
  # 1-(table(test_f$class_auto_90)/table(test$class_auto_90))
  # apply(table(test[,c("class_pred","class_auto_90")]),1,function(tb,wg){
  #   round(tb/wg,2)
  # },table(test$class_auto_90))
  # 
  # test$success_90 <- 0
  # test[class_auto_90==class_pred,]$success_90 <- 1
  # test$success_66 <- 0
  # test[class_auto_66==class_pred,]$success_66 <- 1
  # test$success_00 <- 0
  # test[class_auto_00==class_pred,]$success_00 <- 1
  # 
  # nrow(test[success_90==1])/nrow(test)
  # nrow(test[success_66==1])/nrow(test)
  # nrow(test[success_00==1])/nrow(test)
  # 
  
  test_all <- vect_dt#[!vect_dt$id%in%train$id,]
  test_x_all = data.matrix(test_all[,colnames(test_all)[!colnames(test_all)%in%c("id","class_auto_90","class_auto_66","class_auto_00")],with=F])
  pred <- predict(final,test_x_all)
  library(dplyr)
  prediction <- matrix(pred,nrow=length(unique(train_y)),ncol=length(pred)/length(unique(train_y))) %>%
    t() %>%
    data.frame()
  colnames(prediction) <- paste("cl_",unique(train_y),"perc",sep="")
  rm(pred)
  vect_dt <- cbind(vect_dt,prediction)
  
  vv <- readRDS("./classificador_vol_america/vect/class_auto/all.rds")
  vect_dt <- merge(vect_dt,vv[,append("id",colnames(vv)[!colnames(vv)%in%colnames(vect_dt)]),with=F])
  saveRDS(vect_dt,"./classificador_vol_america/vect/class_auto/all_percpred.rds")
  # vect_dt <- readRDS("./classificador_vol_america/vect/class_auto/all_percpred.rds")
  
  # source("./classificador_vol_america/scripts/model.R")
  metrics <- paste("cl_",unique(train_y),"perc",sep="")
  # metrics <- c("cl_0perc","cl_1perc","cl_2perc","cl_3perc","cl_4perc","cl_5perc","cl_6perc","cl_7perc","cl_8perc","cl_9perc")
  
  vect_dt <- calc_neighbor_metrics_model(vect_dt,metrics=metrics)
  # colnames(vect_dt)[colnames(vect_dt)%in%paste("nmndif",metrics,sep="")] <- paste("nmn",metrics,sep="")
  
  saveRDS(vect_dt,"./classificador_vol_america/vect/class_auto/all_percpred.rds")
  
  train <- merge(train,vect_dt[,append("id",paste("nmn",metrics,sep="")),with=F])
  # colnames(train)[colnames(train)%in%paste("nmndif",metrics,sep="")] <- paste("nmn",metrics,sep="")
  test <- merge(test,vect_dt[,append("id",paste("nmn",metrics,sep="")),with=F])
  # colnames(test)[colnames(test)%in%paste("nmndif",metrics,sep="")] <- paste("nmn",metrics,sep="")
  
  train_x = data.matrix(train[,colnames(train)[!colnames(train)%in%c("id","class_auto_90","class_auto_66","class_auto_00")],with=F])
  train_y = as.vector(train[,c("class_auto_90"),with=F])[[1]]
  test_x = data.matrix(test[,colnames(test)[!colnames(test)%in%c("id","class_auto_90","class_auto_66","class_auto_00")],with=F])
  test_y = test[,c("class_auto_90"),with=F][[1]]
  xgb_train = xgb.DMatrix(data=train_x,label=train_y)
  xgb_test = xgb.DMatrix(data=test_x,label=test_y)
  gc()
  
  watchlist <- list(train=xgb_train,test=xgb_test)
  library(parallel)
  model <- xgb.train(data=xgb_train,max.depth=3,watchlist=watchlist,
                     nrounds=round(2100*(nrow(train)/28510)),
                     verbose=1,nthread=detectCores(),objective="multi:softmax",
                     num_class=length(unique(train_y)),early_stopping_rounds=round(120*(nrow(train)/28510)))
  final = xgboost(data=xgb_train,max.depth=3,nrounds=which.min(model$evaluation_log$test_mlogloss),
                  verbose=0,nthread=detectCores(),objective="multi:softmax",
                  num_class=length(unique(train_y)))
  
  
  # test_all <- vect_dt[!vect_dt$id%in%train$id,]
  test_all <- vect_dt[vect_dt$id%in%train$id,]
  test_all <- test_all[,colnames(test_all)[!colnames(test_all)%in%c("npl","pldn","plare","tmp","geometry","neighbors")],with=F]
  test_all <- test_all[,colnames(test_all)[!colnames(test_all)%in%colnames(test_all)[grepl("^nmndif",colnames(test_all))]],with=F]
  test_all <- test_all[,colnames(test_all)[!colnames(test_all)%in%colnames(test_all)[grepl("^nsd",colnames(test_all))]],with=F]
  test_all <- test_all[,colnames(test_all)[!colnames(test_all)%in%c("quad_ori_allkm","quad_id_allkm","quad_row_allkm","quad_col_allkm")],with=F]
  i <- 1:200
  test_all <- test_all[,colnames(test_all)[!colnames(test_all)%in%paste("quad_ori_",i,"km",sep="")],with=F]
  test_all <- test_all[,colnames(test_all)[!colnames(test_all)%in%paste("quad_id_",i,"km",sep="")],with=F]
  test_all <- test_all[,colnames(test_all)[!colnames(test_all)%in%paste("quad_row_",i,"km",sep="")],with=F]
  test_all <- test_all[,colnames(test_all)[!colnames(test_all)%in%paste("quad_col_",i,"km",sep="")],with=F]
  
  test_x_all = data.matrix(test_all[,colnames(test_all)[!colnames(test_all)%in%append(c("id","class_auto_90","class_auto_66","class_auto_00"),
                                                                                      metrics)],with=F])
  test_x_all <- test_x_all[,colnames(train_x)]
  pred <- predict(final,test_x_all)
  
  
  # colnames(prediction) <- reclass_from_modeltrain(colnames(prediction))
  # prediction <- mutate(prediction,label=test_y,max_prob=max.col(prediction,"last")-1)
  
  test_all$class_pred <- pred
  print(paste("error_all_90: ",nrow(test_all[!is.na(test_all$class_auto_90)&test_all$class_auto_90!=test_all$class_pred,])/(nrow(test_all[!is.na(test_all$class_auto_90)])),sep=""))
  print(paste("error_all_66: ",nrow(test_all[!is.na(test_all$class_auto_66)&test_all$class_auto_66!=test_all$class_pred,])/(nrow(test_all[!is.na(test_all$class_auto_66)])),sep=""))
  print(paste("error_all_00: ",nrow(test_all[!is.na(test_all$class_auto_00)&test_all$class_auto_00!=test_all$class_pred,])/(nrow(test_all[!is.na(test_all$class_auto_00)])),sep=""))
  
  test_all$class_auto_90 <- reclass_from_modeltrain(test_all$class_auto_90)
  test_all$class_auto_66 <- reclass_from_modeltrain(test_all$class_auto_66)
  test_all$class_auto_00 <- reclass_from_modeltrain(test_all$class_auto_00)
  test_all$class_pred <- reclass_from_modeltrain(test_all$class_pred)
  
  test_f_90 <- test_all[class_auto_90!=class_pred]
  test_s_90 <- test_all[class_auto_90==class_pred]

  results_neighbors_all <- 1-(table(test_f_90$class_auto_90)/table(test_all$class_auto_90))
  # save(results_neighbors_all,file="./classificador_vol_america/temp/results_neighbors_all.RData")
  # load("./classificador_vol_america/temp/results_neighbors_all.RData")
  results_neighbors_byclass_90 <- apply(table(test_all[,c("class_pred","class_auto_90")], useNA = "always"),1,function(tb,wg){
    round(tb/wg,2)
  },table(test_all$class_auto_90, useNA = "always"))
  # save(results_neighbors_byclass_90,file="./classificador_vol_america/temp/results_neighbors_byclass_90.RData")
  load("./classificador_vol_america/temp/results_neighbors_byclass_90.RData")
  results_neighbors_byclass_66 <- apply(table(test_all[,c("class_pred","class_auto_66")], useNA = "always"),1,function(tb,wg){
    round(tb/wg,2)
  },table(test_all$class_auto_66, useNA = "always"))
  # save(results_neighbors_byclass_66,file="./classificador_vol_america/temp/results_neighbors_byclass_66.RData")
  results_neighbors_byclass_00 <- apply(table(test_all[,c("class_pred","class_auto_00")], useNA = "always"),1,function(tb,wg){
    round(tb/wg,2)
  },table(test_all$class_auto_00, useNA = "always"))
  # save(results_neighbors_byclass_00,file="./classificador_vol_america/temp/results_neighbors_byclass_00.RData")
  
  
  
  test_all$success_90 <- 0
  test_all[!is.na(class_auto_90)&class_auto_90==class_pred,]$success_90 <- 1
  test_all[is.na(class_auto_90)]$success_90 <- NA
  test_all$success_66 <- 0
  test_all[!is.na(class_auto_66)&class_auto_66==class_pred,]$success_66 <- 1
  test_all[is.na(class_auto_66)]$success_66 <- NA
  test_all$success_00 <- 0
  test_all[!is.na(class_auto_00)&class_auto_00==class_pred,]$success_00 <- 1
  test_all[is.na(class_auto_00)]$success_00 <- NA
  
  library(data.table)
  geoms <- data.table(st_as_sf(readRDS("./classificador_vol_america/vect/class_auto/all.rds"))[,c("id","geometry"),with=F])
  # st_as_sf(geoms)
  test_all <- merge(test_all,geoms)
  rm(geoms)
  # rm(final)
  gc()
  saveRDS(test_all[,c("id","geometry","class_auto_90","class_auto_66","class_auto_00","success_90","class_pred")],"./classificador_vol_america/vect/class_auto/train.rds")
  
  
  # nrow(test_all[success_90==1])/nrow(test_all)
  # nrow(test_all[success_66==1])/nrow(test_all)
  # nrow(test_all[success_00==1])/nrow(test_all)
  # 
  
  # mapply(function(a,b){
  #   a/b
  # },table(test_f$class_auto_90),table(test$class_auto_90))
  
  importance_matrix <- xgb.importance(model=final)
  importance_matrix[importance_matrix$Gain>0.005,]
  xgb.plot.importance(importance_matrix[importance_matrix$Gain>0.005,])
  relevant_fatures <- importance_matrix[importance_matrix$Gain>0.005,]$Feature
  
  # install.packages("SHAPforxgboost")
  # install.packages("shapr")
  libraryC(shapr)
  # library("SHAPforxgboost")
  # shap <- xgboost::xgb.shap.data(data=xgb_train,model=final)
  
  xgb.ggplot.shap.summary(train_x,model=final,target_class=5)
  xgb.plot.shap(train_x,model=final,target_class=0)
  
  
  shap_values <- shap.prep(xgb_model=final,X_train=train_x)
  explainer <- shapr(train_x, model)
  
  shap.summary_plot(shap_values, X_train)
  
  
}

reclass <- function(class){
  class[!is.na(class)&(class==101|class==102|class==103|class==104|class==201|class==601|class==602)] <- 1
  class[!is.na(class)&(class==301|class==708|class==1501)] <- 9#3
  class[!is.na(class)&(class==401)] <- 5#4
  class[!is.na(class)&(class==501|class==1101|class==1201)] <- 5
  class[!is.na(class)&(class==701|class==702)] <- 7
  class[!is.na(class)&(class==801)] <- 8
  class[!is.na(class)&(class==707|class==901)] <- 9
  class[!is.na(class)&(class==1001)] <- 10
  class[!is.na(class)&(class==1301)] <- 13
  class[!is.na(class)&(class==703|class==1401|class==2001)] <- 14
  class[!is.na(class)&(class==502|class==1701|class==1801|class==2201|class==2202)] <- 17
  
  class[!is.na(class)&(class==1601)] <- 17#16
  class[!is.na(class)&(class==1901)] <- 17#19
  
  class[!is.na(class)&(class==1703|class==2101|class==2102|class==2103|class==2104|class==2105|class==2106)] <- 21  
  class[!is.na(class)&(class==2301|class==2302|class==2303)] <- 23
  class[!is.na(class)&(class==704|class==2401|class==2402|class==2403|class==2404|class==2405|class==2406|class==2407)] <- 24  
  class[!is.na(class)&(class==705|class==706)] <- 71
  class[!is.na(class)&(class==1702|class==1704)] <- 1#171
  class
}

reclass_to_modeltrain <- function(class,saveclasses=T){
  # classes <- unique(class[!is.na(class)])
  
  class[!is.na(class)] <- paste("_",class[!is.na(class)],sep="")
  if(saveclasses==T){
    reclass_to_model_train <- rbind(unique(class[!is.na(class)]),0:(length(unique(class[!is.na(class)]))-1))
    save(reclass_to_model_train,file="./classificador_vol_america/temp/reclass_to_model_train.RData")    
  }else{
    load("./classificador_vol_america/temp/reclass_to_model_train.RData")
  }
  for(ic in 1:ncol(reclass_to_model_train)){
    # print(reclass_to_model_train[,ic])
    # print(paste(reclass_to_model_train[1,ic],"-",length(class[!is.na(class)&class==reclass_to_model_train[1,ic]])))
    class[!is.na(class)&class==reclass_to_model_train[1,ic]] <- reclass_to_model_train[2,ic]
  }
  class <- as.numeric(class)
  class
}
reclass_from_modeltrain <- function(class){
  load("./classificador_vol_america/temp/reclass_to_model_train.RData")
  for(ic in 1:ncol(reclass_to_model_train)){
    # print(reclass_to_model_train[,ic])
    class[!is.na(class)&class==reclass_to_model_train[2,ic]] <- reclass_to_model_train[1,ic]
  }
  class <- as.numeric(gsub("_","",class))
  class
}

calc_neighbor_metrics_model <- function(vect_dt,metrics){
  # metrics <- metrics[metrics%in%colnames(vect_dt)]
  library(parallel)
  n.cores <- detectCores()
  # n.cores <- 10
  vect_rest <- vect_dt[,-metrics,with=F]
  vect_dt <- vect_dt[,append(c("id","neighbors"),metrics),with=F]
  
  vect_dt$tmp <- ceiling((1:nrow(vect_dt))/(nrow(vect_dt)/n.cores))
  vect_spl <- split(vect_dt,vect_dt$tmp)
  # rm(vect_dt)
  vect_dt$tmp <- NULL
  gc()
  unlink("./classificador_vol_america/logs/calc_neighbor_metrics_model.txt")
  source("./classificador_vol_america/scripts/clump_vector.R")
  cl <- create_cluster_clump_(n.cores,"calc_neighbor_metrics_model")
  vect_spl <- parLapplyLB(cl,vect_spl,function(vv,vect_dt,metrics){
    setkey(vect_dt,id)
    do.call(rbind,lapply(1:nrow(vv),function(irow,vv,vect_dt,metrics){
      if(irow%%500==0){
        print(irow)        
      }
      calc_neighbor_metrics_model_(vv[irow,],vect_dt,metrics)
    },vv,vect_dt,metrics))
  },vect_dt,metrics)
  stopCluster(cl)
  rm(cl)
  unlink("./classificador_vol_america/logs/calc_neighbor_metrics_model.txt")
  # vect_dt$tmp <- NULL
  gc()
  vect_dt <- do.call(rbind,vect_spl)
  rm(vect_spl)
  gc()
  vect_dt <- merge(vect_dt,vect_rest)
  vect_dt
}


calc_neighbor_metrics_model_ <- function(v,vect_dt,metrics){
  # metrics <- metrics[metrics%in%colnames(v)]
  for(metric in metrics){
    if(v$neighbors==""){
      v[,paste("n","mn",metric,sep="")] <- NA
    }else{
      v[,paste("n","mn",metric,sep=""):=mean(vect_dt[id%in%str_split(v$neighbors, ",")[[1]]][[which(colnames(vect_dt)==metric)]],na.rm=T)]  
    }
  }
  return(v)
}
