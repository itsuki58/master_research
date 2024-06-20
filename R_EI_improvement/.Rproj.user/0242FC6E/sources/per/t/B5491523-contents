library(dplyr)

d_train <- read.csv('/Users/e185716/研究/R/ei_make/event_train.csv')
d_test <- read.csv('/Users/e185716/研究/R/ei_make/event_test.csv')

d_train$event_impact <- ei_train_list[,100]
d_test$event_impact <- ei_test_list[,100]

# EIを含む予測
ei_inspection <- function(train_data, test_data, csv_name){
  d_train <- train_data
  d_test <- test_data
  
  # イベントの有無onehotを削除
  d_train <- subset(d_train, select = -c((ncol(d_train)-10):ncol(d_train)))
  d_test <- subset(d_test, select = -c((ncol(d_test)-10):ncol(d_test)))
  
  
  #日付省く
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "date"))
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "date"))
  
  
  #sales消す
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "salesTotal"))
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "salesTotal"))
  
  #kankou,tabelog消す
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "kankou"))
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "kankou"))
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "tabelog"))
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "tabelog"))
  
  #イベントインパクト消す
  # d_train <- subset(d_train, select = setdiff(colnames(d_train), "event_impact"))
  # d_test <- subset(d_test, select = setdiff(colnames(d_test), "event_impact"))

  # d_train <- d_train[,1:5]
  # d_test <- d_test[,1:5]
  for (i in event_name){
    d_train <- subset(d_train, select = setdiff(colnames(d_train), i))
    d_test <- subset(d_test, select = setdiff(colnames(d_test), i))
  }
  
  d_train[!complete.cases(d_train),]
  
  #d_train <- d_train[-1,]
  d_train[is.na(d_train)] <- 0
  print(d_train[!complete.cases(d_train),])
  
  d_test[is.na(d_test)] <- 0
  d_test[!complete.cases(d_test),]
  
  # 季節,トレンド,回帰成分.
  library(bsts)
  # ss <- AddLocalLinearTrend(list(),d_train$CustNum)
  # ss <- AddSeasonal(ss, d_train$CustNum, nseasons = 7)
  ss <- AddSeasonal(list(), d_train$CustNum, nseasons = 7)
  
  model <- bsts(CustNum~., state.specification = ss, niter=1000, data=d_train)
  
  # 予測 
  #pred <- predict(model, burn=100, newdata=d_test3[,-2])
  pred <- predict(model, burn=100, newdata=d_test)
  # file_name <- paste0("/Users/e185716/グラフ練習/来客数予測/", csv_name, ".csv")
  # file_name <- paste0("/Users/e185716/グラフ練習/", csv_name, ".csv")
  
  # write.csv(x = pred$mean, file = file_name)

  return(pred)
}

# EIを含まない予測
non_ei_inspection <- function(train_data, test_data, csv_name){
  d_train <- train_data
  d_test <- test_data
  
  # イベントの有無onehotを削除
  d_train <- subset(d_train, select = -c((ncol(d_train)-10):ncol(d_train)))
  d_test <- subset(d_test, select = -c((ncol(d_test)-10):ncol(d_test)))
  
  
  #日付省く
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "date"))
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "date"))
  
  
  #sales消す
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "salesTotal"))
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "salesTotal"))
  
  #kankou,tabelog消す
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "kankou"))
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "kankou"))
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "tabelog"))
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "tabelog"))
  
  #イベントインパクト消す
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "event_impact"))
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "event_impact"))
  
  # d_train <- d_train[,1:5]
  # d_test <- d_test[,1:5]
  for (i in event_name){
    d_train <- subset(d_train, select = setdiff(colnames(d_train), i))
    d_test <- subset(d_test, select = setdiff(colnames(d_test), i))
  }
  
  d_train[!complete.cases(d_train),]
  
  #d_train <- d_train[-1,]
  d_train[is.na(d_train)] <- 0
  print(d_train[!complete.cases(d_train),])
  
  d_test[is.na(d_test)] <- 0
  d_test[!complete.cases(d_test),]
  
  # 季節,トレンド,回帰成分.
  library(bsts)
  # ss <- AddLocalLinearTrend(list(),d_train$CustNum)
  # ss <- AddSeasonal(ss, d_train$CustNum, nseasons = 7)
  ss <- AddSeasonal(list(), d_train$CustNum, nseasons = 7)
  
  model <- bsts(CustNum~., state.specification = ss, niter=1000, data=d_train)
  
  # 予測 
  #pred <- predict(model, burn=100, newdata=d_test3[,-2])
  pred <- predict(model, burn=100, newdata=d_test)
  # file_name <- paste0("/Users/e185716/グラフ練習/来客数予測/", csv_name, ".csv")
  file_name <- paste0("/Users/e185716/グラフ練習/", csv_name, ".csv")
  
  write.csv(x = pred$mean, file = file_name)
  
  return(pred)
}


non_ei_pred <- non_ei_inspection(d_train, d_test, "non_ei_pred")
first_ei_pred <- ei_inspection(d_train, d_test, "first_ei_pred")
fifth_ei_pred <- ei_inspection(d_train, d_test, "fifth_ei_pred")
tenth_ei_pred <- ei_inspection(d_train, d_test, "tenth_ei_pred")
hundred_ei_pred <- ei_inspection(d_train, d_test, "hundred_ei_pred")

# グラフの保存
save_graph <- function(pred, graph_name){
  file_name <- paste0("/Users/e185716/グラフ練習/来客数予測/", graph_name, ".png")
  png(file_name, width = 800, height = 600)
  par(family = "HiraKakuProN-W3")
  par(oma = c(0, 0,0, 1.5))
  plot(pred, type = "l", xlab="" ,ylab="Number of Customer")
  #lines(plot_ei$ei, col = "red")
  par(new = TRUE)
  plot(d_test$CustNum,col = "red", type = "l", axes = FALSE, xlab = "", ylab = "")
  box()
  legend("topleft", legend = c("prediction", "original"), col = c("black", "red"), lty = 1,bg = "transparent")
  dev.off()
}

save_graph(non_ei_pred$mean, "non_ei_pred")
save_graph(first_ei_pred$mean, "first_ei_pred")
save_graph(fifth_ei_pred$mean, "fifth_ei_pred")
save_graph(tenth_ei_pred$mean, "tenth_ei_pred")


 #  決定係数を求める
calculate_r_squared <- function(y_actual, y_predicted) {
  # 実際の値と予測値の差の二乗和
  SS_res <- sum((y_actual - y_predicted)^2)
  # 実際の値の平均を計算
  mean_y <- mean(y_actual)
  # 実際の値と平均値の差の二乗和
  SS_tot <- sum((y_actual - mean_y)^2)
  # 決定係数の計算
  r_squared <- 1 - (SS_res / SS_tot)
  return(r_squared)
}

calculate_r_squared(d_test$CustNum, non_ei_pred$mean)
calculate_r_squared(d_test$CustNum, first_ei_pred$mean)
calculate_r_squared(d_test$CustNum, fifth_ei_pred$mean)
calculate_r_squared(d_test$CustNum, tenth_ei_pred$mean)


# RMSEを求める
calculate_rmse <- function(actual, predicted) {
  n <- length(predicted)
  if (length(actual) != n) {
    stop("予測値と実際の値の長さが一致していません。")
  }
  
  squared_errors <- (predicted - actual) ^ 2
  mean_squared_error <- sum(squared_errors) / n
  rmse <- sqrt(mean_squared_error)
  return(rmse)
}

calculate_rmse(d_test$CustNum, non_ei_pred$mean)
calculate_rmse(d_test$CustNum, first_ei_pred$mean)
calculate_rmse(d_test$CustNum, fifth_ei_pred$mean)
calculate_rmse(d_test$CustNum, tenth_ei_pred$mean)

event_kind <- event_name[2]



# 各イベントごとにRMSE, 決定係数を求める
for (event_kind in event_name) {
  idx <- which(event_test[,event_kind] == 1)
  if (length(idx) > 0) {
    print(event_kind)
    print(length(idx))
    print("RMSE")
    print(paste("EIなし：",round(calculate_rmse(d_test$CustNum[idx], non_ei_pred$mean[idx]),3)))
    print(paste("初期値：",round(calculate_rmse(d_test$CustNum[idx], first_ei_pred$mean[idx]),3)))
    print(paste("初期値：",round(calculate_rmse(d_test$CustNum[idx], hundred_ei_pred$prediction[idx]),3)))
    
    # print(paste("5周：",round(calculate_rmse(d_test$CustNum[idx], fifth_ei_pred$mean[idx]),3)))
    # print(paste("10周：",round(calculate_rmse(d_test$CustNum[idx], tenth_ei_pred$mean[idx]),3)))
    
    print("決定係数")
    print(paste("EIなし：",round(calculate_r_squared(d_test$CustNum[idx], non_ei_pred$mean[idx]),3)))
    print(paste("初期値：",round(calculate_r_squared(d_test$CustNum[idx], first_ei_pred$mean[idx]),3)))
    print(paste("100周：",round(calculate_r_squared(d_test$CustNum[idx], hundred_ei_pred$prediction[idx]),3)))
    
    # print(paste("5周：",round(calculate_r_squared(d_test$CustNum[idx], fifth_ei_pred$mean[idx]),3)))
    # print(paste("10周：",round(calculate_r_squared(d_test$CustNum[idx], tenth_ei_pred$mean[idx]),3)))
    print("-------------------------------------")
    }
}



#####################################################################
# 特徴ベクトルの組み合わせごと
d_train <- read.csv('/Users/e185716/研究/R/ei_make/event_train.csv')
d_test <- read.csv('/Users/e185716/研究/R/ei_make/event_test.csv')

# 使用する特徴ベクトルの組み合わせ
FV1 <- c("CustNum","event_impact")
FV2 <- c("CustNum","day1_sales","event_impact")
FV3 <- c("CustNum","day1_sales","day1_kankou","event_impact")
FV4 <- c("CustNum","day1_sales","day1_kankou","day1_tabelog","event_impact")


ei_step_num <- c(100)  # 用いるイベントインパクトのステップ数

# FV <- FV1
# FV_name <- "FV1"
# i <- 1


fv_learn <- function(FV, FV_name){
  for (i in ei_step_num){
    # イベントインパクトを使用しない場合
    if (i == 0){
      fv_train <-d_train[,FV]
      fv_test <- d_test[,FV]
      save_file_name <- paste0("特徴ベクトル/",FV_name,"/ei_non/","pred")
      pred <- shift_learn(fv_train, fv_test, save_file_name, "no")
      
      save_data <- data.frame(d_test$date, pred$mean)
      colnames(save_data) <- c("date", "prediction")
      file_name <- paste0("/Users/e185716/グラフ練習2/", save_file_name, ".csv")
      write.csv(x =save_data, file = file_name)
    }else{  # ↓使用する場合
      train_test_list <- ei_step(d_train,d_test,i)
      fv_train <- train_test_list[[1]]
      fv_test <- train_test_list[[2]]
      fv_train <-fv_train[,FV]
      fv_test <- fv_test[,FV]
      save_file_name <- paste0("特徴ベクトル/",FV_name,"/ei_",i,"/pred")
      pred <- shift_learn(fv_train, fv_test, save_file_name, "yes")
      
      save_data <- data.frame(d_test$date, pred$mean)
      colnames(save_data) <- c("date", "prediction")
      file_name <- paste0("/Users/e185716/グラフ練習2/", save_file_name, ".csv")
      write.csv(x =save_data, file = file_name)
      
    }
  }
}
# write.csv(x =ei_test_list, file = "/Users/e185716/グラフ練習/イベントインパクト_test.csv")
# write.csv(x =ei_train_list, file = "/Users/e185716/グラフ練習/イベントインパクト_train.csv")


fv_learn(FV1,"FV1")
fv_learn(FV2,"FV2")
fv_learn(FV3,"FV3")
fv_learn(FV4,"FV4")




cal_data = read.csv("/Users/e185716/グラフ練習/特徴ベクトル/FV4/ei_10/pred.csv")

for (event_kind in event_name) {
  idx <- which(event_test[,event_kind] == 1)
  if (length(idx) > 0) {
    print(event_kind)
    print(length(idx))
    print("RMSE")
    print(paste("EIなし：",round(calculate_rmse(d_test$CustNum[idx], non_ei_pred$mean[idx]),3)))
    print(paste("1周：",round(calculate_rmse(d_test$CustNum[idx], first_ei_pred$mean[idx]),3)))
    print(paste("5周：",round(calculate_rmse(d_test$CustNum[idx], fifth_ei_pred$mean[idx]),3)))
    print(paste("10周：",round(calculate_rmse(d_test$CustNum[idx], tenth_ei_pred$mean[idx]),3)))
    
    print("決定係数")
    print(paste("EIなし：",round(calculate_r_squared(d_test$CustNum[idx], non_ei_pred$mean[idx]),3)))
    print(paste("1周：",round(calculate_r_squared(d_test$CustNum[idx], first_ei_pred$mean[idx]),3)))
    print(paste("5周：",round(calculate_r_squared(d_test$CustNum[idx], fifth_ei_pred$mean[idx]),3)))
    print(paste("10周：",round(calculate_r_squared(d_test$CustNum[idx], tenth_ei_pred$mean[idx]),3)))
    print("-------------------------------------")
  }
}


first_ei_pred <- read.csv("/Users/e185716/EI_1_-1/one_minus_one.csv")
tenth_ei_pred = read.csv("/Users/e185716/EI_1_-1/min_ei.csv")
d_test$CustNum

for (event_kind in event_name) {
  idx <- which(event_test[,event_kind] == 1)
  if (length(idx) > 0) {
    print(event_kind)
    print(length(idx))
    print("RMSE")
    # print(paste("EIなし：",round(calculate_rmse(d_test$CustNum[idx], non_ei_pred[idx]),3)))
    print(paste("0周：",round(calculate_rmse(d_test$CustNum[idx], first_ei_pred$prediction[idx]),2)))
    print(paste("最小：",round(calculate_rmse(d_test$CustNum[idx], tenth_ei_pred$prediction[idx]),2)))
    
    print("-------------------------------------")
  }
}
