event_kind <- "お盆"

# 各イベントの誤差最小値時点のインデックス取得
min_index <- c()
for (event_kind in event_name) {
  idx <- which(event_test[,event_kind] == 1)
  if (length(idx) > 0) {
    #list.dirs(getwd(), recursive = FALSE)
    
    average_list <- c()
    for (i in seq_along(gosa_list)) {
      heikin <- sum(gosa_list[i][idx,])/length(idx)
      average_list <- c(average_list, heikin)
    }
    kankou_error_plot <- data.frame(average_list,
                                    ei = t(ei_test_list[idx[1],]))
    colnames(kankou_error_plot) <- c("kankou_error", "ei")
    which.min(kankou_error_plot$kankou_error)
    min_index <- c(min_index, which.min(kankou_error_plot$kankou_error))
    
  }else{
    min_index <-  c(min_index,0)
  }
   
}

event_val_list2[[2]][1]
[[2]][2]
# use_eventにその時点のイベントインパクトの値を格納
use_event <- c()
loop_num <- 1
for (i in min_index){
  if (i ==0){
    use_event <- c(use_event, event_val_list2[[1]][loop_num])
  }else{
    event_val_list2[i]
    use_event <- c(use_event,  event_val_list2[[i]][loop_num])
  }
  loop_num <- loop_num +1
}

use_event
############################################################
d_train <- read.csv('/Users/e185716/研究/R/ei_make/event_train.csv')
d_test <- read.csv('/Users/e185716/研究/R/ei_make/event_test.csv')

# 上で取得したイベントインパクト適応
event_name
d_train$event_impact <- 0
d_test$event_impact <- 0
# イベント１つずつ値を更新
for (i in seq_along(event_name)){
  idx_test <- which(d_test[,event_name[i]] == 1)
  idx_train <- which(d_train[,event_name[i]] == 1)
  if(length(idx_test) == 0){
    d_train[idx_train,]$event_impact <- use_event[i]
  }else if (length(idx_train) == 0){
    d_test[idx_test,]$event_impact <- use_event[i]
  }else{
    d_test[idx_test,]$event_impact <- use_event[i]
    d_train[idx_train,]$event_impact <- use_event[i]
  }
}
# write.csv(x =d_train, file = "/Users/e185716/グラフ練習2/クロス/ei_error_min_train.csv")
# write.csv(x =d_test, file = "/Users/e185716/グラフ練習2/クロス/ei_error_min_test.csv")

typeof(d_train)
# d_train <- as.data.frame(d_train)
# d_test <- as.data.frame(d_test)

###########################################
# このイベントインパクトを使ってクロスバリデーションやるためのデータ構築用
# updated_ei <- append(d_train$event_impact, d_test$event_impact)
# updated_ei <- data.frame(updated_ei)
# all_df$event_impact <- updated_ei[,]
###########################################

FV1 <- c("CustNum","event_impact")
FV2 <- c("CustNum","day1_sales","event_impact")
FV3 <- c("CustNum","day1_sales","day1_kankou","event_impact")
FV4 <- c("CustNum","day1_sales","day1_kankou","day1_tabelog","event_impact")


fv_learn_error_min <- function(FV, FV_name){
  fv_train <-d_train
  fv_test <- d_test
  fv_train <-fv_train[,FV]
  fv_test <- fv_test[,FV]
  save_file_name <- paste0("特徴ベクトル/",FV_name,"/ei_error_min/pred")
  pred <- shift_learn(fv_train, fv_test, save_file_name, "yes")
  
  save_data <- data.frame(d_test$date, pred$mean)
  colnames(save_data) <- c("date", "prediction")
  file_name <- paste0("/Users/e185716/グラフ練習2/", save_file_name, ".csv")
  write.csv(x =save_data, file = file_name)
}



fv_learn_error_min(FV1,"FV1")
fv_learn_error_min(FV2,"FV2")
fv_learn_error_min(FV3,"FV3")
fv_learn_error_min(FV4,"FV4")


########################################################
# 7日先予測
original_shift_train <- d_train
original_shift_test <- d_test
original_shift_train <- original_shift_train[,c("CustNum", "event_impact")]
original_shift_test <- original_shift_test[,c("CustNum", "event_impact")]

# 7日前までの客数を変数に追加
for (i in 1:7) {
  # non_ei_pred <- non_ei_inspection(d_train, d_test, "non_ei_pred")
  new_column_name <- paste0(i, "_ago_cust")  # 新しい列名
  
  original_shift_test[new_column_name] <- lag(original_shift_test$CustNum, n=i, default = NA) # テストデータシフト
  original_shift_test[new_column_name][1:i,] <- tail(d_train$CustNum, i) # テストデータの最初に学習データに含まれていた値を代入
  
  original_shift_train[new_column_name] <- lag(original_shift_train$CustNum, n=i, default = NA) # 学習データシフト
}

shift_train <- original_shift_train
shift_test <- original_shift_test
for (i in 1:7){
  save_file_name <- paste0( "7期先/adapted_ei/pred_ahead_", i)
  shift_nonei_pred <- shift_learn(shift_train, shift_test, save_file_name, "yes")
  
  # 予測値をテストデータに挿入
  shift_test <- cbind(shift_test[, 1:2], shift_nonei_pred$mean, shift_test[, 3:ncol(shift_test)])
  del_col_num <-  3+i
  shift_test <- shift_test[, -del_col_num]
  colnames(shift_test) <- col_names
}


########################################################

non_ei_pred2 <- read.csv('/Users/e185716/グラフ練習2/特徴ベクトル/FV3/ei_non/pred.csv')
first_ei_pred2 <-read.csv('/Users/e185716/グラフ練習2/特徴ベクトル/FV3/ei_1/pred.csv')
hundred_ei_pred <- read.csv('/Users/e185716/グラフ練習2/特徴ベクトル/FV3/ei_100/pred.csv')
ei_error_min <- read.csv('/Users/e185716/グラフ練習2/特徴ベクトル/FV3/ei_error_min/pred.csv')

for (event_kind in event_name) {
  idx <- which(event_test[,event_kind] == 1)
  if (length(idx) > 0) {
    print(event_kind)
    print(length(idx))
    print("RMSE")
    print(paste("EIなし：",round(calculate_rmse(d_test$CustNum[idx], non_ei_pred2$prediction[idx]),3)))
    print(paste("初期値：",round(calculate_rmse(d_test$CustNum[idx], first_ei_pred2$prediction[idx]),3)))
    print(paste("100周：",round(calculate_rmse(d_test$CustNum[idx], hundred_ei_pred$prediction[idx]),3)))
    print(paste("各最小値：",round(calculate_rmse(d_test$CustNum[idx], ei_error_min$prediction[idx]),3)))
    
    
    # print(paste("5周：",round(calculate_rmse(d_test$CustNum[idx], fifth_ei_pred$mean[idx]),3)))
    # print(paste("10周：",round(calculate_rmse(d_test$CustNum[idx], tenth_ei_pred$mean[idx]),3)))
    
    # print("決定係数")
    # print(paste("EIなし：",round(calculate_r_squared(d_test$CustNum[idx], non_ei_pred2$prediction[idx]),3)))
    # print(paste("初期値：",round(calculate_r_squared(d_test$CustNum[idx], first_ei_pred2$prediction[idx]),3)))
    # print(paste("100周：",round(calculate_r_squared(d_test$CustNum[idx], hundred_ei_pred$prediction[idx]),3)))
    # print(paste("各最小値：",round(calculate_r_squared(d_test$CustNum[idx], ei_error_min$prediction[idx]),3)))
    
    
    # print(paste("5周：",round(calculate_r_squared(d_test$CustNum[idx], fifth_ei_pred$mean[idx]),3)))
    # print(paste("10周：",round(calculate_r_squared(d_test$CustNum[idx], tenth_ei_pred$mean[idx]),3)))
    print("-------------------------------------")
  }
}
idx <- which(event_test[,"祝日"] == 1)
non_ei_pred2[idx[0:10],]$prediction
first_ei_pred2[idx[0:10],]$prediction
hundred_ei_pred[idx[0:10],]$prediction
ei_error_min[idx[0:10],]$prediction
d_test[idx[0:10],]$CustNum

length(idx)
