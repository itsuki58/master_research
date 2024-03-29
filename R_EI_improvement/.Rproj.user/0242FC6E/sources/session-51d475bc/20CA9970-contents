d_train <- read.csv('/Users/e185716/研究/R/ei_make/event_train.csv')
d_test <- read.csv('/Users/e185716/研究/R/ei_make/event_test.csv')

all_df <- read.csv('/Users/e185716/研究/R/ei_make/data_before_split.csv')

# library(dplyr)

# 更新したイベントインパクトを結合
update_train <- list(ei_train_list[,100])
update_test <- list(ei_test_list[,100])
updated_ei <- append(update_train[[1]], update_test[[1]])
updated_ei <- data.frame(updated_ei)
all_df$event_impact <- updated_ei[,]

cross_train_first <- 1
cross_train_last <- 730
cross_test_first <- 731
cross_test_last <- cross_test_first + 30
loop_num <- ceiling((nrow(all_df) - cross_train_last) / 30)

split_train <- list()
split_test <- list()
cross_result <- list()
cross_rmse_list <- list()

for (i in 1:loop_num) {
  d_train <- all_df[cross_train_first:cross_train_last,]
  if (i == loop_num){
    d_test <- all_df[cross_test_first:nrow(all_df),]
  }else{
    d_test <- all_df[cross_test_first:cross_test_last,]
  }
  date_list <- d_test$date
  d_train <- d_train[,FV]
  d_test <-d_test[,FV]
  cross_pred <- cross_learn(d_train, d_test,"yes")
  save_file_name <- paste0("/Users/e185716/グラフ練習2/クロス/ei_error_min/period_",i,".csv")
  save_data <- data.frame(date_list, cross_pred$mean)
  colnames(save_data) <- c("date", "prediction")
  write.csv(x =save_data, file = save_file_name)
  
  cross_result <- c(cross_result,list(cross_pred$mean))
  
  cross_rmse <- calculate_rmse(d_test$CustNum, cross_pred$mean)
  cross_r2 <- calculate_r_squared(d_test$CustNum, cross_pred$mean)
  print(paste0("period_", i, ", RMSE : ", round(cross_rmse,3), " , R2 : ", round(cross_r2,3)))
  cross_rmse_list <- c(cross_rmse_list,(paste0("period_", i, ", RMSE : ", round(cross_rmse,3), " , R2 : ", round(cross_r2,3))))

  #分割データの保存
  split_train <- c(split_train, list(d_train))
  split_test <- c(split_test, list(d_test))
  
  cross_train_first <- cross_train_first + 30
  cross_train_last <- cross_train_last + 30
  cross_test_first <- cross_test_first + 30
  cross_test_last <- cross_test_last + 30
}
split_test[9]
cross_result[8]
length(cross_result[7])

for (i in cross_rmse_list){
  print(i)
}








save_graph <- function(pred, graph_name){
  file_name <- paste0("/Users/e185716/グラフ練習/来客数予測/", graph_name, ".png")
  png(file_name, width = 800, height = 600)
  par(family = "HiraKakuProN-W3")
  par(oma = c(0, 0,0, 1.5))
  plot(pred$mean, type = "l", xlab="" ,ylab="Number of Customer")
  #lines(plot_ei$ei, col = "red")
  par(new = TRUE)
  plot(d_test$CustNum,col = "red", type = "l", axes = FALSE, xlab = "", ylab = "")
  box()
  legend("topleft", legend = c("prediction", "original"), col = c("black", "red"), lty = 1,bg = "transparent")
  dev.off()
}

save_graph(non_ei_pred, "non_ei_pred")
save_graph(first_ei_pred, "first_ei_pred")
save_graph(tenloop_ei_pred$mean, "tenloop_ei_pred")

save_graph(pred$mean, "tenloop_ei_pred")


ei_train_list[5]


new_ei_pred <- ei_inspection(event_train, event_test)
pre_ei_pred <- ei_inspection(d_train, d_test)

calculate_rmse <- function(predicted, actual) {
  n <- length(predicted)
  if (length(actual) != n) {
    stop("予測値と実際の値の長さが一致していません。")
  }
  
  squared_errors <- (predicted - actual) ^ 2
  mean_squared_error <- sum(squared_errors) / n
  rmse <- sqrt(mean_squared_error)
  return(rmse)
}

new_ei_pred$mean

new_rmse_result <- calculate_rmse(new_ei_pred$mean, event_test$CustNum)
pre_rmse_result <- calculate_rmse(pre_ei_pred$mean, read.csv('/Users/e185716/研究/R/ei_make/event_test.csv')$CustNum)

print(paste("イベントインパクト更新後RMSE", new_rmse_result, sep=" : "))
print(paste("イベントインパクト更新前RMSE", pre_rmse_result, sep=" : "))

#d_test2[,-2]
# プロット
#plot(pred,xlab = '', ylab = '')
plot(new_ei_pred, ylim = c(-10000, 10000), xlab = '', ylab = '')
plot(pre_ei_pred, ylim = c(0, 1300), xlab = '', ylab = '')

new_ei_pred$distribution

hyouka_ei_test <- function(event_kind, result_list, pred){
  idx <- which(event_test[,event_kind] == 1)
  rmse_result <- calculate_rmse(pred$mean[idx], event_test$CustNum[idx])
  result_list <- c(result_list, rmse_result)
  return(result_list)
}


# イベントごとの評価
hyouka_list <- c()
for (i in event_name) {
  hyouka_list <- hyouka_ei_test(i,hyouka_list,new_ei_pred)
}

pre_hyouka_list <- c()
for (i in event_name) {
  pre_hyouka_list <- hyouka_ei_test(i,pre_hyouka_list,pre_ei_pred)
}



# 各イベントのRMSEを出力
evaluation_print <- function(evaluation_list){
  j <- 1
  for (i in evaluation_list){
    hyouka <- paste(event_name[j], i, sep=" : ")
    print(hyouka)
    j <- j+1
  }
}

evaluation_print(hyouka_list)
evaluation_print(pre_hyouka_list)


for (i in event_name){
  idx <- which(event_test[,i] == 1)
  print(i)
  print("実測値")
  print(event_test$CustNum[idx])
  print("予測値")
  print(new_ei_pred$mean[idx])
  # print(pre_ei_pred$mean[idx])
  # print(paste("イベントあり", new_ei_pred$mean[idx], sep=" : "))
  # print(paste("イベントなし", event_test$CustNum[idx], sep=" : "))
}

plot(pred$mean, ylim = c(0, 600),type = 'l', xlab = '', ylab = '',col = 'red')
par(new = T)
#plot(x=c(0:400),d_train2$CustNum, ylim = c(0,1500), axes = F, type = 'l', col = 'red', lwd = 1, lty = 5, xlab = '', ylab = '')

plot(d_test$CustNum, ylim = c(0,1000), axes = F, type = 'l', lwd = 3, lty = 1, xlab = '', ylab = '')
#legend('topleft', legend = c('Predicted', 'Actual'), col = c('blue', 'red'), lty = 1, lwd = 3, cex = 1.5)

event_train[,"台風"]



#pred$distribution[,1]
result_dir <- paste0('/Users/e185716/研究/R/result/trend/test22')
if (dir.exists(result_dir) == FALSE){
  dir.create(paste0(result_dir))
}else {
  print("directory exist")
}
write.csv(x = pred$mean, file = paste0(result_dir,'/result_mean.csv'))
write.csv(x = pred$median, file = paste0(result_dir,'/result_median.csv'))
write.csv(x = pred$interval, file = paste0(result_dir,'/Interval.csv'))

