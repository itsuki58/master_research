d_train <- read.csv('/Users/e185716/研究/R/ei_make/event_train.csv')
d_test <- read.csv('/Users/e185716/研究/R/ei_make/event_test.csv')


d_train <- event_train
d_test <- event_test

#イベントインパクト検証
ei_inspection <- function(train_data, test_data){
  d_train <- train_data
  d_test <- test_data
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
  return(pred)
}

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

