event_train <- read.csv('/Users/e185716/研究/R/ei_make/event_train.csv')
event_test <- read.csv('/Users/e185716/研究/R/ei_make/event_test.csv')

# イベントの種類を抽出
event_name = names(event_train)
event_name <- event_name[10:length(event_name)]
event_name #イベントの種類

# イベントがある日の合計日数
## テストデータ
all_event_count <- 0
for (i in event_name) {
  idx <- which(event_test[,i] == 1)
  all_event_count <- all_event_count + length(idx)
  print(paste(i, length(idx), sep=" : "))
}
## トレーニングデータ
for (i in event_name) {
  idx <- which(event_train[,i] == 1)
  print(paste(i, length(idx), sep=" : "))
}

# イベントインパクト初期値
event_val_list <- c(20,6,15,17,11,-3,18,-10,16,7,-5)
event_val_list


for (i in seq_along(event_name)) {
  print(paste(event_name[i], event_val_list[i], sep=" : "))
}

# イベントインパクト更新
event_decide <- function(gosa_data, event_test, event_train, event_val_list){
  # イベントインパクト列を0に
  event_train$event_impact <- 0
  event_test$event_impact <- 0
  # イベント１つずつ値を更新
  for (i in seq_along(event_name)){
    idx_test <- which(event_test[,event_name[i]] == 1)
    idx_train <- which(event_train[,event_name[i]] == 1)
    if(length(idx_test) == 0){
      event_train[idx_train,]$event_impact <- event_train[idx_train,]$event_impact + event_val_list[i]
    }else{
      new_ei <- mean(gosa_data[idx_test,])/10000
      event_val_list[i] <- event_val_list[i] + new_ei
      # event_val_list <- c(event_val_list)
      event_test[idx_test,]$event_impact <- event_test[idx_test,]$event_impact + event_val_list[i]
      event_train[idx_train,]$event_impact <- event_train[idx_train,]$event_impact + event_val_list[i]
    }
  }
  return(list(event_test, event_train, event_val_list))
}

event_train <- read.csv('/Users/e185716/研究/R/ei_make/event_train.csv')
event_test <- read.csv('/Users/e185716/研究/R/ei_make/event_test.csv')
nrow(event_test)

# event_data <- read.csv('/Users/e185716/研究/R/ei_make/event_train.csv')
# event_train <- event_data[1:600,]
# event_test <- event_data[601:nrow(event_data),]

abs_sum <- 10000
loop_count <- 0
# while(abs_sum > 550){
while(loop_count < 42){
  # EIの値の調整
  if(loop_count > 0){
    new_ei <- event_decide(gosa_data, event_test, event_train, event_val_list)
    event_test <- data.frame(new_ei[1])
    event_train <- data.frame(new_ei[2])
    event_val_list <- c(new_ei[3])[[1]]
  }
  event_val_list

  result <- kankou_leaning(event_train = event_train,event_test = event_test)
  gosa_data <- data.frame(result[1])
  # new_ei_pred <- ei_inspection(event_train, event_test)
  # cust_error_val <- event_test$CustNum - new_ei_pred$mean
  
  #全てのループの結果を保存
  if(loop_count == 0){
    # 観光客数
    # kankou_pred <- data.frame(result[2])
    # colnames(kankou_pred) <- NULL
    # gosa_list <- data.frame(result[1])  #誤差
    # colnames(gosa_list) <- NULL
    # ei_test_list <- data.frame(event_test$event_impact)  #EIの更新歴
    # colnames(ei_test_list) <- NULL
    # ei_train_list <- data.frame(event_train$event_impact)  #EIの更新歴
    # colnames(ei_train_list) <- NULL
    # event_val_list2 <- data.frame(event_val_list)  #EIの更新歴
    # colnames(event_val_list2) <- NULL
    # 来客数
    # pred_list <- data.frame(new_ei_pred$mean) #来客数予測値
    # colnames(pred_list) <- NULL
    # cust_error <- data.frame(cust_error_val) #来客数誤差
    # colnames(cust_error) <- NULL
  }else{
    kankou_pred <- cbind(kankou_pred,result[2])
    colnames(kankou_pred) <- NULL
    gosa_list <- cbind(gosa_list,result[1])
    colnames(gosa_list) <- NULL
    ei_test_list <- cbind(ei_test_list,event_test$event_impact)
    colnames(ei_test_list) <- NULL
    ei_train_list <- cbind(ei_train_list,event_train$event_impact)
    colnames(ei_train_list) <- NULL
    event_val_list2 <- cbind(event_val_list2,event_val_list)
    colnames(event_val_list2) <- NULL
    # 来客数
    # pred_list <- cbind(pred_list,new_ei_pred$mean)
    # colnames(pred_list) <- NULL
    # cust_error<- cbind(cust_error,cust_error_val)
    # colnames(cust_error) <- NULL
  }
  
  
  # 誤差の絶対値平均
  # abs_gosa <- mean(abs(gosa_data)$gosa)
  # print(abs_gosa)
  
  abs_sum <- 0
  for (i in event_name) {
    idx <- which(event_test[,i] == 1)
    abs_sum <- abs_sum + sum(gosa_data[idx,])
  }
  abs_sum <- abs_sum/all_event_count
  print(abs_sum)
  
  loop_count <- loop_count + 1
  print(paste('number of loop :',loop_count))
}

for (i in seq_along(event_name)) {
  print(paste(event_name[i], event_val_list[i], sep=" : "))
}


# 学習後のイベントインパクトの値
## トレーニングデータ
for (i in event_name) {
  idx <- which(event_train[,i] == 1)
  output <- paste(i, event_train$event_impact[idx[1]], sep=" : ")
  print(output)
}

## テストデータ
for (i in event_name) {
  idx <- which(event_test[,i] == 1)
  output <- paste(i, event_test$event_impact[idx[1]], sep=" : ")
  print(output)
}

event_name
idx <- which(event_test[,"GW"] == 1)
event_test[idx,]
event_train[idx,]

idx <- which(event_train[,"お盆"] == 1)
idx[2]
event_train$event_impact[idx[3]]


# 結果のプロット
result_data <- as.data.frame(result[2],result[3])

plot(1:length(result[3]), result[3], xlab = '', ylab = '')


result[2]
result[3]

nrow(plot_data)
plot_data <- data.frame(t(gosa_list[1,]))

plot_data
idx <- which(event_test[,"雨"] == 1)
# new_ei <- mean(gosa_data[idx,])

loop_count = 0
for (i in idx) {
  if(loop_count == 0){
    plot_data <- data.frame(t(gosa_list[i,]))
    plot(plot_data[,1], type="l")
  }else{
    par(new=T)
    plot_data <- data.frame(t(gosa_list[i,])) 
    plot(plot_data[,1], type="l")
  }
  loop_count <- loop_count + 1
}
plot_ei <- data.frame(t(ei_test_list[1,]))

par(new=T)
plot(plot_ei[,1], type="l",col = "red")
plot()
#############################################
idx <- which(event_test[,"お盆"] == 1)
idx <- idx[1]
plot_data <- data.frame(t(cust_error[idx,]))
idx[1]

plot_ei <- data.frame(gosa = t(cust_error[idx[1],]),
                   row.names = t(ei_test_list[idx[1],]))

plot_ei <- data.frame(gosa = t(cust_error[idx[1],]),
                      ei = t(ei_test_list[idx[1],]))

ei_test_list[idx,]

plot_ei
plot(plot_ei, type="l",col = "red")
plot(t(ei_test_list[idx[1],]), t(gosa_list[idx[1],]), type = "l", xlab="Event Impact", ylab="CustNum")

##########################################################################################
idx <- which(event_test[,"お盆"] == 1)
idx <- idx[1]

#RMSE計算
calculate_rmse <- function(predicted, actual) {
  mse <- mean((predicted - actual)^2)
  rmse_value <- sqrt(mse)
  return(rmse_value)
}

pred_rmse_loopnum <- 0
for(i in seq_along(pred_list)){
  rmse_results <- mapply(calculate_rmse, pred_list[[i]], event_test$CustNum) #計算
  # データフレーム格納
  if(pred_rmse_loopnum == 0){
    rmse_list <- data.frame(RMSE = rmse_results)
    colnames(rmse_list) <- NULL
  }
  else{
    rmse_list <- cbind(rmse_list,rmse_results)
    colnames(rmse_list) <- NULL
  }
  pred_rmse_loopnum <- pred_rmse_loopnum + 1
}


plot_ei <- data.frame(t(rmse_list[idx,]))
plot(plot_ei$X37, type='l',ylab="RMSE")

plot_ei

par(new=T)
axis(side = 4)
plot(t(ei_test_list[idx,]), type="l",col = "red")

plot_ei <- data.frame(
  t(rmse_list[idx,]),
  t(ei_test_list[idx,])
  )
colnames(plot_ei) <- c("rmse", "ei")

rmse_val  <- calculate_rmse(pred_list[[1]][idx], event_test$CustNum[idx])

rmse_list[1][idx,]

### EIとRMSE
plot_ei$rmse
plot(plot_ei$rmse, type = "l", ylab="RMSE")
lines(plot_ei$ei, col = "red")
par(new = TRUE)
plot(plot_ei$ei,col = "red", type = "l", axes = FALSE, xlab = "", ylab = "")
axis(side = 4, col = "red", col.axis = "red", las = 1, ylab="Event Impact")
mtext(side=4, text = "Event Impact", adj = 1, line = 0, col="red")


### EIと誤差
idx <- which(event_test[,"お盆"] == 1)
mylist <- c()
for (i in seq_along(gosa_list)) {
  heikin <- sum(gosa_list[i][idx,])/length(idx)
  mylist <- c(mylist, heikin)
}
mylist
gosa_plot <- data.frame(
  mylist
)

mean(event_test$kankou[idx])

plot(gosa_plot$mylist, type = "l", ylab="Error")
lines(plot_ei$ei, col = "red")
par(new = TRUE)
plot(plot_ei$ei,col = "red", type = "l", axes = FALSE, xlab = "", ylab = "")
axis(side = 4, col = "red", col.axis = "red", las = 1, ylab="Event Impact")
mtext(side=4, text = "Event Impact", adj = 1, line = 0, col="red")





idx <- which(event_test[,"お盆"] == 1)


gosa_plot <- data.frame(
  t(cust_error[idx,])
)

new_ei_pred

t(pred_list[idx,])
t(event_test[idx,]$CustNum)
gosa_plot <- data.frame(
  t(pred_list[idx,]),
  t(event_test[idx,]$CustNum)
)

plot(gosa_plot$X37, type = "l", ylab="pred")
par(new = TRUE)
plot(gosa_plot$X1, col = "red", type = "l", axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)


plot(gosa_plot$X37, type = "l", ylab="Error")
par(new = TRUE)
plot(gosa_plot$X38, col = "red", type = "l", axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)
plot(gosa_plot$X39, col = "blue", type = "l", axes = FALSE, xlab = "", ylab = "")

ei_test_list[,4][idx]



kankou_pred <- cbind(kankou_pred,result[2])
colnames(kankou_pred) <- NULL
gosa_list <- cbind(gosa_list,result[1])
colnames(gosa_list) <- NULL
ei_test_list <- cbind(ei_test_list,event_test$event_impact)
colnames(ei_test_list) <- NULL
ei_train_list <- cbind(ei_train_list,event_train$event_impact)
colnames(ei_train_list) <- NULL
  event_val_list2 <- cbind(event_val_list2,event_val_list)
colnames(event_val_list2) <- NULL


write.csv(x = kankou_pred, file ='kankou_pred.csv')
write.csv(x = gosa_list, file ='gosa_list.csv')
write.csv(x = ei_test_list, file ='ei_test_list.csv')
write.csv(x = ei_train_list, file ='ei_train_list.csv')
write.csv(x = event_val_list2, file ='event_val_list2.csv')



