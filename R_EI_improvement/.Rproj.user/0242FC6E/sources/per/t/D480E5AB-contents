# n期先予測
library(dplyr)

d_train <- read.csv('/Users/e185716/研究/R/ei_make/event_train.csv')
d_test <- read.csv('/Users/e185716/研究/R/ei_make/event_test.csv')
date_list <- d_test$date
date_list

nrow(d_train)
nrow(d_test)

ei_step <- function(train, test, step){
  train$event_impact <- ei_train_list[,step]
  test$event_impact <- ei_test_list[,step]

  return(list(train, test))
}


shift_learn <- function(train_data, test_data, csv_name, ei){
  d_train <- train_data
  d_test <- test_data
  
  if (ei == "no"){
    # イベントインパクト消す
    d_train <- subset(d_train, select = setdiff(colnames(d_train), "event_impact"))
    d_test <- subset(d_test, select = setdiff(colnames(d_test), "event_impact"))
  }
  
  
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
  ss <- AddSeasonal(list(), d_train$CustNum, nseasons = 7)
  
  model <- bsts(CustNum~., state.specification = ss, niter=1000, data=d_train)
  
  # 予測 
  pred <- predict(model, burn=100, newdata=d_test)
  file_name <- paste0("/Users/e185716/グラフ練習2/", csv_name, ".csv")

  # write.csv(x = pred$mean, file = file_name)

  save_data <- data.frame(date_list, pred$mean)
  colnames(save_data) <- c("date", "prediction")
  write.csv(x = save_data, file = file_name)
  return(pred)
}


shift_train <-  read.csv('/Users/e185716/研究/R/ei_make/event_train.csv')
shift_test <- read.csv('/Users/e185716/研究/R/ei_make/event_test.csv')
original_shift_train <- shift_train[,c("CustNum", "event_impact")]
original_shift_test <- shift_test[,c("CustNum", "event_impact")]

# 7日前までの客数を変数に追加
for (i in 1:7) {
  # non_ei_pred <- non_ei_inspection(d_train, d_test, "non_ei_pred")
  new_column_name <- paste0(i, "_ago_cust")  # 新しい列名
  
  original_shift_test[new_column_name] <- lag(original_shift_test$CustNum, n=i, default = NA) # テストデータシフト
  original_shift_test[new_column_name][1:i,] <- tail(d_train$CustNum, i) # テストデータの最初に学習データに含まれていた値を代入
  
  original_shift_train[new_column_name] <- lag(original_shift_train$CustNum, n=i, default = NA) # 学習データシフト
}

col_names <- colnames(original_shift_train)

# イベントインパクトなし
shift_train <- original_shift_train
shift_test <- original_shift_test
for (i in 1:7){
  save_file_name <- paste0( "7期先/non_ei/pred_ahead_", i)
  shift_nonei_pred <- shift_learn(shift_train, shift_test, save_file_name, "no")
  
  # 予測値をテストデータに挿入
  shift_test <- cbind(shift_test[, 1:2], shift_nonei_pred$mean, shift_test[, 3:ncol(shift_test)])
  del_col_num <-  3+i
  shift_test <- shift_test[, -del_col_num]
  
  colnames(shift_test) <- col_names
}


# イベントインパクトt=0
shift_train <- original_shift_train
shift_test <- original_shift_test
for (i in 1:7){
  save_file_name <- paste0( "7期先/first_ei/pred_ahead_", i)
  shift_nonei_pred <- shift_learn(shift_train, shift_test, save_file_name, "yes")
  
  # 予測値をテストデータに挿入
  shift_test <- cbind(shift_test[, 1:2], shift_nonei_pred$mean, shift_test[, 3:ncol(shift_test)])
  del_col_num <-  3+i
  shift_test <- shift_test[, -del_col_num]
  colnames(shift_test) <- col_names
}

# イベントインパクトt=5
shift_train <- original_shift_train
shift_test <- original_shift_test
train_test_list <- ei_step(shift_train,shift_test,100)
shift_train <- train_test_list[[1]]
shift_test <- train_test_list[[2]]
for (i in 1:7){
  save_file_name <- paste0( "7期先/100_ei/pred_ahead_", i)
  shift_nonei_pred <- shift_learn(shift_train, shift_test, save_file_name, "yes")
  
  # 予測値をテストデータに挿入
  shift_test <- cbind(shift_test[, 1:2], shift_nonei_pred$mean, shift_test[, 3:ncol(shift_test)])
  del_col_num <-  3+i
  shift_test <- shift_test[, -del_col_num]
  colnames(shift_test) <- col_names
}

# イベントインパクトt=10
shift_train <- original_shift_train
shift_test <- original_shift_test
train_test_list <- ei_step(shift_train,shift_test,10)
shift_train <- train_test_list[[1]]
shift_test <- train_test_list[[2]]
for (i in 1:7){
  save_file_name <- paste0( "7期先/tenth_ei/pred_ahead_", i)
  shift_nonei_pred <- shift_learn(shift_train, shift_test, save_file_name, "yes")
  
  # 予測値をテストデータに挿入
  shift_test <- cbind(shift_test[, 1:2], shift_nonei_pred$mean, shift_test[, 3:ncol(shift_test)])
  del_col_num <-  3+i
  shift_test <- shift_test[, -del_col_num]
  colnames(shift_test) <- col_names
}


# 最適イベントインパクトによる7日先はuse_event_decideで



color_list <- rainbow(7)
color_list
"1 ahead prediction"
legend <- c("1 ahead prediction","2 ahead prediction","3 ahead prediction","4 ahead prediction","5 ahead prediction","6 ahead prediction", "original")

plot_ahead <- function(ei){
  # png("/Users/e185716/グラフ練習/n期先/non_ei/plot.png", width = 800, height = 600)
  save_file_name <- paste0("/Users/e185716/グラフ練習/n期先/",ei,"_ei/plot.png")
  png(save_file_name, width = 1200, height = 900)
  par(family = "HiraKakuProN-W3")
  par(oma = c(0, 0,0, 1.5))
  # par(mfrow = c(1, 6))
  for (i in 1:6){
    save_file_name <- paste0( "n期先/",ei,"_ei/pred_ahead_", i)
    plot_file_name <- paste0("/Users/e185716/グラフ練習/", save_file_name, ".csv")
    ahead_plot <- read.csv(plot_file_name)
    if (i > 1){
      par(new = TRUE)
      plot(ahead_plot$x, type = "l", xlab="", ylab="", 
           las=1, ylim = ylim, axes = FALSE, col=color_list[i])
    }else{
      ylim = c(0, max(ahead_plot$x))*1.01
      plot(ahead_plot$x, type = "l", xlab="", ylab="", 
           las=1, ylim = ylim, col=color_list[i])
    }
  }
  #lines(plot_ei$ei, col = "red")
  par(new = TRUE)
  plot(d_test$CustNum,col = "red", type = "l", axes = FALSE, xlab = "", ylab = "number of customers",cex.axis = 1.5, cex.lab = 1.5)
  box()
  legend("topleft", legend = legend, col = color_list, lty = 1,bg = "transparent", cex = 1.75)
  dev.off()
}

plot_ahead("non")
plot_ahead("first")
plot_ahead("fifth")
plot_ahead("tenth")



# png("/Users/e185716/グラフ練習/n期先/non_ei/plot.png", width = 800, height = 600)
png("/Users/e185716/グラフ練習/n期先/non_ei/plot.png", width = 1200, height = 900)
par(family = "HiraKakuProN-W3")
par(oma = c(0, 0,0, 1.5))
# par(mfrow = c(1, 6))
for (i in 1:7){
  save_file_name <- paste0( "7期先/non_ei/pred_ahead_", i)
  plot_file_name <- paste0("/Users/e185716/グラフ練習/", save_file_name, ".csv")
  ahead_plot <- read.csv(plot_file_name)
  if (i > 1){
    par(new = TRUE)
    plot(ahead_plot$x, type = "l", xlab="", ylab="", 
         las=1, ylim = ylim, axes = FALSE, col=color_list[i])
  }else{
    ylim = c(0, max(ahead_plot$x))*1.01
    plot(ahead_plot$x, type = "l", xlab="", ylab="", 
         las=1, ylim = ylim, col=color_list[i])
  }
}
#lines(plot_ei$ei, col = "red")
par(new = TRUE)
plot(d_test$CustNum,col = "red", type = "l", axes = FALSE, xlab = "", ylab = "number of customers",cex.axis = 1.5, cex.lab = 1.5)
box()
legend("topleft", legend = legend, col = color_list, lty = 1,bg = "transparent", cex = 1.75)
dev.off()



train_test_list <- ei_step(d_train, d_test, 10)
shift_train <- train_test_list[[1]]
shift_test <- train_test_list[[2]]

write.csv(x = shift_train, file = "/Users/e185716/研究/R/ei_step/ei_10_train.csv")
write.csv(x = shift_test, file = "/Users/e185716/研究/R/ei_step/ei_10_test.csv")
