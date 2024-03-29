# d_train <- read.csv('/Users/e185716/研究/R/train/train22.csv')
# d_test <- read.csv('/Users/e185716/研究/R/test/test22.csv')
# 
# d <- data.frame(V1=d_train['date'])
# d <- data.frame(Index=d_train['date'])
# 
# #日付省く
# d_train <- subset(d_train, select = setdiff(colnames(d_train), "date"))
# d_test <- subset(d_test, select = setdiff(colnames(d_test), "date"))
# 
# 
# #sales消す
# d_train <- subset(d_train, select = setdiff(colnames(d_train), "salesTotal"))
# d_test <- subset(d_test, select = setdiff(colnames(d_test), "salesTotal"))
# 
# #kankou,tabelog消す
# d_train <- subset(d_train, select = setdiff(colnames(d_train), "kankou"))
# d_test <- subset(d_test, select = setdiff(colnames(d_test), "kankou"))
# d_train <- subset(d_train, select = setdiff(colnames(d_train), "tabelog"))
# d_test <- subset(d_test, select = setdiff(colnames(d_test), "tabelog"))
# 
# #1階微分
# d_train <- diff_make(d_train)
# d_test <- diff_make(d_test)


diff_make <- function(target_data){
  lag_event <- diff(target_data[,"event_impact"])
  add_data <- data.frame(lag_event)
  names(add_data) <- c("lag_event")
  add_data <- rbind(0,add_data)
  lag_data <- cbind(target_data, add_data)
  lag_data <- subset(lag_data, select = setdiff(colnames(lag_data), "event_impact"))
  return(lag_data)
} 

data_make <- function(data_kinds,input_data){
  #日付省く
  input_data <- subset(input_data, select = setdiff(colnames(input_data), "date"))

  #sales消す
  input_data <- subset(input_data, select = setdiff(colnames(input_data), "salesTotal"))

  #kankou,tabelog消す
  input_data <- subset(input_data, select = setdiff(colnames(input_data), "kankou"))
  input_data <- subset(input_data, select = setdiff(colnames(input_data), "tabelog"))

  #1階微分
  input_data <- diff_make(input_data)

  if (data_kinds == "train"){
    input_data[!complete.cases(input_data),]
    input_data[is.na(input_data)] <- 0
  }else if (data_kinds == "test"){
    input_data[is.na(input_data)] <- 0
    input_data[!complete.cases(input_data),]
  }
  
  return(input_data)
}

# d_train <- data_make("train",d_train)
# d_test <- data_make("test",d_test)





kankou_leaning <- function(event_train, event_test){
  d_train <- event_train
  d_test <- event_test
  
  d <- data.frame(V1=d_train['date'])
  d <- data.frame(Index=d_train['date'])
  
  #日付省く
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "date"))
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "date"))
  
  
  #sales消す
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "salesTotal"))
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "salesTotal"))
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "CustNum"))
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "CustNum"))
  
  #kankou,tabelog消す
  # d_train <- subset(d_train, select = setdiff(colnames(d_train), "kankou"))
  # d_test <- subset(d_test, select = setdiff(colnames(d_test), "kankou"))
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "tabelog"))
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "tabelog"))
  
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "day1_tabelog"))
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "day1_tabelog"))
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "day1_sales"))
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "day1_sales"))
  
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "day1_kankou"))
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "day1_kankou"))
  
  # d_train <- subset(d_train, select = setdiff(colnames(d_train), "event_impact"))
  # d_test <- subset(d_test, select = setdiff(colnames(d_test), "event_impact"))
  
  d_train[!complete.cases(d_train),]
  
  #d_train <- d_train[-1,]
  d_train[is.na(d_train)] <- 0
  print(d_train[!complete.cases(d_train),])
  
  d_test[is.na(d_test)] <- 0
  d_test[!complete.cases(d_test),]
  
  d_train <- d_train[1:2]
  d_test <- d_test[1:2]
  
  
  # 季節,トレンド,回帰成分.
  library(bsts)
  # ss <- AddSeasonal(list(), d_train$day1_kankou, nseasons = 7)
  ss <- AddSeasonal(list(), d_train$kankou, nseasons = 7)
  
  
  # model <- bsts(day1_kankou~., state.specification = ss, niter=1000, data=d_train)
  model <- bsts(kankou~., state.specification = ss, niter=1000, data=d_train)
  
  
  # 予測 
  #pred <- predict(model, burn=100, newdata=d_test3[,-2])
  pred <- predict(model, burn=100, newdata=d_test)

  # gosa <- event_test$day1_kankou - pred$mean
  gosa <- event_test$kankou - pred$mean
  gosa_data <- as.data.frame(gosa)
  
  pred <- pred$mean
  #gosa_data <- gosa_data/10
  # return(gosa_data)
  # return(list(gosa_data, pred, d_test$day1_kankou))
  return(list(gosa_data, pred, d_test$kankou))
  
  }

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
  file_name <- paste0("/Users/e185716/グラフ練習/", csv_name, ".csv")
  
  write.csv(x = pred$mean, file = file_name)
  
  return(pred)
}

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



# ei_inspection <- function(train_data, test_data){
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
  
  # d_train <- subset(d_train, select = setdiff(colnames(d_train), "event_impact"))
  # d_test <- subset(d_test, select = setdiff(colnames(d_test), "event_impact"))
  
  d_train <- d_train[,1:5]
  d_test <- d_test[,1:5]
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

ei_step <- function(train, test, step){
  train$event_impact <- ei_train_list[,step]
  test$event_impact <- ei_test_list[,step]
  
  return(list(train, test))
}


cross_learn <- function(train_data, test_data, ei){
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
  return(pred)
}
