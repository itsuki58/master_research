diff_make <- function(target_data){
  lag_event <- diff(target_data[,"event_impact"])
  add_data <- data.frame(lag_event)
  names(add_data) <- c("lag_event")
  add_data <- rbind(0,add_data)
  lag_data <- cbind(target_data, add_data)
  lag_data <- subset(lag_data, select = setdiff(colnames(lag_data), "event_impact"))
  return(lag_data)
} 

learning <- function(train,test){
  train_data <- paste0('/Users/e185716/研究/R/train/',train,'.csv')
  test_data <- paste0('/Users/e185716/研究/R/test/',test,'.csv')

  d_train <- read.csv(train_data)
  d_test <- read.csv(test_data)
  
  d <- data.frame(V1=d_train['date'])
  d <- data.frame(Index=d_train['date'])
  
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
  
  
  #1階微分
  # d_train <- diff_make(d_train)
  # d_test <- diff_make(d_test)
  
  #イベントインパクト削除
  d_test <- subset(d_test, select = setdiff(colnames(d_test), "event_impact"))
  d_train <- subset(d_train, select = setdiff(colnames(d_train), "event_impact"))
  
  #if (train == "train1" | train == "2train1"){
   # d_train <- d_train[-1,] 
  #}
  #欠損値を０に
  d_train[is.na(d_train)] <- 0
  d_test[is.na(d_test)] <- 0
  print(d_train[!complete.cases(d_train),])
  d_test[!complete.cases(d_test),]
  
  library(bsts)
  ss <- AddLocalLinearTrend(list(),d_train$CustNum)
  ss <- AddSeasonal(ss, d_train$CustNum, nseasons = 7)
  # ss <- AddSeasonal(list(), d_train$CustNum, nseasons = 7)
  model <- bsts(CustNum~., state.specification = ss, niter=1000, data=d_train)
  
  pred <- predict(model, burn=100, newdata=d_test)
  
  #result_dir <- paste0('/Users/e185716/研究/R/result/',test)
  #result_dir <- paste0('/Users/e185716/研究/R/result/k/',test)# s:sales k:kankoukyaku t:tabelog e:eventimpact
  result_dir <- paste0('/Users/e185716/研究/R/result/trend/',test)# s:sales k:kankoukyaku t:tabelog e:eventimpact
  
    
  #結果を保存するディレクトリ作成
  if (dir.exists(result_dir) == FALSE){
    dir.create(paste0(result_dir))
  }
  write.csv(x = pred$mean, file = paste0(result_dir,'/result_mean.csv'))
  write.csv(x = pred$median, file = paste0(result_dir,'/result_median.csv'))
  write.csv(x = pred$interval, file = paste0(result_dir,'/Interval.csv'))
  
  d_train <- read.csv(train_data)
  d_test <- read.csv(test_data)
  write.csv(x = d_train, file = paste0(result_dir,'/train.csv'))
  write.csv(x = d_test, file = paste0(result_dir,'/test.csv'))
}


# training : 1年, test : 30日
for (i in 1:22){
  train <- paste0('train',i)
  test <- paste0('test',i)
  print(paste(train, test,sep = '-'))
  learning(train,test)
}

# training : 2年, test : 60日
for (i in 1:5){
  train <- paste0('2train',i)
  test <- paste0('2test',i)
  print(paste(train, test,sep = '-'))
  learning(train,test)
}

# training : 半年, test : 15日
for (i in 1:55){
  train <- paste0('182_15/train',i)
  test <- paste0('182_15/test',i)
  print(paste(train, test,sep = '-'))
  learning(train,test)
}

# イベントインパクトなし
for (i in 1:22){
  train <- paste0('train',i)
  test <- paste0('test',i)
  print(paste(train, test,sep = '-'))
  learning(train,test)
}
