#d <- read.csv('/Users/e185716/研究/R/ebilab.csv')
event_train <- read.csv('/Users/e185716/研究/R/ei_make/event_train.csv')
event_test <- read.csv('/Users/e185716/研究/R/ei_make/event_test.csv')

#d_train <- read.csv('/Users/e185716/研究/R/train/2train5.csv')
#d_test <- read.csv('/Users/e185716/研究/R/test/2test5.csv')

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
d_train <- subset(d_train, select = setdiff(colnames(d_train), "kankou"))
d_test <- subset(d_test, select = setdiff(colnames(d_test), "kankou"))
d_train <- subset(d_train, select = setdiff(colnames(d_train), "tabelog"))
d_test <- subset(d_test, select = setdiff(colnames(d_test), "tabelog"))

d_train <- subset(d_train, select = setdiff(colnames(d_train), "day1_tabelog"))
d_test <- subset(d_test, select = setdiff(colnames(d_test), "day1_tabelog"))
d_train <- subset(d_train, select = setdiff(colnames(d_train), "day1_sales"))
d_test <- subset(d_test, select = setdiff(colnames(d_test), "day1_sales"))

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
#ss <- AddLocalLinearTrend(list(),d_train$day1_kankou)
#ss <- AddSeasonal(ss, d_train$day1_kankou, nseasons = 7)
ss <- AddSeasonal(list(), d_train$day1_kankou, nseasons = 7)

model <- bsts(day1_kankou~., state.specification = ss, niter=1000, data=d_train)

# 予測 
#pred <- predict(model, burn=100, newdata=d_test3[,-2])
pred <- predict(model, burn=100, newdata=d_test)

gosa <- d_test$day1_kankou - pred$mean
gosa_data <- as.data.frame(gosa)

gosa_data <- gosa_data/10


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


