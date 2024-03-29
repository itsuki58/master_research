#d <- read.csv('/Users/e185716/研究/R/ebilab.csv')
d_train <- read.csv('/Users/e185716/研究/R/train/train22.csv')
d_test <- read.csv('/Users/e185716/研究/R/test/test22.csv')

#d_train <- read.csv('/Users/e185716/研究/R/train/2train5.csv')
#d_test <- read.csv('/Users/e185716/研究/R/test/2test5.csv')

d <- data.frame(V1=d_train['date'])
d <- data.frame(Index=d_train['date'])

# d_train <- data_make("train",d_train)
# d_test <- data_make("test",d_test)

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

d_test <- subset(d_test, select = setdiff(colnames(d_test), "event_impact"))
d_train <- subset(d_train, select = setdiff(colnames(d_train), "event_impact"))

d_train[!complete.cases(d_train),]

#d_train <- d_train[-1,]
d_train[is.na(d_train)] <- 0
print(d_train[!complete.cases(d_train),])

d_test[is.na(d_test)] <- 0
d_test[!complete.cases(d_test),]

# 季節,トレンド,回帰成分.
library(bsts)
ss <- AddLocalLinearTrend(list(),d_train$CustNum)
ss <- AddSeasonal(ss, d_train$CustNum, nseasons = 7)
# ss <- AddSeasonal(list(), d_train$CustNum, nseasons = 7)

model <- bsts(CustNum~., state.specification = ss, niter=1000, data=d_train)

# 予測 
#pred <- predict(model, burn=100, newdata=d_test3[,-2])
pred <- predict(model, burn=100, newdata=d_test)


#d_test2[,-2]
# プロット
#plot(pred,xlab = '', ylab = '')
plot(pred, ylim = c(0, 1300), xlab = '', ylab = '')

plot(pred$mean, ylim = c(0, 600),type = 'l', xlab = '', ylab = '',col = 'red')
par(new = T)
#plot(x=c(0:400),d_train2$CustNum, ylim = c(0,1500), axes = F, type = 'l', col = 'red', lwd = 1, lty = 5, xlab = '', ylab = '')

plot(d_test$CustNum, ylim = c(0,1000), axes = F, type = 'l', lwd = 3, lty = 1, xlab = '', ylab = '')
#legend('topleft', legend = c('Predicted', 'Actual'), col = c('blue', 'red'), lty = 1, lwd = 3, cex = 1.5)

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

#csv保存
#write.csv(x = pred$mean, file = '/Users/e185716/研究/R/result/t/test22/result_mean.csv')
#write.csv(x = pred$median, file = '/Users/e185716/研究/R/result/t/test22/result_median.csv')
#write.csv(x = pred$interval, file = '/Users/e185716/研究/R/result/t/test22/Interval.csv')

#write.csv(x = pred$mean, file = '/Users/e185716/研究/R/result/2test5/result_mean.csv')
#write.csv(x = pred$median, file = '/Users/e185716/研究/R/result/2test5/result_median.csv')
#write.csv(x = pred$interval, file = '/Users/e185716/研究/R/result/2test5/Interval.csv')


