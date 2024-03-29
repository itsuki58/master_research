#d <- read.csv('/Users/e185716/研究/R/ebilab.csv')
d_train <- read.csv('/Users/e185716/研究/R/train.csv')
d_test <- read.csv('/Users/e185716/研究/R/test.csv')

#日付省く
d_train2 <- d_train[,-1] 
d_test2 <- d_test[,-1]

#sales消す
d_train3 = d_train2[,-1]
d_test3 = d_test2[,-1]

#kankou,tabelog消す
d_train3 = d_train3[,-2]
d_test3 = d_test3[,-2]
d_test3 = d_test3[,-2]
d_train3 = d_train3[,-2]

d_test3 = d_test3[,-5]
d_train3 = d_train3[,-5]

# 季節,トレンド,回帰成分.
library(bsts)
ss <- AddLocalLinearTrend(list(),d_train3$CustNum)
ss <- AddSeasonal(ss, d_train3$CustNum, nseasons = 7)
model <- bsts(CustNum~., state.specification = ss, niter=1000, data=d_train3)

# 予測 
#pred <- predict(model, burn=100, newdata=d_test3[,-2])
pred <- predict(model, burn=100, newdata=d_test3)


#d_test2[,-2]
# プロット
#plot(pred,xlab = '', ylab = '')
plot(pred, ylim = c(0, 1300), xlab = '', ylab = '')

#plot(pred, ylim = c(0, 1300))
plot(pred$mean, ylim = c(0, 1000),type = 'l', xlab = '', ylab = '',col = 'red')

par(new = T)
#plot(x=c(0:400),d_train2$CustNum, ylim = c(0,1500), axes = F, type = 'l', col = 'red', lwd = 1, lty = 5, xlab = '', ylab = '')

plot(d_test2$CustNum, ylim = c(0,1000), axes = F, type = 'l', lwd = 3, lty = 1, xlab = '', ylab = '')
#legend('topleft', legend = c('Predicted', 'Actual'), col = c('blue', 'red'), lty = 1, lwd = 3, cex = 1.5)

#pred$distribution[,1]

#csv保存
write.csv(x = pred$mean, file = '/Users/e185716/研究/R/result/result_mean.csv')
write.csv(x = pred$median, file = '/Users/e185716/研究/R/result/result_median.csv')
write.csv(x = pred$interval, file = '/Users/e185716/研究/R/result/Interval.csv')


d <- read.csv('https://raw.githubusercontent.com/ozt-ca/tjo.hatenablog.samples/master/r_samples/public_lib/jp/sample_dlm_season_trend.csv', sep='\t')
　
