d_train <- read.csv('/Users/e185716/研究/R/train.csv')
d_test <- read.csv('/Users/e185716/研究/R/test.csv')


ss <- AddLocalLinearTrend(list(), d_train$CustNum)
ss <- AddSeasonal(ss, d_train$CustNum, nseasons = 7)
model1 <- bsts(d_train$CustNum,
               state.specification = ss,
               niter = 1000)

#plot(model1, "comp")
#plot(model1, "resid")

# forecasting
pred <- predict(model1, horizon = 30)
#plot(pred1, plot.original = 156)
#plot(pred, ylim = c(0, 1300), xlab = '', ylab = '')
plot(pred,  xlab = '', ylab = '',xlim = c(0, 1300))

plot(pred$original.series,  xlab = '', ylab = '',type = 'l')
par(new = T)
plot(pred$mean,  xlab = '', ylab = '')


typeof(pred)

plot(pred$mean, ylim = c(0, 600),type = 'l', xlab = '', ylab = '',col = 'red')
par(new = T)
plot(d_test$CustNum, ylim = c(0,1000), axes = F, type = 'l', lwd = 3, lty = 1, xlab = '', ylab = '')

length(pred$mean)

