d <- read.csv('https://raw.githubusercontent.com/ozt-ca/tjo.hatenablog.samples/master/r_samples/public_lib/jp/sample_marketing.csv')
d_train <- d[1:84,]
d_test <- d[85:100,]
d_test
d_train
d_test[,-1]

d <- read.csv('https://raw.githubusercontent.com/ozt-ca/tjo.hatenablog.samples/master/r_samples/public_lib/jp/sample_dlm_season_trend.csv', sep='\t')

d
library(bsts)
ss <- AddLocalLinearTrend(list(),d_train$cv)
ss <- AddSeasonal(ss, d_train$cv, nseasons = 7)
model <- bsts(cv~., state.specification = ss, niter=1000, data=d_train)

d_test[,-1]


pred <- predict(model, burn=100, newdata=d_test[,-1])
pred

plot(pred, ylim = c(0, 1500),  xlab = '', ylab = '')
par(new = T)
plot(d_test$cv, ylim = c(0,1500), xlim = c(-84,16), axes = F, type = 'l', col = 'red', lwd = 3, lty = 1, xlab = '', ylab = '')
legend('topleft', legend = c('Predicted', 'Actual'), col = c('blue', 'red'), lty = 1, lwd = 3, cex = 1.5)



cal1 <- function(x,y){
  z <- x+y
  a <- x*y
  return(list(z,a)) 
}

result <- cal1(2,5)

a <- result[1]
a
