# install.packages("forecast")
# install.packages("tseries")

# ライブラリを読み込み
library(forecast)
library(tseries)


d_train <- read.csv('/Users/e185716/研究/R/ei_make/event_train.csv')
d_test <- read.csv('/Users/e185716/研究/R/ei_make/event_test.csv')


d_train$event_impact <- ei_train_list[,5]
d_test$event_impact <- ei_test_list[,5]


arima_model <- arima(d_train$CustNum, order=c(1,1,1))  # (p,d,q) = (1,1,1)のARIMAモデル
summary(arima_model)
forecast_values <- predict(arima_model, h=nrow(d_test))  # 12ステップ先の予測
forecast_values <- predict(arima_model)
forecast_values$pred





# データを読み込む（ここでは例として時系列データを読み込む）
data <- read.csv("your_data.csv")  # データを適切な方法で読み込む

# データの可視化や前処理（必要に応じて）
plot(data)

# 学習データとテストデータに分割
train_data <- window(data, start = c(年, 月), end = c(年, 月))  # 適切な期間を指定して学習データを作成
test_data <- window(data, start = c(年, 月), end = c(年, 月))   # 適切な期間を指定してテストデータを作成

# ARIMAモデルの適合
arima_model <- auto.arima(d_train$CustNum)


# テストデータの予測
forecast_result <- forecast(arima_model, h = length(d_test$CustNum))


# 予測結果の可視化
plot(forecast_result$mean)

# 予測結果の評価（例: 平均絶対誤差を計算）
accuracy(forecast_result, test_data)



d_train <- read.csv('/Users/e185716/研究/R/ei_make/event_train.csv')
d_test <- read.csv('/Users/e185716/研究/R/ei_make/event_test.csv')

train_test_list <- ei_step(d_train,d_test,100)
d_train <- train_test_list[[1]]
d_test <- train_test_list[[2]]

write.csv(x =d_train, file = "/Users/e185716/グラフ練習2/クロス/ei_100_train.csv")
write.csv(x =d_test, file = "/Users/e185716/グラフ練習2/クロス/ei_100_test.csv")
