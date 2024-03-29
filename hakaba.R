## set_ei
# gw_idx <- which(event_test$GW == 1)
# bon_idx <- which(event_test$お盆 == 1)
# event_idx <- which(event_test$イベント == 1)
# xmas_idx <- which(event_test$クリスマス == 1)
# three_idx <- which(event_test$三連休 == 1)
# road_idx <- which(event_test$交通規制 == 1)
# newyear_idx <- which(event_test$元日 == 1)
# pre_newyear_idx <- which(event_test$大晦日 == 1)
# typhoon_idx <- which(event_test$台風 == 1)
# holiday_idx <- which(event_test$祝日 == 1)
# rain_idx <- which(event_test$雨 == 1)
# 
# gosa_data[rain_idx,]

# event_test <- set_new_ei_test("GW")
# event_test <- set_new_ei_test("お盆")
# event_test <- set_new_ei_test("イベント")
# event_test <- set_new_ei_test("クリスマス")
# event_test <- set_new_ei_test("三連休")
# event_test <- set_new_ei_test("交通規制")
# event_test <- set_new_ei_test("元日")
# event_test <- set_new_ei_test("大晦日")
# event_test <- set_new_ei_test("祝日")
# event_test <- set_new_ei_test("台風")
# event_test <- set_new_ei_test("雨")


# event_train <- set_new_ei_train("GW")
# event_train <- set_new_ei_train("お盆")
# event_train <- set_new_ei_train("イベント")
# event_train <- set_new_ei_train("クリスマス")
# event_train <- set_new_ei_train("三連休")
# event_train <- set_new_ei_train("交通規制")
# event_train <- set_new_ei_train("元日")
# event_train <- set_new_ei_train("大晦日")
# event_train <- set_new_ei_train("祝日")
# event_train <- set_new_ei_train("台風")
# event_train <- set_new_ei_train("雨")


# 11/20
# Test dataのイベントインパクト更新
set_new_ei_test <- function(event_kind){
  idx <- which(event_test[,event_kind] == 1)
  if(length(idx) > 0){
    new_ei <- mean(gosa_data[idx,]) #平均が違う
    event_test[idx,]$event_impact <- event_test[idx,]$event_impact + new_ei/10000
    return(event_test)
  }
  else{return(event_test)}
}　

# Training dataのイベントインパクト更新
set_new_ei_train <- function(event_kind){
  idx <- which(event_test[,event_kind] == 1)
  idx2 <- which(event_train[,event_kind] == 1)
  if(length(idx) > 0){
    new_ei <- mean(gosa_data[idx,])
    event_train[idx2,]$event_impact <- event_train[idx2,]$event_impact + new_ei/10000
    return(event_train)
  }
  else{return(event_train)}
}

while(loop_count < 5){
  # EIの値の調整
  # if(loop_count > 0){
  #   for (i in event_name) {
  #     event_test <- set_new_ei_test(i)
  #   }
  #   for (i in event_name) {
  #     event_train <- set_new_ei_train(i)
  #   }
  
  # }
  
}

