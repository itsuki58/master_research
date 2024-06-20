# set_eiで保存したイベントインパクト、観光客数予測、来客数予測のプロット

event_name
idx <- which(event_test[,"雨"] == 1)
i <- event_name[1]
idx <- which(event_test[,i] == 1)
idx

save_dir <- "/Users/e185716/グラフ練習2"
for (event_kind in event_name) {
  idx <- which(event_test[,event_kind] == 1)
  if (length(idx) > 0) {
    file_name <- paste0("観光客数誤差_EI/観光客数誤差andEI_", event_kind, ".png")
    graphics_file_path <- file.path(save_dir, file_name)
    graphics_file_path
    
    #list.dirs(getwd(), recursive = FALSE)
    
    average_list <- c()
    for (i in seq_along(gosa_list)) {
      heikin <- sum(gosa_list[i][idx,])/length(idx)
      average_list <- c(average_list, heikin)
    }
    kankou_error_plot <- data.frame(average_list,
                                    ei = t(ei_test_list[idx[1],]))
    colnames(kankou_error_plot) <- c("kankou_error", "ei")
    
    
    png(graphics_file_path, width = 800, height = 600)
    par(family = "HiraKakuProN-W3")
    par(oma = c(0, 0,0, 1.5))
    plot(kankou_error_plot$kankou_error, type = "l", xlab='Loop Count',ylab="Error", main=event_kind)
    #lines(plot_ei$ei, col = "red")
    par(new = TRUE)
    plot(kankou_error_plot$ei,col = "red", type = "l", axes = FALSE, xlab = "", ylab = "")
    axis(side = 4, col = "red", col.axis = "red", las = 1, ylab="Event Impact")
    mtext(side=4, text = "Event Impact", adj = 0.5, line = 2.5, col="red")
    dev.off()
  }
}



#############################################################################################
# 観光客数　EI プロット
idx <- which(event_test[,"雨"] == 1)

for (event_kind in event_name) {
  idx <- which(event_test[,event_kind] == 1)
  if (length(idx) > 0) {
    file_name <- paste0("観光客数_EI/観光客数andEI_", event_kind, ".png")
    graphics_file_path <- file.path(save_dir, file_name)
    
    
    average_list <- c()
    for (i in seq_along(kankou_pred)) {
      heikin <- sum(kankou_pred[i][idx,])/length(idx)
      average_list <- c(average_list, heikin)
    }
    
    kankou_plot <- data.frame(average_list,
                              ei = t(ei_list[idx[1],]))
    colnames(kankou_plot) <- c("kankou", "ei")
    
    # plot_ei <- data.frame(kankou = t(kankou_pred[idx[1],]),
    #                       ei = t(ei_list[idx[1],]))
    # colnames(plot_ei) <- c("kankou", "ei")
    
    png(graphics_file_path, width = 800, height = 600)
    par(family = "HiraKakuProN-W3")
    par(oma = c(0, 0,0, 1.5))
    plot(kankou_plot$kankou, type = "l", xlab="Loop Count", ylab="number of tourists", main=event_kind)
    # lines(plot_ei$ei, col = "red")
    par(new = TRUE)
    plot(kankou_plot$ei,col = "red", type = "l", axes = FALSE, xlab = "", ylab = "")
    axis(side = 4, col = "red", col.axis = "red", las = 1, ylab="Event Impact")
    mtext(side=4, text = "Event Impact", adj = 0.5, line = 2.3, col="red")
    dev.off()
  }
}


#############################################################################################
# 来客数　EI プロット
idx <- which(event_test[,"雨"] == 1)

for (event_kind in event_name) {
  idx <- which(event_test[,event_kind] == 1)
  if (length(idx) > 0) {
    file_name <- paste0("来客数_EI/来客数andEI_", event_kind, ".png")
    graphics_file_path <- file.path(save_dir, file_name)
    
    plot_custpred <- data.frame(kankou = t(pred_list[idx[1],]),
                                ei = t(ei_list[idx[1],]))
    colnames(plot_custpred) <- c("CustNum", "ei")
    
    actual_value <- event_test$CustNum[idx[1]]
    
    if (actual_value >= max(plot_custpred$CustNum)){
      y_lim <- c(min(plot_custpred$CustNum),actual_value)
    } else if (actual_value <= min(plot_custpred$CustNum)){
      y_lim <- c(event_test$CustNum[idx[1]],max(plot_custpred$CustNum))
    } else{
      y_lim <- c(min(plot_custpred$CustNum),max(plot_custpred$CustNum))
    }
    
    png(graphics_file_path, width = 800, height = 600)
    par(family = "HiraKakuProN-W3") #タイトルを日本語表記できるように
    par(oma = c(0, 0,0, 1.5)) #グラフサイズ
    plot(plot_custpred$CustNum, type = "l", xlab="Loop Count", ylab="number of customers", 
         las=1,ylim = y_lim, main=event_kind)
    abline(h = event_test$CustNum[idx[1]], col = "blue", lty = 2) #実測値
    par(new = TRUE)
    plot(plot_custpred$ei,col = "red", type = "l", axes = FALSE, xlab = "", ylab = "")
    axis(side = 4, col = "red", col.axis = "red", las = 1, ylab="Event Impact")
    mtext(side=4, text = "Event Impact", adj = 0.5, line = 2.3, col="red")
    dev.off()
  }
}




