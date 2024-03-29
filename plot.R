event_name
idx <- which(event_test[,"雨"] == 1)
i <- event_name[2]
idx <- which(event_test[,i] == 1)


save_dir <- "/Users/e185716/グラフ"
graphics_file_path <- file.path(save_dir, "観光客数誤差_EI/観光客数誤差andEI_雨.png")
graphics_file_path

#list.dirs(getwd(), recursive = FALSE)

average_list <- c()
for (i in seq_along(gosa_list)) {
  heikin <- sum(gosa_list[i][idx,])/length(idx)
  average_list <- c(average_list, heikin)
}
kankou_error_plot <- data.frame(average_list,
                        ei = t(ei_list[idx[1],]))
colnames(kankou_error_plot) <- c("kankou_error", "ei")


png(graphics_file_path, width = 800, height = 600)
par(oma = c(0, 0,0, 1.5))
plot(kankou_error_plot$kankou_error, type = "l", xlab='Loop Count',ylab="Error")
#lines(plot_ei$ei, col = "red")
par(new = TRUE)
plot(kankou_error_plot$ei,col = "red", type = "l", axes = FALSE, xlab = "", ylab = "")
axis(side = 4, col = "red", col.axis = "red", las = 1, ylab="Event Impact")
mtext(side=4, text = "Event Impact", adj = 0.5, line = 2.5, col="red")
dev.off()

##################################################################################################
# 観光客数　EI プロット
idx <- which(event_test[,"雨"] == 1)
graphics_file_path <- file.path(save_dir, "観光客数_EI/観光客数andEI_雨.png")


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
par(oma = c(0, 0,0, 1.5))
plot(kankou_plot$kankou, type = "l", xlab="Loop Count", ylab="number of tourists")
# lines(plot_ei$ei, col = "red")
par(new = TRUE)
plot(kankou_plot$ei,col = "red", type = "l", axes = FALSE, xlab = "", ylab = "")
axis(side = 4, col = "red", col.axis = "red", las = 1, ylab="Event Impact")
mtext(side=4, text = "Event Impact", adj = 0.5, line = 2.3, col="red")
dev.off()

##################################################################################################
# 来客数　EI プロット
idx <- which(event_test[,"雨"] == 1)
graphics_file_path <- file.path(save_dir, "来客数_EI/来客数andEI_雨.png")

# average_list <- c()
# for (i in seq_along(kankou_pred)) {
#   heikin <- sum(kankou_pred[i][idx,])/length(idx)
#   average_list <- c(average_list, heikin)
# }
# kankou_plot <- data.frame(
#   average_list
# )

plot_custpred <- data.frame(kankou = t(pred_list[idx[1],]),
                      ei = t(ei_list[idx[1],]))
colnames(plot_custpred) <- c("CustNum", "ei")

png(graphics_file_path, width = 800, height = 600)
par(oma = c(0, 0,0, 1.5))
plot(plot_custpred$CustNum, type = "l", xlab="Loop Count", ylab="number of customers", las=1)
# lines(plot_ei$ei, col = "red")
par(new = TRUE)
plot(plot_custpred$ei,col = "red", type = "l", axes = FALSE, xlab = "", ylab = "")
axis(side = 4, col = "red", col.axis = "red", las = 1, ylab="Event Impact")
mtext(side=4, text = "Event Impact", adj = 0.5, line = 2.3, col="red")
dev.off()


# write.csv(kankou_pred, file = "観光客数予測.csv", row.names = FALSE)
# write.csv(pred_list, file = "来客数予測.csv", row.names = FALSE)
# write.csv(gosa_list, file = "観光客数予測誤差.csv", row.names = FALSE)
# write.csv(cust_error, file = "来客数予測誤差.csv", row.names = FALSE)
# write.csv(ei_list, file = "イベントインパクト変動.csv", row.names = FALSE)

