#graphical representation of RD
#see Functions.R for details of loess_plot() function.

#post treatment plot
plot1 <- loess_plot(x = constituency_data$win_margin, y = constituency_data$v744a_f2012, W = constituency_data$woman_win, ylab = "Goes out (Pr(Yes))")
plot2 <- loess_plot(x = constituency_data$win_margin, y = constituency_data$v744b_f2012, W = constituency_data$woman_win, ylab = "Neglects children (Pr(Yes))")
plot3 <- loess_plot(x = constituency_data$win_margin, y = constituency_data$v744c_f2012, W = constituency_data$woman_win, ylab = "Argues (Pr(Yes))")
plot4 <- loess_plot(x = constituency_data$win_margin, y = constituency_data$v744d_f2012, W = constituency_data$woman_win, ylab = "Refuses sex (Pr(Yes))")
plot5 <- loess_plot(x = constituency_data$win_margin, y = constituency_data$v744e_f2012, W = constituency_data$woman_win, ylab = "Burns food (Pr(Yes))")
plot6 <- loess_plot(x = constituency_data$win_margin, y = constituency_data$add_index_f2012, W = constituency_data$woman_win, ylab = "Index")



pdf("./outputs/figures/loess_plot.pdf")
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=2)
dev.off()
