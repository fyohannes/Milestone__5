#make the first stage graph
estimation_sample$gender <- as.numeric(as.character(estimation_sample$gender))
estimation_sample$woman_win <- as.numeric(as.character(estimation_sample$woman_win))


tdat <- estimation_sample %>% group_by(dapil_code) %>% summarise(mean(woman_win, na.rm=T),
                                                                 mean(gender == 0, na.rm=T))

plot_dat <- tdat %>% filter (`mean(woman_win, na.rm = T)` == 1 | `mean(woman_win, na.rm = T)` == 0) %>% 
  group_by(`mean(woman_win, na.rm = T)`) %>% summarise(mean(`mean(gender == 0, na.rm = T)`))

names(plot_dat) <- c("woman_win", "prop")

plot_dat[plot_dat$woman_win == 1,]$woman_win <- "Treatment"
plot_dat[plot_dat$woman_win == 0,]$woman_win <- "Control"

p10 <- ggplot(plot_dat, aes(x=as.factor(woman_win), y = prop)) + geom_bar(stat = "identity", fill = "darkgrey", color = "black") + theme_minimal() + ylab("Share of Seats Held by Women") +
  xlab("") + theme(text=element_text(size=10, family="Times")) 



ggsave("./outputs/figures/first_stage.pdf",plot=p10,  width = 4.5, height = 6)
