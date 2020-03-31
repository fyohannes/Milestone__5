###################
### balance ploi
###################

bal1 <- lm(constituency_data$pop~woman_win, data = constituency_data)
bal2 <- lm(constituency_data$schools~woman_win, data = constituency_data)
bal3 <- lm(constituency_data$mosques~woman_win, data = constituency_data)
bal4 <- lm(constituency_data$pubs~woman_win, data = constituency_data)
bal5 <- lm(constituency_data$some_gambling~woman_win, data = constituency_data)
bal6 <- lm(constituency_data$civil_def~woman_win, data = constituency_data)
bal7 <- lm(constituency_data$pre_fem_doc~woman_win, data = constituency_data)
bal8 <- lm(constituency_data$age~woman_win, data = constituency_data)
bal9 <- lm(constituency_data$education~woman_win, data = constituency_data)
bal10 <- lm(constituency_data$motorcycle~woman_win, data = constituency_data)
bal11 <- lm(constituency_data$pre_male_doc~woman_win, data = constituency_data)



bal1 <- coeftest(bal1, vcov=vcovHC(bal1,type="HC0"))[2,4]
bal2 <- coeftest(bal2, vcov=vcovHC(bal2,type="HC0"))[2,4]
bal3 <- coeftest(bal3, vcov=vcovHC(bal3,type="HC0"))[2,4]
bal4 <- coeftest(bal4, vcov=vcovHC(bal4,type="HC0"))[2,4]
bal5 <- coeftest(bal5, vcov=vcovHC(bal5,type="HC0"))[2,4]
bal6 <- coeftest(bal6, vcov=vcovHC(bal6,type="HC0"))[2,4]
bal7 <- coeftest(bal7, vcov=vcovHC(bal7,type="HC0"))[2,4]
bal8 <- coeftest(bal8, vcov=vcovHC(bal8,type="HC0"))[2,4]
bal9 <- coeftest(bal9, vcov=vcovHC(bal9,type="HC0"))[2,4]
bal10 <- coeftest(bal10, vcov=vcovHC(bal10,type="HC0"))[2,4]
bal11 <- coeftest(bal11, vcov=vcovHC(bal11,type="HC0"))[2,4]


covs <- c("Population (#)", "Schools (#)", "Mosques (#)", "Pubs (#)",
          "Gambling (#)", "Civil defense (#)", "Female doctors (#)",
          "[Resp] Age", " [Resp] Education (%, high school)", "[Resp] Owns motorcycle (%)", "Male doctors (#)")
bp <- data.frame(cbind(covs, c(bal1, bal2, bal3, bal4, bal5, bal6, bal7, bal8, bal9, bal10, bal11)))
bp$V2 <- as.numeric(as.character(bp$V2))



p4 <- ggplot(bp, aes(x = V2, y = covs)) +
  geom_dotplot(binaxis = "y", stackgroups = TRUE, binwidth = 1, method = "histodot", dotsize = 0.1) + 
  geom_vline(xintercept=0.055, linetype="dashed", color = "red") + theme_minimal() + xlab("p-values") + theme(axis.title.y=element_blank()) +
  theme(text=element_text(size=10, family="Times")) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) + 
  theme(legend.title = element_blank()) + theme(legend.text=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("./outputs/figures/balance_plot.pdf", plot = p4,  width = 4.5, height = 3)