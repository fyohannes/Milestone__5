

islamists <- c("PARTAI KEADILAN SEJAHTERA", "PARTAI BULAN BINTANG", "PARTAI PERSATUAN PEMBANGUNAN")
###################
### balance ploi
###################

ebal1 <- lm(abs(win_margin)~woman_win, data = balance_elections_data_trim)
ebal2 <- lm(total_votes~woman_win, data = balance_elections_data_trim)
ebal3 <- lm(quota_seats~woman_win, data = balance_elections_data_trim)
ebal4 <- lm(remainder_votes~woman_win, data = balance_elections_data_trim)
ebal5 <- lm(as.numeric(order)~woman_win, data = balance_elections_data_trim)
ebal6 <- lm(as.numeric(party %in% islamists)~woman_win, data = balance_elections_data_trim)


ebal1 <- coeftest(ebal1, vcov=vcovHC(ebal1,type="HC0"))[2,4]
ebal2 <- coeftest(ebal2, vcov=vcovHC(ebal2,type="HC0"))[2,4]
ebal3 <- coeftest(ebal3, vcov=vcovHC(ebal3,type="HC0"))[2,4]
ebal4 <- coeftest(ebal4, vcov=vcovHC(ebal4,type="HC0"))[2,4]
ebal5 <- coeftest(ebal5, vcov=vcovHC(ebal5,type="HC0"))[2,4]
ebal6 <- coeftest(ebal6, vcov=vcovHC(ebal6,type="HC0"))[2,4]



covs <- c("Absolute Win Margin (Candidate)", "Party's Total Votes", "Party's First Round Seats",
          "Party's Remainder Votes", "Party List Order (Candidate)", "Islamist Party")
bp <- data.frame(cbind(covs, c(ebal1, ebal2, ebal3, ebal4, ebal5, ebal6)))
bp$V2 <- as.numeric(as.character(bp$V2))



p4 <- ggplot(bp, aes(x = V2, y = covs)) +
  geom_dotplot(binaxis = "y", stackgroups = TRUE, binwidth = 1, method = "histodot", dotsize = 0.1) + 
  geom_vline(xintercept=0.055, linetype="dashed", color = "red") + theme_minimal() + xlab("p-values") + theme(axis.title.y=element_blank()) +
  theme(text=element_text(size=10, family="Times")) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) + 
  theme(legend.title = element_blank()) + theme(legend.text=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("./outputs/figures/balance_plot_elections.pdf", plot = p4,  width = 4.5, height = 3)

rm(p4)