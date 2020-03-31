##center versus no center

#has no center

mod_fem1 <- lm(v744a_f2012 ~ woman_win, data = constituency_data[constituency_data$center == 0,])
mod_fem2 <- lm(v744b_f2012 ~ woman_win, data = constituency_data[constituency_data$center == 0,])
mod_fem3 <- lm(v744c_f2012 ~ woman_win, data = constituency_data[constituency_data$center == 0,])
mod_fem4 <- lm(v744d_f2012 ~ woman_win, data = constituency_data[constituency_data$center == 0,])
mod_fem5 <- lm(v744e_f2012 ~ woman_win, data = constituency_data[constituency_data$center == 0,])
mod_fem7 <- lm(add_index_f2012 ~ woman_win, data = constituency_data[constituency_data$center == 0,])

mod_fem1 <- coeftest(mod_fem1, vcov=vcovHC(mod_fem1,type="HC0"))
mod_fem2 <- coeftest(mod_fem2, vcov=vcovHC(mod_fem2,type="HC0"))
mod_fem3 <- coeftest(mod_fem3, vcov=vcovHC(mod_fem3,type="HC0"))
mod_fem4 <- coeftest(mod_fem4, vcov=vcovHC(mod_fem4,type="HC0"))
mod_fem5 <- coeftest(mod_fem5, vcov=vcovHC(mod_fem5,type="HC0"))
mod_fem7 <- coeftest(mod_fem7, vcov=vcovHC(mod_fem7,type="HC0"))


#has center

mod_fem_center1 <- lm(v744a_f2012 ~ woman_win, data = constituency_data[constituency_data$center == 1,])
mod_fem_center2 <- lm(v744b_f2012 ~ woman_win, data = constituency_data[constituency_data$center == 1,])
mod_fem_center3 <- lm(v744c_f2012 ~ woman_win, data = constituency_data[constituency_data$center == 1,])
mod_fem_center4 <- lm(v744d_f2012 ~ woman_win, data = constituency_data[constituency_data$center == 1,])
mod_fem_center5 <- lm(v744e_f2012 ~ woman_win, data = constituency_data[constituency_data$center == 1,])
mod_fem_center7 <- lm(add_index_f2012 ~ woman_win, data = constituency_data[constituency_data$center == 1,])

mod_fem_center1 <- coeftest(mod_fem_center1, vcov=vcovHC(mod_fem_center1,type="HC0"))
mod_fem_center2 <- coeftest(mod_fem_center2, vcov=vcovHC(mod_fem_center2,type="HC0"))
mod_fem_center3 <- coeftest(mod_fem_center3, vcov=vcovHC(mod_fem_center3,type="HC0"))
mod_fem_center4 <- coeftest(mod_fem_center4, vcov=vcovHC(mod_fem_center4,type="HC0"))
mod_fem_center5 <- coeftest(mod_fem_center5, vcov=vcovHC(mod_fem_center5,type="HC0"))
mod_fem_center7 <- coeftest(mod_fem_center7, vcov=vcovHC(mod_fem_center7,type="HC0"))

hte_dat = cbind(c(mod_fem1[2,1], mod_fem2[2,1],mod_fem3[2,1],mod_fem4[2,1],mod_fem5[2,1],mod_fem7[2,1], 
                  mod_fem_center1[2,1], mod_fem_center2[2,1],mod_fem_center3[2,1],mod_fem_center4[2,1],mod_fem_center5[2,1],mod_fem_center7[2,1]),
                c(mod_fem1[2,2], mod_fem2[2,2],mod_fem3[2,2],mod_fem4[2,2],mod_fem5[2,2],mod_fem7[2,2],
                  mod_fem_center1[2,2], mod_fem_center2[2,2],mod_fem_center3[2,2],mod_fem_center4[2,2],mod_fem_center5[2,2],mod_fem_center7[2,2])) %>%
  data.frame()
                            

hte_dat$loc <- c(rep("No", 6), rep("Yes", 6))

hte_dat$test <- c("(1) Goes out", "(2) Neglects children", "(3) Argues", "(4) Refuses sex", "(5) Burns food", "(6) Index")

hte_dat$test <- as.factor(hte_dat$test)

plot1 <- ggplot(hte_dat, aes(x=test, y = X1, group = loc)) +  
  geom_point(aes(shape = loc), size = 1.75, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=hte_dat$X1 - 1.96*hte_dat$X2, ymax = hte_dat$X1 + 1.96*hte_dat$X2),
                width = 0, color = "darkgrey", position=position_dodge(width=0.5)) +
  theme_test() + geom_hline(yintercept=0.0, linetype="dashed", color = "black") + ylim(-0.75, 0.75) +
  theme(axis.title.x=element_blank()) + ylab("Pr(Agree)") + theme(legend.title=element_blank()) +
  theme(text=element_text(size=10, family="Times")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(plot = plot1, filename = "./outputs/figures/figure_center_hte.pdf", height = 6, width = 8)


rm(mod_fem1, mod_fem2, mod_fem3, mod_fem4, mod_fem5, mod_fem7,
   mod_fem_center1, mod_fem_center2, mod_fem_center3, mod_fem_center4, mod_fem_center5, mod_fem_center7,
   hte_dat, plot1)



