
#placebo test looking at share of women who hold intolerant attitudes towards HIV victims

#formal test (not rendered in table):

hiv_mod1 <- lm(hiv_family ~ woman_win, data = constituency_data[abs(constituency_data$win_margin) < 0.01,])
hiv_mod1 <- coeftest(hiv_mod1, vcov=vcovHC(hiv_mod1,type="HC0"))
hiv_mod1

hiv_mod2 <- lm(hiv_teacher ~ woman_win, data = constituency_data[abs(constituency_data$win_margin) < 0.01,])
hiv_mod2 <- coeftest(hiv_mod2, vcov=vcovHC(hiv_mod2,type="HC0"))
hiv_mod2
#graphical representation:
plot1 <- loess_plot(x = constituency_data$win_margin, y = constituency_data$hiv_family, W = constituency_data$woman_win, ylab = "Would Care for Family Member with HIV ")

plot2 <- loess_plot(x = constituency_data$win_margin, y = constituency_data$hiv_teacher, W = constituency_data$woman_win, ylab = "Would Care for Family Member with HIV ")

p <- grid.arrange(plot1, plot2, ncol=2)

ggsave("./outputs/figures/irrelevant_outcome.pdf", plot = p, width = 12, height = 8)

rm(p, plot2, plot1, hiv_mod1, hiv_mod2)
