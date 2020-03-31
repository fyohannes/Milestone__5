
#make smoothness plot

p9 <- ggplot(constituency_data, aes(x=win_margin)) + geom_histogram(bins = 80, colour="darkgrey", fill = "lightgrey") + theme_minimal() +
  geom_vline(xintercept=0.0, linetype="dashed", color = "black") + xlim(c(-0.05, 0.05)) + theme(axis.title.y=element_blank()) + 
  xlab("Female Margin of Victory for Last Seat Against Male Candidate (%)") + theme(text=element_text(size=10, family="Times")) 

ggsave("./outputs/figures/smoothness_plot.pdf", plot = p9,  width = 6, height = 4)