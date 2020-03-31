
#Define range of potential bandwidths
sequence=seq(from=0.001, to=0.02, by=0.0005)

#Create empty variables
confint1=rep(NA, length(sequence))
confint2=rep(NA, length(sequence))
confint3=rep(NA, length(sequence))
confint4=rep(NA, length(sequence))
coef1=rep(NA, length(sequence))


#This loop calculates the estimates
for(i in 1:length(sequence)){
  band=constituency_data[which(abs(constituency_data$win_margin)<sequence[i]),]
  model1 <- lm(v744a_f2012 ~ woman_win, data = band)
  model1 <- coeftest(model1, vcov=vcovHC(model1,type="HC0"))
  
  confint1[i]=model1[2,1] - model1[2,2]*1.96
  confint2[i]=model1[2,1] + model1[2,2]*1.96
  confint3[i]=model1[2,1] - model1[2,2]*1.67
  confint4[i]=model1[2,1] + model1[2,2]*1.67
  coef1[i]=model1[2,1]
}

df <- data.frame(sequence, coef1, confint1, confint2, confint3, confint4)

pa <- ggplot(df, aes(x=sequence, y=coef1)) + geom_point(color = "black") + ylim(-0.2, 0.2) + theme_minimal() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + geom_linerange(ymin = confint1, ymax=confint2, color = "lightgrey") + 
  geom_linerange(ymin = confint3, ymax=confint4, color = "darkgrey") + 
  ylab("Goes out") + xlab("Bandwidth (%)") + scale_x_continuous() + theme(text=element_text(size=10, family="Times"))


##second outcome
#Create empty variables
confint1=rep(NA, length(sequence))
confint2=rep(NA, length(sequence))
confint3=rep(NA, length(sequence))
confint4=rep(NA, length(sequence))
coef1=rep(NA, length(sequence))

#This loop calculates the estimates
for(i in 1:length(sequence)){
  band=constituency_data[which(abs(constituency_data$win_margin)<sequence[i]),]
  model1 <- lm(v744b_f2012 ~ woman_win, data = band)
  model1 <- coeftest(model1, vcov=vcovHC(model1,type="HC0"))
  
  confint1[i]=model1[2,1] - model1[2,2]*1.96
  confint2[i]=model1[2,1] + model1[2,2]*1.96
  confint3[i]=model1[2,1] - model1[2,2]*1.67
  confint4[i]=model1[2,1] + model1[2,2]*1.67
  coef1[i]=model1[2,1]
}

df <- data.frame(sequence, coef1, confint1, confint2, confint3, confint4)

pb <- ggplot(df, aes(x=sequence, y=coef1)) + geom_point(color = "black") + ylim(-0.2, 0.2) + theme_minimal() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + geom_linerange(ymin = confint1, ymax=confint2, color = "lightgrey") + 
  geom_linerange(ymin = confint3, ymax=confint4, color = "darkgrey") + 
  ylab("Neglects children") + xlab("Bandwidth (%)") + scale_x_continuous() + theme(text=element_text(size=10, family="Times"))



##third outcome
#Create empty variables
confint1=rep(NA, length(sequence))
confint2=rep(NA, length(sequence))
confint3=rep(NA, length(sequence))
confint4=rep(NA, length(sequence))
coef1=rep(NA, length(sequence))

#This loop calculates the estimates
for(i in 1:length(sequence)){
  band=constituency_data[which(abs(constituency_data$win_margin)<sequence[i]),]
  model1 <- lm(v744c_f2012 ~ woman_win, data = band)
  model1 <- coeftest(model1, vcov=vcovHC(model1,type="HC0"))
  
  confint1[i]=model1[2,1] - model1[2,2]*1.96
  confint2[i]=model1[2,1] + model1[2,2]*1.96
  confint3[i]=model1[2,1] - model1[2,2]*1.67
  confint4[i]=model1[2,1] + model1[2,2]*1.67
  coef1[i]=model1[2,1]
}

df <- data.frame(sequence, coef1, confint1, confint2, confint3, confint4)

pc <- ggplot(df, aes(x=sequence, y=coef1)) + geom_point(color = "black") + ylim(-0.1, 0.1) + theme_minimal() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + geom_linerange(ymin = confint1, ymax=confint2, color = "lightgrey") + 
  geom_linerange(ymin = confint3, ymax=confint4, color = "darkgrey") + 
  ylab("Argues") + xlab("Bandwidth (%)") + scale_x_continuous() + theme(text=element_text(size=10, family="Times"))



#fourth outcome
#Create empty variables
confint1=rep(NA, length(sequence))
confint2=rep(NA, length(sequence))
confint3=rep(NA, length(sequence))
confint4=rep(NA, length(sequence))
coef1=rep(NA, length(sequence))
#This loop calculates the estimates

for(i in 1:length(sequence)){
  band=constituency_data[which(abs(constituency_data$win_margin)<sequence[i]),]
  model1 <- lm(v744d_f2012 ~ woman_win, data = band)
  model1 <- coeftest(model1, vcov=vcovHC(model1,type="HC0"))
  
  confint1[i]=model1[2,1] - model1[2,2]*1.96
  confint2[i]=model1[2,1] + model1[2,2]*1.96
  confint3[i]=model1[2,1] - model1[2,2]*1.67
  confint4[i]=model1[2,1] + model1[2,2]*1.67
  coef1[i]=model1[2,1]
}

df <- data.frame(sequence, coef1, confint1, confint2, confint3, confint4)

pd <- ggplot(df, aes(x=sequence, y=coef1)) + geom_point(color = "black") + ylim(-0.15, 0.15) + theme_minimal() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + geom_linerange(ymin = confint1, ymax=confint2, color = "lightgrey") + 
  geom_linerange(ymin = confint3, ymax=confint4, color = "darkgrey") + 
  ylab("Refuses sex") + xlab("Bandwidth (%)") + scale_x_continuous() + theme(text=element_text(size=10, family="Times"))



#fifth outcome
#Create empty variables
confint1=rep(NA, length(sequence))
confint2=rep(NA, length(sequence))
confint3=rep(NA, length(sequence))
confint4=rep(NA, length(sequence))
coef1=rep(NA, length(sequence))

#This loop calculates the estimates
for(i in 1:length(sequence)){
  band=constituency_data[which(abs(constituency_data$win_margin)<sequence[i]),]
  model1 <- lm(v744e_f2012 ~ woman_win, data = band)
  model1 <- coeftest(model1, vcov=vcovHC(model1,type="HC0"))
  
  confint1[i]=model1[2,1] - model1[2,2]*1.96
  confint2[i]=model1[2,1] + model1[2,2]*1.96
  confint3[i]=model1[2,1] - model1[2,2]*1.67
  confint4[i]=model1[2,1] + model1[2,2]*1.67
  coef1[i]=model1[2,1]
}

df <- data.frame(sequence, coef1, confint1, confint2, confint3, confint4)

pe <- ggplot(df, aes(x=sequence, y=coef1)) + geom_point(color = "black") + ylim(-0.1, 0.1) + theme_minimal() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + geom_linerange(ymin = confint1, ymax=confint2, color = "lightgrey") + 
  geom_linerange(ymin = confint3, ymax=confint4, color = "darkgrey") + 
  ylab("Burns food") + xlab("Bandwidth (%)") + scale_x_continuous() + theme(text=element_text(size=10, family="Times"))



#sxith outcome
#Create empty variables
confint1=rep(NA, length(sequence))
confint2=rep(NA, length(sequence))
confint3=rep(NA, length(sequence))
confint4=rep(NA, length(sequence))
coef1=rep(NA, length(sequence))

#This loop calculates the estimates
for(i in 1:length(sequence)){
  band=constituency_data[which(abs(constituency_data$win_margin)<sequence[i]),]
  model1 <- lm(add_index_f2012 ~ woman_win, data = band)
  model1 <- coeftest(model1, vcov=vcovHC(model1,type="HC0"))
  
  confint1[i]=model1[2,1] - model1[2,2]*1.96
  confint2[i]=model1[2,1] + model1[2,2]*1.96
  confint3[i]=model1[2,1] - model1[2,2]*1.67
  confint4[i]=model1[2,1] + model1[2,2]*1.67
  coef1[i]=model1[2,1]
}

df <- data.frame(sequence, coef1, confint1, confint2, confint3, confint4)

pf <- ggplot(df, aes(x=sequence, y=coef1)) + geom_point(color = "black") + ylim(-0.6, 0.6) + theme_minimal() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + geom_linerange(ymin = confint1, ymax=confint2, color = "lightgrey") + 
  geom_linerange(ymin = confint3, ymax=confint4, color = "darkgrey") + 
  ylab("Index") + xlab("Bandwidth (%)") + scale_x_continuous() + theme(text=element_text(size=10, family="Times"))


pdf("./outputs/figures/sensitivity.pdf")
grid.arrange(pa, pb, pc, pd, pe, pf, ncol=2)
dev.off()

