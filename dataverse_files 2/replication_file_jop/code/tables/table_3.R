
###########################
#female rspondent results##
# placebo #################
# 1 percent bandwidth######
###########################


mod2007_fem1 <- lm(v744a_f2007 ~ woman_win, data = constituency_data)
mod2007_fem2 <- lm(v744b_f2007 ~ woman_win, data = constituency_data)
mod2007_fem3 <- lm(v744c_f2007 ~ woman_win, data = constituency_data)
mod2007_fem4 <- lm(v744d_f2007 ~ woman_win, data = constituency_data)
mod2007_fem5 <- lm(v744e_f2007 ~ woman_win, data = constituency_data)
mod2007_fem7 <- lm(add_index_f2007 ~ woman_win, data = constituency_data)

observations <- c(nobs(mod2007_fem1),nobs(mod2007_fem2),nobs(mod2007_fem3),
                  nobs(mod2007_fem4),nobs(mod2007_fem5),nobs(mod2007_fem7))


mod2007_fem1 <- coeftest(mod2007_fem1, vcov=vcovHC(mod2007_fem1,type="HC0"))
mod2007_fem2 <- coeftest(mod2007_fem2, vcov=vcovHC(mod2007_fem2,type="HC0"))
mod2007_fem3 <- coeftest(mod2007_fem3, vcov=vcovHC(mod2007_fem3,type="HC0"))
mod2007_fem4 <- coeftest(mod2007_fem4, vcov=vcovHC(mod2007_fem4,type="HC0"))
mod2007_fem5 <- coeftest(mod2007_fem5, vcov=vcovHC(mod2007_fem5,type="HC0"))
mod2007_fem7 <- coeftest(mod2007_fem7, vcov=vcovHC(mod2007_fem7,type="HC0"))



###########################
#male rspondent results##
# placebo #################
# 1 percent bandwidth######
###########################

mod2007_mal1 <- lm(v744a_m2007 ~ woman_win, data = constituency_data)
mod2007_mal2 <- lm(v744b_m2007 ~ woman_win, data = constituency_data)
mod2007_mal3 <- lm(v744c_m2007 ~ woman_win, data = constituency_data)
mod2007_mal4 <- lm(v744d_m2007 ~ woman_win, data = constituency_data)
mod2007_mal5 <- lm(v744e_m2007 ~ woman_win, data = constituency_data)
mod2007_mal7 <- lm(add_index_m2007 ~ woman_win, data = constituency_data)

observations_male <- c(nobs(mod2007_mal1),nobs(mod2007_mal2),nobs(mod2007_mal3),
                       nobs(mod2007_mal4),nobs(mod2007_mal5),nobs(mod2007_mal7))


mod2007_mal1 <- coeftest(mod2007_mal1, vcov=vcovHC(mod2007_mal1,type="HC0"))
mod2007_mal2 <- coeftest(mod2007_mal2, vcov=vcovHC(mod2007_mal2,type="HC0"))
mod2007_mal3 <- coeftest(mod2007_mal3, vcov=vcovHC(mod2007_mal3,type="HC0"))
mod2007_mal4 <- coeftest(mod2007_mal4, vcov=vcovHC(mod2007_mal4,type="HC0"))
mod2007_mal5 <- coeftest(mod2007_mal5, vcov=vcovHC(mod2007_mal5,type="HC0"))
mod2007_mal7 <- coeftest(mod2007_mal7, vcov=vcovHC(mod2007_mal7,type="HC0"))








table <- list(mod2007_fem1, mod2007_mal1, mod2007_fem2, mod2007_mal2, mod2007_fem3, mod2007_mal3, 
              mod2007_fem4, mod2007_mal4, mod2007_fem5, mod2007_mal5)

note_text <- paste(" Beta coefficients from OLS regression. Decimals rounded to two digits to maintain alignment. Standard errors were calculated using the huber-white (HC0) correction. The outcomes are drawn from a battery of questions that asked respondents if it was acceptable to beat one's wife if she: (1) goes out without telling her husband; (2) neglects her children; (3) argues with her husband; (4) refuses sex; (5) burns the food. The index is an additive measure.")

table = stargazer(table, type = 'latex', 
                  title = "Effect of Female Incumbency on Pre-treatment Attitudes Towards IPV (Placebo Test)",
                  label = 'tab:placebo',
                  model.names = F,
                  model.numbers = T,
                  #column.separate = c(6),
                  column.separate = c(2,2,2,2,2,2),
                  column.labels = c("Goes out", "Neglects children", "Argues", "Refuses sex", "Burns food", "Index"),
                  
                  multicolumn = T,
                  dep.var.labels = c("Is it okay to beat one's wife if she:"), 
                  add.lines = list(c("Observations", rep(observations, 2)),
                                   c('Bandwidth', rep(c('1\\%'), 12)),
                                   c("Gender", rep(c("F", "M"), 6))),
                  covariate.labels = c("Female"),
                  star.cutoffs = c(0.05, 0.01),
                  digits = 3,
                  #float.env = 'sidewaystable',
                  keep.stat = c("n"),
                  notes = NULL,
                  notes.align = 'l')


write_latex_placebo(table[-c(18, 21, 28)], note_text, './outputs/tables/table_placebo_main.tex', .8)






table <- list(mod2007_fem1, mod2007_mal1, mod2007_fem2, mod2007_mal2, mod2007_fem3, mod2007_mal3, 
              mod2007_fem4, mod2007_mal4, mod2007_fem5, mod2007_mal5, mod2007_fem7, mod2007_mal7)

note_text <- paste(" Beta coefficients from OLS regression. Decimals rounded to two digits to maintain alignment. Heteroskedastic consistent standard errors were calculated using the huber-white (HC0) correction. The outcomes are drawn from a battery of questions that asked respondents if it was acceptable to beat one's wife if she: (1) goes out without telling her husband; (2) neglects her children; (3) argues with her husband; (4) refuses sex; (5) burns the food. The index is an additive measure.")

table = stargazer(table, type = 'latex', 
                  title = "Effect of Female Incumbency on Pre-treatment Female Support for IPV (Placebo Test)",
                  label = 'tab:placebo_full',
                  model.names = F,
                  model.numbers = T,
                  #column.separate = c(6),
                  column.separate = c(2,2,2,2,2,2),
                  column.labels = c("Goes out", "Neglects children", "Argues", "Refuses sex", "Burns food", "Index"),
                  
                  multicolumn = T,
                  dep.var.labels = c("Is it okay to beat one's wife if she:"), 
                  add.lines = list(c("Observations", rep(observations, 2)),
                                   c('Bandwidth', rep(c('1\\%'), 12)),
                                   c("Gender", rep(c("F", "M"), 6))),
                  covariate.labels = c("Female"),
                  star.cutoffs = c(0.05, 0.01),
                  digits = 3,
                  #float.env = 'sidewaystable',
                  keep.stat = c("n"),
                  notes = NULL,
                  notes.align = 'l')


write_latex_placebo(table[-c(18, 21, 28)], note_text, './outputs/tables/table_placebo_full.tex', .8)




