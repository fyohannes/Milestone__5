###########################
#female rspondent results##
# 1 percent bandwidth######
###########################

mod_mal1 <- lm(v744a_m2012 ~ woman_win + v744a_m2007, data = constituency_data)
mod_mal2 <- lm(v744b_m2012 ~ woman_win + v744b_m2007, data = constituency_data)
mod_mal3 <- lm(v744c_m2012 ~ woman_win + v744c_m2007, data = constituency_data)
mod_mal4 <- lm(v744d_m2012 ~ woman_win + v744d_m2007, data = constituency_data)
mod_mal5 <- lm(v744e_m2012 ~ woman_win + v744e_m2007, data = constituency_data)
mod_mal7 <- lm(add_index_m2012 ~ woman_win + add_index_m2007, data = constituency_data)

observations <- c(nobs(mod_mal1),nobs(mod_mal2),nobs(mod_mal3),nobs(mod_mal4),nobs(mod_mal5),nobs(mod_mal7))

mod_mal1 <- coeftest(mod_mal1, vcov=vcovHC(mod_mal1,type="HC0"))
mod_mal2 <- coeftest(mod_mal2, vcov=vcovHC(mod_mal2,type="HC0"))
mod_mal3 <- coeftest(mod_mal3, vcov=vcovHC(mod_mal3,type="HC0"))
mod_mal4 <- coeftest(mod_mal4, vcov=vcovHC(mod_mal4,type="HC0"))
mod_mal5 <- coeftest(mod_mal5, vcov=vcovHC(mod_mal5,type="HC0"))
mod_mal7 <- coeftest(mod_mal7, vcov=vcovHC(mod_mal7,type="HC0"))

table <- list(mod_mal1, mod_mal2, mod_mal3, mod_mal4, mod_mal5, mod_mal7)

note_text <- paste("Beta coefficients from OLS regression with lagged (2007) dependentvariable. Heteroskedastic consistent standard errors were calculated using the huber-white (HC0) correction. The outcomes are drawn from a battery of questions that asked respondents if it was acceptable to beat one’s wife if she: (1) goes out without telling her husband; (2) neglects her children; (3) argues with her husband; (4) refuses sex; (5) burns the food. The index is an additive measure.")

table = stargazer(table, type = 'latex', 
                  title = "Effect of Female Incumbency on Male Attitudes Towards IPV, Lagged DV",
                  label = 'tab:table_male_lagged',
                  model.names = F,
                  model.numbers = T,
                  #column.separate = c(6),
                  column.labels = c("Goes out", "Neglects children", "Argues", "Refuses sex", "Burns food", "Index"),
                  
                  multicolumn = T,
                  dep.var.labels = c("Is it okay to beat one's wife if she:"), 
                  add.lines = list(c("Observations", observations),
                                   c('Bandwidth', rep(c('1\\%'), 6))),
                  covariate.labels = c("Female Incumbency", "Goes Out (2007)", "Neglects children  (2007)", "Argues (2007)", "Refuses sex (2007)", "Burns food (2007)", "Index (2007)"),
                  star.cutoffs = c(0.05, 0.01),
                  #float.env = 'sidewaystable',
                  keep.stat = c("n"),
                  notes = NULL,
                  notes.align = 'l')


write_latex_placebo(table[-c(18, 21, 24, 27, 30, 33, 36, 39, 45)], note_text, './outputs/tables/table_male_respondents_lagged.tex', .8)

