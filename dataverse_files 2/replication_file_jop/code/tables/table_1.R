###########################
#female rspondent results##
# 1 percent bandwidth######
###########################

mod_fem1 <- lm(v744a_f2012 ~ woman_win, data = constituency_data)
mod_fem2 <- lm(v744b_f2012 ~ woman_win, data = constituency_data)
mod_fem3 <- lm(v744c_f2012 ~ woman_win, data = constituency_data)
mod_fem4 <- lm(v744d_f2012 ~ woman_win, data = constituency_data)
mod_fem5 <- lm(v744e_f2012 ~ woman_win, data = constituency_data)
mod_fem7 <- lm(add_index_f2012 ~ woman_win, data = constituency_data)

observations <- c(nobs(mod_fem1),nobs(mod_fem2),nobs(mod_fem3),nobs(mod_fem4),nobs(mod_fem5),nobs(mod_fem7))

mod_fem1 <- coeftest(mod_fem1, vcov=vcovHC(mod_fem1,type="HC0"))
mod_fem2 <- coeftest(mod_fem2, vcov=vcovHC(mod_fem2,type="HC0"))
mod_fem3 <- coeftest(mod_fem3, vcov=vcovHC(mod_fem3,type="HC0"))
mod_fem4 <- coeftest(mod_fem4, vcov=vcovHC(mod_fem4,type="HC0"))
mod_fem5 <- coeftest(mod_fem5, vcov=vcovHC(mod_fem5,type="HC0"))
mod_fem7 <- coeftest(mod_fem7, vcov=vcovHC(mod_fem7,type="HC0"))

table <- list(mod_fem1, mod_fem2, mod_fem3, mod_fem4, mod_fem5, mod_fem7)

note_text <- paste("Beta coefficients from OLS regression. Standard errors were calculated using the huber-white (HC0) correction. The outcomes are drawn from a battery of questions that asked respondents if it was acceptable to beat one's wife if she: (1) goes out without telling her husband; (2) neglects her children; (3) argues with her husband; (4) refuses sex; (5) burns the food. The index is an additive measure.")

table = stargazer(table, type = 'latex', 
                  title = "Effect of Female Incumbency on Female Attitudes Towards IPV",
                  label = 'tab:table_female',
                  model.names = F,
                  model.numbers = T,
                  #column.separate = c(6),
                  column.labels = c("Goes out", "Neglects children", "Argues", "Refuses sex", "Burns food", "Index"),
                  
                  multicolumn = T,
                  dep.var.labels = c("Is it okay to beat one's wife if she:"), 
                  add.lines = list(c("Observations", observations),
                                   c('Bandwidth', rep(c('1\\%'), 6))),
                  covariate.labels = c("Female Incumbency"),
                  star.cutoffs = c(0.05, 0.01),
                  #float.env = 'sidewaystable',
                  keep.stat = c("n"),
                  notes = NULL,
                  notes.align = 'l')

write_latex_placebo(table[-c(18, 21, 27)], note_text, './outputs/tables/table_female_respondents.tex', .8)

