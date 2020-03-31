
###########################
#female rspondent results##
# placebo + parallel ######
# constituencies ##########
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


table <- list(mod2007_fem1, mod2007_fem2, mod2007_fem3, mod2007_fem4, mod2007_fem5, mod2007_fem7)

note_text <- paste("Beta coefficients from OLS regression. Heteroskedastic consistent standard errors were calculated using the huber-white (HC0) correction. The outcomes are drawn from a battery of questions that asked respondents if it was acceptable to beat one’s wife if she: (1) goes out without telling her husband; (2) neglects her children; (3) argues with her husband; (4) refuses sex; (5) burns the food. The index is an additive measure.")

table = stargazer(table, type = 'latex', 
                  title = "Effect of Female Incumbency on Pre-treatment Female Attitudes Towards IPV (Placebo Test + Same Constituencies)",
                  label = 'tab:table_placebo_parallel',
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


write_latex_placebo(table[-27], note_text, './outputs/tables/table_placebo_parallel.tex', .8)

