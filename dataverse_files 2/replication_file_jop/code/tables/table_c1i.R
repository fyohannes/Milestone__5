#make results for individual level analysis

individual_data <- individual_data[!is.na(individual_data$v744a),]


individual_data$v106 <- ifelse(individual_data$v106 == 0, 1, 0)

ind_mod_fem1 <- lm(v744a ~ woman_win, data = individual_data)
ind_mod_fem2 <- lm(v744b ~ woman_win, data = individual_data)
ind_mod_fem3 <- lm(v744c ~ woman_win, data = individual_data)
ind_mod_fem4 <- lm(v744d ~ woman_win, data = individual_data)
ind_mod_fem5 <- lm(v744e ~ woman_win, data = individual_data)
ind_mod_fem7 <- lm(add_index ~ woman_win, data = individual_data)

observations <- c(nobs(ind_mod_fem1),nobs(ind_mod_fem2),nobs(ind_mod_fem3),nobs(ind_mod_fem4),nobs(ind_mod_fem5),nobs(ind_mod_fem7))


ind_mod_fem1 <- coeftest(ind_mod_fem1, vcov=cluster.vcov(ind_mod_fem1, cluster = individual_data$dapil))
ind_mod_fem2 <- coeftest(ind_mod_fem2, vcov=cluster.vcov(ind_mod_fem2, cluster = individual_data$dapil))
ind_mod_fem3 <- coeftest(ind_mod_fem3, vcov=cluster.vcov(ind_mod_fem3, cluster = individual_data$dapil))
ind_mod_fem4 <- coeftest(ind_mod_fem4, vcov=cluster.vcov(ind_mod_fem4, cluster = individual_data$dapil))
ind_mod_fem5 <- coeftest(ind_mod_fem5, vcov=cluster.vcov(ind_mod_fem5, cluster = individual_data$dapil))
ind_mod_fem7 <- coeftest(ind_mod_fem7, vcov=cluster.vcov(ind_mod_fem7, cluster = individual_data$dapil))

table <- list(ind_mod_fem1, ind_mod_fem2, ind_mod_fem3, ind_mod_fem4, ind_mod_fem5, ind_mod_fem7)

note_text <- paste("Beta coefficients from OLS regression. Heteroskedastic consistent standard errors were calculated using the huber-white (HC0) correction. The outcomes are drawn from a battery of questions that asked respondents if it was acceptable to beat one’s wife if she: (1) goes out without telling her husband; (2) neglects her children; (3) argues with her husband; (4) refuses sex; (5) burns the food. The index is an additive measure.")

table = stargazer(table, type = 'latex', 
                  title = "Effect of Female Incumbency on Female Attitudes Towards IPV (Individual Level)",
                  label = 'tab:ind_table',
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


write_latex_placebo(table[-27], note_text, './outputs/tables/table_individual_level.tex', .8)



