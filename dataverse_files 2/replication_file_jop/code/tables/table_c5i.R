##budget analysis

budget_data$diff <- budget_data$health_prop - budget_data$health_prop_2008

bud_mod1 <- lm(health_prop ~ prop_women, data = budget_data)
bud_mod2 <- lm(health_prop ~ prop_women + health_prop_2008, data = budget_data)
bud_mod3 <- lm(health_prop ~ prop_women + health_prop_2008 + schools + pre_fem_doc + polindes + pop, data = budget_data)
bud_mod4 <- lm(health_prop ~ prop_women + health_prop_2008 + schools + pre_fem_doc + polindes + pop + as.factor(prov), data = budget_data)

observations <- c(nobs(bud_mod1),nobs(bud_mod2),nobs(bud_mod3),
                  nobs(bud_mod4))

bud_mod1 <- coeftest(bud_mod1, vcov=vcovHC(bud_mod1,type="HC1"))
bud_mod2 <- coeftest(bud_mod2, vcov=vcovHC(bud_mod2,type="HC1"))
bud_mod3 <- coeftest(bud_mod3, vcov=vcovHC(bud_mod3,type="HC1"))
bud_mod4 <- coeftest(bud_mod4, vcov=vcovHC(bud_mod4,type="HC1"))

table <- list(bud_mod1, bud_mod2, bud_mod3, bud_mod4)

note_text <- paste("Beta coefficients from OLS regression. Heteroskedastic consistent standard errors were calculated using the huber-white (HC1) correction. Controls are pre–treatment levels of (1) schools, (2) number of female doctors, (3) number of clinics, (4) population.")

table = stargazer(table, type = 'latex', 
                  title = "Effect of Female Representation on District Health Budget",
                  label = 'tab:budget',
                  model.names = F,
                  model.numbers = T,
                  #column.separate = c(6),
                  multicolumn = T,
                  dep.var.labels = c("Share of District Budget Spent on Health (\\%, 2014)"),
                  keep = c("prop_women", "Constant"),
                  add.lines = list(c("Observations", observations),
                                   c("Lagged DV?", "No", "Yes", "Yes", "Yes"),
                                   c("Controls?", "No", "No", "Yes", "Yes"),
                                   c("Province FE?", "No", "No", "No", "Yes")),
                  covariate.labels = c("Proportion Female (2009–2014)"),
                  star.cutoffs = c(0.1, 0.05, 0.01),
                  #float.env = 'sidewaystable',
                  keep.stat = c("n"),
                  notes = NULL,
                  notes.align = 'l')


write_latex(table[-28], note_text, './outputs/tables/table_budget.tex', .85)



