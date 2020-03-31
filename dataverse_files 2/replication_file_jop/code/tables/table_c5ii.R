###########################
#puskemas analysis (health#
# centers) ################
# 1 percent bandwidth######
###########################

#transform variables to millions of rupiah
constituency_data$total_rupiah <- constituency_data$total_rupiah/1000000
constituency_data$health_communication <- constituency_data$health_communication/1000000
constituency_data$pub_health


mod_pusk1 <- lm(total_rupiah ~ woman_win, data = constituency_data)
mod_pusk2 <- lm(health_communication ~ woman_win, data = constituency_data)
mod_pusk3 <- lm(prop_female_doc_pusk ~ woman_win, data = constituency_data)
mod_pusk4 <- lm(pub_health ~ woman_win, data = constituency_data)


observations <- c(nobs(mod_pusk1),nobs(mod_pusk2),nobs(mod_pusk3),nobs(mod_pusk4))

mod_pusk1 <- coeftest(mod_pusk1, vcov=vcovHC(mod_pusk1,type="HC0"))
mod_pusk2 <- coeftest(mod_pusk2, vcov=vcovHC(mod_pusk2,type="HC0"))
mod_pusk3 <- coeftest(mod_pusk3, vcov=vcovHC(mod_pusk3,type="HC0"))
mod_pusk4 <- coeftest(mod_pusk4, vcov=vcovHC(mod_pusk4,type="HC0"))


table <- list(mod_pusk1, mod_pusk2, mod_pusk3, mod_pusk4)

note_text <- paste("Beta coefficients from OLS regression. Heteroskedastic consistent standard errors were calculated using the huber-white (HC0) correction. The outcomes are drawn from the 2014 Indonesian Family Life Survey (IFLS), which surveyed administrators at health clinics in enumeration areas.")

table = stargazer(table, type = 'latex', 
                  title = "Effect of Female Incumbency on Local Health Clinics",
                  label = 'tab:pusk',
                  model.names = F,
                  model.numbers = T,
                  #column.separate = c(6),
                  column.labels = c("Total Funding", "Outreach Initiatives", "Female Doctors", "Public Health Officials"),
                  
                  multicolumn = T,
                  add.lines = list(c("Observations", observations),
                                   c('Bandwidth', rep(c('1\\%'), 4)),
                                   c("Outcome", "IDR (million)", "IDR (million)", "\\%", "\\#")),
                  covariate.labels = c("Female Incumbency"),
                  star.cutoffs = c(0.1, 0.05, 0.01),
                  #float.env = 'sidewaystable',
                  keep.stat = c("n"),
                  notes = NULL,
                  notes.align = 'l')

write_latex_placebo(table[-c(28)], note_text, './outputs/tables/table_pusk.tex', .85)

