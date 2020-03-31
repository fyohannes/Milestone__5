#####################################
## DESCRIPTIVE STATISTICS############
#####################################

########################
### DEPENDENT VARIABLES#
### Female respondents##
########################
note_text <- paste("")



table = stargazer(constituency_data, type = 'latex', 
                  title = "Descriptive Statistics (Female Respondent Outcomes)",
                  label = 'tab:dso',
                  summary = T,
                  keep = c(7, 8, 9, 10, 11, 12),
                  covariate.labels = c("Goes out", "Neglects children", "Argues", "Refuses sex", "Burns food", "Additive index")
                  )


write_latex_plain(table, note_text, './outputs/tables/descriptives_outcomes.tex')


########################
### DEPENDENT VARIABLES#
### male respondents##
########################
note_text <- paste("")


table = stargazer(constituency_data, type = 'latex', 
                  title = "Descriptive Statistics (Male Respondent Outcomes)",
                  label = 'tab:dso_male',
                  summary = T,
                  keep = c(25, 26, 27, 28, 29, 30),
                  covariate.labels = c("Goes out", "Neglects children", "Argues", "Refuses sex", "Burns food", "Additive index")
)


write_latex_plain(table, note_text, './outputs/tables/descriptives_outcomes_male.tex')



stargazer(constituency_data, keep = 13)

names(constituency_data)

#########################
###INDEPENDENT VARIABLES#
#########################

note_text <- paste("")
constituency_data$pop <- constituency_data$pop/10000
table = stargazer(constituency_data, type = 'latex', 
                  title = "Descriptive Statistics (Balance Tests)",
                  label = 'tab:desc_tab_bt',
                  summary = T,
                  keep = c(13, 14, 15, 37, 38, 39, 40, 41, 42, 43, 46, 47),
                  covariate.labels = c("(Resp) Age", "(Resp) Education", "(Resp) Owns motorcycle (\\%)",
                                       "Number of Villages (\\#)", "Population (x 10,000)", "Schools(\\#)", "Male Doctors (\\#)",
                                       "Female Doctors (\\#)", "Mosques(\\#)", "Pubs(\\#)", "Gambling Institutions (\\#)",
                                       "Civil Defense Forces(\\#)")
                  
            
)


write_latex_plain(table, note_text, './outputs/tables/table_descriptives_balance_tests.tex')



