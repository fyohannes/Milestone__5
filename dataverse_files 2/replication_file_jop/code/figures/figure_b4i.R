

laws <- health_laws_2009 %>% group_by(kab_code, year) %>% summarise(sum(health),
                                                  n()) %>%
  mutate(prop_health = `sum(health)`/`n()`)

gend <- constituency_data_main %>% group_by(kab_code) %>% summarise(sum(woman_win),
                                                  n()) %>%
  filter(`n()` == 1)

plotdat <- left_join(laws, gend, by = "kab_code")

plotlaws <- ggplot(plotdat[!is.na(plotdat$`sum(woman_win)`),], aes(x=year, y  = prop_health * 100, linetype = as.factor(`sum(woman_win)`))) +
                geom_line(stat = "summary") + theme_minimal() + xlab("Year") + xlim(1995, 2009) + 
  ylab("Laws Concerning 'Health' in DPRD-II (% of all Laws)") +
  scale_linetype_discrete(name = "Woman in Race \nfor Last Seat \n in 2009", labels=c("Lost", "Win"))


ggsave("./outputs/figures/laws_graph.pdf",plot=plotlaws,  width = 7, height = 4.5)
