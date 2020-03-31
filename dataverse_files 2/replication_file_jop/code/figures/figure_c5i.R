
#making the dataframe to plot
tf <- candidates_2019_data %>% filter(motivation != "" & !(is.na(motivation))) %>% 
  group_by(gender, namaPartai) %>% summarise(mean(grepl("PEREMPUAN", motivation), na.rm = T))

#relabel genders in english
tf$gender <- ifelse(tf$gender == "Laki-Laki", "Man", 
                   ifelse(tf$gender == "Perempuan", "Woman", NA))

p1 <- ggplot(tf, aes(x=gender, y = `mean(grepl("PEREMPUAN", motivation), na.rm = T)`)) + 
                geom_bar(stat = "identity", color = "darkgrey", fill = "lightgrey") +
                xlab("Gender of Candidate") + ylab("Proportion of Candidate Platforms Mentioning Gender Issues") + theme_test() + ylim(0, 0.4) +
                facet_wrap(namaPartai~.)

ggsave("./outputs/figures/candidate_2019_platforms.pdf", plot = p1, width = 9, height = 9)

rm(tf)