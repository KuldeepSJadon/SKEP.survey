#### Pesticide application in Indonesia #####

data <- all.pesticide %>%
  filter(location == "idn", season == "ws")%>%
  select(location, year, season, fno, herb, wmg.dvs) %>%
  group_by(location, year, season, fno, herb, wmg.dvs) %>%
  filter(!herb == "0" ) %>%
  summarise(n.herb.app = n()) %>%
  ungroup() %>%
  arrange(fno)

data$wmg.dvs <- as.factor(data$wmg.dvs)
levels(data$wmg.dvs)[levels(data$wmg.dvs) == "T"] <- "TR"
levels(data$wmg.dvs)[levels(data$wmg.dvs) == "0"] <- "TR"


data$wmg.dvs <- factor(data$wmg.dvs, levels = c("PRE", "SO","TR","ET", "AT", "MT", "PI", "SD","ME", "EB","MB","HE","ER","AR", "LR", "HA"))

data$level <- ifelse(data$fno %in% y_in_box$fn, "Majority",
                              ifelse(data$fno %in% y_low_box$fn, "Drifter",
                                     ifelse(data$fno %in% y_high_box$fn, "Adopter", NA
                                            )))

data$level <- factor(data$level, level = c("Adopter", "Majority", "Drifter"))

data$nofarmers <- ifelse(data$level == "Adopter", length(y_high_box$fn),
                                  ifelse(data$level == "Majority", length(y_in_box$fno),
                                         ifelse(data$level == "Drifter", length(y_low_box), 0)))
data$herb <- as.factor(data$herb)
levels(data$herb)
levels(data$herb)[levels(data$herb) == "Praquate"] <- "Paraquat"
levels(data$herb)[levels(data$herb) == "Paraquate-dichloride"] <- "Paraquat"
levels(data$herb)[levels(data$herb) == "Paraquate dichloride"] <- "Paraquat"
levels(data$herb)[levels(data$herb) == "Paraquat dichloride"] <- "Paraquat"
levels(data$herb)[levels(data$herb) == "Metsulfuron-methy"] <- "Metsulfuron-methyl"
levels(data$herb)[levels(data$herb) == "Pyrosulforon-ethyl"] <- "Pyrazosulfuron-ethyl"
levels(data$herb)[levels(data$herb) == "Pyrasulfuron-ethyl"] <- "Pyrazosulfuron-ethyl"

levels(data$herb)

##### select farmer ###
data %>%
  filter(!herb == "0", !level == "NA" ) %>%
  group_by(location, year, season, herb, wmg.dvs, level, nofarmers) %>%
  summarise(n.herb.app = n()) %>%
  mutate(freq = n.herb.app/nofarmers) %>%
  ggplot(., aes(x= herb, y = freq, fill = herb)) +
  geom_bar(stat = "identity") +
  facet_grid(level*season ~ wmg.dvs, scale = "free" , space = "free") +
  ylim(0, 1) +
  ggtitle("Herbicide Application in West Java, Indonesia from\nWet Season Survey Data 2013 and 2014") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", angle = 35,
                                   size = 9,
                                   hjust = 1,
                                   vjust = 1),
        axis.text.y = element_text(face = "bold", size = 8),
        plot.title = element_text(face = "bold", size = 15),
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        legend.position = "none") +
  xlab("Herbicide") +
  ylab("Average Number Applications/Season/Farmer") +
  scale_fill_brewer(palette = "Set3", name = "Active ingredient") +
  theme(legend.position = "none")

ggsave("~/Google Drive/Figures/SYT-SKEP/Survey/idn.ds.herbicide.png", height = 7, width = 10)

#eos
