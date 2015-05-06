data <- all.pesticide %>%
  filter(location == "idn", season == "ws")%>%
  select(location, year, season, fno, fung, fung.dvs) %>%
  group_by(location, year, season, fno, fung, fung.dvs)%>%
  filter(!fung == "0" ) %>%
  summarise(n.fung.app = n()) %>%
  ungroup() %>%
  arrange(fno)

data$fung.dvs <- as.factor(data$fung.dvs)

levels(data$fung.dvs)[levels(data$fung.dvs) == "Tillering"] <- "TR"
levels(data$fung.dvs)[levels(data$fung.dvs) == "pi"] <- "PI"
levels(data$fung.dvs)[levels(data$fung.dvs) == "Milk"] <- "FL"
levels(data$fung.dvs)[levels(data$fung.dvs) == "TL"] <- "TR"
levels(data$fung.dvs)[levels(data$fung.dvs) == "HD"] <- "HE"
levels(data$fung.dvs)[levels(data$fung.dvs) == "0"] <- "AT"
levels(data$fung.dvs)[levels(data$fung.dvs) == "PF"] <- "SD"

data$fung.dvs <- factor(data$fung.dvs, levels = c("SO","TR","ET", "AT", "MT", "PI", "SD", "ME", "BT","EB","MB","HE","FL","ER","AR", "LR", "HA"))

levels(data$fung.dvs)[levels(data$fung.dvs) == "Tillering"] <- "TR"
levels(data$fung.dvs)[levels(data$fung.dvs) == "pi"] <- "PI"
levels(data$fung.dvs)[levels(data$fung.dvs) == "Milk"] <- "FL"

data$level <- ifelse(data$fno %in% y_in_box$fn, "Majority",
                     ifelse(data$fno %in% y_low_box$fn, "Drifter",
                            ifelse(data$fno %in% y_high_box$fn, "Adopter", NA
                            )))

data$level <- factor(data$level, level = c("Adopter", "Majority", "Drifter"))

data$nofarmers <- ifelse(data$level == "Adopter", length(y_high_box$fn),
                         ifelse(data$level == "Majority", length(y_in_box$fno),
                                ifelse(data$level == "Drifter", length(y_low_box), 0)))

levels(data$fung) # check for mispellings

# Correct misspellings
levels(data$fung)[levels(data$fung) == "Carbendaxim"] <- "Carbendazim"
levels(data$fung)[levels(data$fung) == " Mancozeb"] <- "Mancozeb"
levels(data$fung)[levels(data$fung) == "Dinenoconazole"] <- "Difenoconazole"
levels(data$fung)[levels(data$fung) == "Haxaconazole"] <- "Hexaconozole"
levels(data$fung)[levels(data$fung) == "Procloraz"] <- "Prochloraz"
levels(data$fung)[levels(data$fung) == "Tebuconaxole"] <- "Tebuconazole"
levels(data$fung)[levels(data$fung) == "Propiconzole"] <- "Propiconazole"
levels(data$fung)[levels(data$fung) == "Trifolxystrobin"] <- "Trifolxystrobin"
levels(data$fung)[levels(data$fung) == "Methalexyle"] <- "Metalaxyl"
levels(data$fung)[levels(data$fung) == "Thio-phanate"] <- "Thiophanate"
levels(data$fung)[levels(data$fung) == "Oxychloride"] <- NA
levels(data$fung)[levels(data$fung) == "0"] <- NA
levels(data$fung)[levels(data$fung) == "Axozystobin"] <- "Azoxystrobin"

levels(data$fung) # check again for misspellings


#=====================================#
##### select farmer                ###
#=====================================#
data %>%
  filter(!fung == "0", !level == "NA" ) %>% group_by(location, year, season, fung, fung.dvs, level, nofarmers) %>%
  summarise(n.fung.app = n()) %>%
  mutate(freq = n.fung.app/nofarmers) %>%
  ggplot(., aes(x = fung, y = freq, fill = fung)) +
  geom_bar(stat = "identity") +
  facet_grid(level*season ~ fung.dvs, scale = "free" , space = "free") +
  ylim(0, 1) +
  ggtitle("Fungicide Application in West Java, Indonesia from\nWet Season Survey Data 2013 and 2014") +
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
  xlab("Fungicide") +
  ylab("Average Number Applications/Season/Farmer") +
  scale_fill_brewer(palette = "Set3", name = "Active ingredient")  +
  theme(legend.position = "none")

ggsave("~/Google Drive/Figures/SYT-SKEP/Survey/idn.ds.fungicide.png", height = 7, width = 10)
# eos
