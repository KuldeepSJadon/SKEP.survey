data <- all.pesticide %>%
  filter(location == "idn") %>% #
  select(location, year, season, fno, insect, ins.dvs) %>%
  group_by(location, year, season, fno, insect, ins.dvs) %>%
  filter(!insect == "0" ) %>%
  summarise(n.insect.app = n()) %>%
  ungroup() %>%
  arrange(fno)

data$ins.dvs <- as.factor(data$ins.dvs)

levels(data$ins.dvs)[levels(data$ins.dvs) == "Tillering"] <- "TR"
levels(data$ins.dvs)[levels(data$ins.dvs) == "pi"] <- "PI"
levels(data$ins.dvs)[levels(data$ins.dvs) == "Milk"] <- "FL"
levels(data$ins.dvs)[levels(data$ins.dvs) == "HD"] <- "HE"
levels(data$ins.dvs)[levels(data$ins.dvs) == "PF"] <- "SD"

data$ins.dvs <- factor(data$ins.dvs, levels = c("SO","TR","ET", "AT", "MT", "PI", "SD", "ME", "BT","EB","MB","HE","FL","ER","AR", "LR", "HA"))


data$level <- ifelse(data$fno %in% y_in_box$fn, "Majority",
                     ifelse(data$fno %in% y_low_box$fn, "Drifter",
                            ifelse(data$fno %in% y_high_box$fn, "Adopter", NA
                            )))

data$level <- factor(data$level, level = c("Adopter", "Majority", "Drifter"))

data$nofarmers <- ifelse(data$level == "Adopter", length(y_high_box$fn),
                         ifelse(data$level == "Majority", length(y_in_box$fno),
                                ifelse(data$level == "Drifter", length(y_low_box), 0)))

levels(data$insect) # check for misspellings

levels(data$insect)[levels(data$insect) == "BPMC"] <- "Fenobucarb"
levels(data$insect)[levels(data$insect) == "Abamectine"] <- "Abamectin"
levels(data$insect)[levels(data$insect) == "Chlorantranilprolee"] <- "Chlorantraniliprole"
levels(data$insect)[levels(data$insect) == "Chlorantranilprole"] <- "Chlorantraniliprole"
levels(data$insect)[levels(data$insect) == "Imidaclaprid"] <- "Imidacloprid"
levels(data$insect)[levels(data$insect) == "chlorpyriphos"] <- "Chlorpyrifos"
levels(data$insect)[levels(data$insect) == "Chlopyrifos"] <- "Chlorpyrifos"
levels(data$insect)[levels(data$insect) == "chlorpyriphos"] <- "Chlorpyrifos"
levels(data$insect)[levels(data$insect) == "Chlrantraniloprole"] <- "Chlorantraniliprole"
levels(data$insect)[levels(data$insect) == "Flubendimide "] <- "Flubendiamide"
levels(data$insect)[levels(data$insect) == "Thiosultap-sodium"] <- "Thiosulfate-sodium"
levels(data$insect)[levels(data$insect) == "Emanmectin benzoamide"] <- "Emamectin Benzoate"
levels(data$insect)[levels(data$insect) == "Cyperminthrin"] <- "Cypermethrin"
levels(data$insect)[levels(data$insect) == "Monochrotophos"] <- "Monocrotophos"
levels(data$insect)[levels(data$insect) == "0"] <- NA

levels(data$insect) # check again for misspellings

##### select farmer ###
data %>%
        filter(!insect == "0", !level == "NA" ) %>%
        group_by(location, year, season, insect, ins.dvs, level, nofarmers) %>%
        summarise(n.insect.app = n()) %>%
        mutate(freq = n.insect.app/nofarmers) %>%
        ggplot(., aes(x= insect, y = freq, fill = insect)) +
  geom_bar(stat = "identity") +
  facet_grid(level*season ~ ins.dvs, scale = "free" , space ="free") +
  ylim(0, 1) +
  ggtitle("Insecticide Application in West Java, Indonesia from\nWet Season Survey Data 2013 and 2014") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", angle = 35,
                                   size = 8,
                                   hjust = 1,
                                   vjust = 1),
        axis.text.y = element_text(face = "bold", size = 8),
        plot.title = element_text(face = "bold", size = 15),
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        legend.position = "none") +
  xlab("Insecticide") +
  ylab("Average Number Applications/Season/Farmer") +
  scale_fill_brewer(palette = "Set3", name = "Active ingredient")  +
  theme(legend.position = "none")

ggsave("~/Google Drive/Figures/SYT-SKEP/Survey/idn.ds.insecticide.png", height = 7, width = 10)

#eos
