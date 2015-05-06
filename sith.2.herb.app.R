#==================================
# Herbicide Application Computation and ggplot
#==================================


herbdata <- all.pesticide %>%
        filter(location == country & season == sseason) %>%
        select(location, year, season, fno, herb, wmg.dvs) %>%
        group_by(location, year, season, fno, herb, wmg.dvs) %>%
        filter(!herb == "0") %>%
        summarise(n.herb.app = n()) %>%
        ungroup() %>%
        arrange(fno)

herbdata$wmg.dvs <- as.factor(herbdata$wmg.dvs)
levels(herbdata$wmg.dvs)[levels(herbdata$wmg.dvs) == "T"] <- "TR"
levels(herbdata$wmg.dvs)[levels(herbdata$wmg.dvs) == "0"] <- "TR"


herbdata$wmg.dvs <- factor(herbdata$wmg.dvs, levels = c("PRE", "SO","TR","ET", "AT", "MT", "PI", "SD","ME", "EB","MB","HE","ER","AR", "LR", "HA"))

herbdata$level <- ifelse(herbdata$fno %in% major$fn, "Majority",
                     ifelse(herbdata$fno %in% drif$fn, "Drifter",
                            ifelse(herbdata$fno %in% adop$fn, "Adopter", NA
                            )))

herbdata$level <- factor(herbdata$level, level = c("Adopter", "Majority", "Drifter"))

herbdata$nofarmers <- ifelse(herbdata$level == "Adopter", length(adop$fno),
                         ifelse(herbdata$level == "Majority", length(major$fno),
                                ifelse(herbdata$level == "Drifter", length(drif$fno), 0)))

herbdata$herb <- as.factor(herbdata$herb)
#levels(herbdata$herb)
levels(herbdata$herb)[levels(herbdata$herb) == "Acetachlor"] <- "Acetochlor"
levels(herbdata$herb)[levels(herbdata$herb) == "Praquate"] <- "Paraquat"
levels(herbdata$herb)[levels(herbdata$herb) == "Paraquate-dichloride"] <- "Paraquat"
levels(herbdata$herb)[levels(herbdata$herb) == "Paraquate dichloride"] <- "Paraquat"
levels(herbdata$herb)[levels(herbdata$herb) == "Metsulfuron-methy"] <- "Metsulfuron-methyl"
levels(herbdata$herb)[levels(herbdata$herb) == "Pyrosulforon-ethyl"] <- "Pyrazosulfuron-ethyl"
levels(herbdata$herb)[levels(herbdata$herb) == "Pyrasulforon-ethyl"] <- "Pyrazosulfuron-ethyl"



country <- ifelse(country == "idn", "West Java, Indonesia",
                  ifelse(country == "tmn", "Tamil Nadu, India",
                         ifelse(country == "ods", "Odisha, India",
                                ifelse(country == "tha", "Central plain, Thailand",
                                       ifelse(country == "vnm", "Red River Delta, Vietnam", 0
                                              )))))

sseason <- ifelse(sseason == "ds", "Dry season",
                 ifelse(sseason == "ws", "Wet season", 0
                        ))

levels(herbdata$level)[levels(herbdata$level) == "Adopter"] <- paste0("Adopter\n(n = ", length(adop$fno), ")")
levels(herbdata$level)[levels(herbdata$level) == "Majority"] <- paste0("Majority\n(n = ", length(major$fno), ")")
levels(herbdata$level)[levels(herbdata$level) == "Drifter"] <- paste0("Drifter\n(n = ", length(drif$fno), ")")

##### Plot Herbicide #####
herbdata %>%
        filter(!herb == "0", !level == "NA" ) %>%
        group_by(location, year, season, herb, wmg.dvs, level, nofarmers) %>%
        summarise(n.herb.app = n()) %>%
        mutate(freq = n.herb.app/nofarmers) %>%
        ggplot(., aes(x= herb, y = freq, fill = herb)) +
        geom_bar(stat = "identity") +
        facet_grid(level ~ wmg.dvs, scale = "free" , space ="free") +
        ylim(0,1) +
        ggtitle(paste("Herbicide Application in", country, "from Survey Data \nin", sseason, "2013 to 2014", sep = " ")) + mytheme + xlab(" Herbicide") + ylab("No. Applications Normalized by No. Farmers/Group\n (applications/season)") + scale_fill_brewer(palette= "Set3", name = "Active ingredient")# +  theme(legend.position = "right")


##### save file #####
country <- ifelse(country == "West Java, Indonesia", "idn",
                  ifelse(country == "Tamil Nadu, India", "tmn",
                         ifelse(country == "Odisha, India", "ods",
                                ifelse(country == "Central plain, Thailand", "tha",
                                       ifelse(country == "Red River Delta, Vietnam","vnm", 0
                                       )))))

sseason <- ifelse(sseason == "Dry season", "ds",
                  ifelse(sseason == "Wet season", "ws", 0
                  ))


#filename <- paste0("pic/kon/",country,".",sseason,".","herbcide.png")
#ggsave(filename, height = 10, width = 14, dpi = 300)

