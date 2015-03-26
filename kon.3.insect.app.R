insectdata <- all.pesticide %>%
        filter(location == country & season == sseason) %>% # 
        select(location, year, season, fno, insect, ins.dvs) %>%
        group_by(location, year, season, fno, insect, ins.dvs) %>%
        filter(!insect == "0" ) %>%
        summarise(n.insect.app = n()) %>%
        ungroup() %>%
        arrange(fno)

insectdata$ins.dvs <- as.factor(insectdata$ins.dvs)

levels(insectdata$ins.dvs)[levels(insectdata$ins.dvs) == "Tillering"] <- "TR"
levels(insectdata$ins.dvs)[levels(insectdata$ins.dvs) == "pi"] <- "PI"
levels(insectdata$ins.dvs)[levels(insectdata$ins.dvs) == "Milk"] <- "FL"
levels(insectdata$ins.dvs)[levels(insectdata$ins.dvs) == "HD"] <- "HE"
levels(insectdata$ins.dvs)[levels(insectdata$ins.dvs) == "PF"] <- "SD"

insectdata$ins.dvs <- factor(insectdata$ins.dvs, levels = c("SO","TR","ET", "AT", "MT", "PI", "SD", "ME", "BT","EB","MB","HE","FL","ER","AR", "LR", "HA"))


insectdata$level <- ifelse(insectdata$fno %in% major$fn, "Majority",
                         ifelse(insectdata$fno %in% drif$fn,"Drifter",
                                ifelse(insectdata$fno %in% adap$fn,"Adapter", NA
                                )))

insectdata$level <- factor(insectdata$level, level = c("Adapter", "Majority", "Drifter"))

insectdata$nofarmers <- ifelse(insectdata$level == "Adapter", length(adap$fno),
                             ifelse(insectdata$level == "Majority", length(major$fno),
                                    ifelse(insectdata$level == "Drifter", length(drif$fno), 0)))


levels(insectdata$insect)[levels(insectdata$insect) == "BPMC"] <- "Fenobucarb" 
levels(insectdata$insect)[levels(insectdata$insect) == "Chlorantranilprolee"] <- "Chlorantraniliprole" 
levels(insectdata$insect)[levels(insectdata$insect) == "Chlorantranilprole"] <- "Chlorantraniliprole" 
levels(insectdata$insect)[levels(insectdata$insect) == "Imidaclaprid"] <- "Imidacloprid" 
levels(insectdata$insect)[levels(insectdata$insect) == "Chlopyrifos"] <- "Chlorpyrifos" 

### Chenge the abb to long 
country <- ifelse(country == "idn", "West Java, Indonesia",
                  ifelse(country == "tmn", "Tamil Nadu, India",
                         ifelse(country == "ods", "Odisha, India",
                                ifelse(country == "tha", "Central plain, Thailand",
                                       ifelse(country == "vnm", "Red River Delta, Vietnam", 0
                                       )))))

sseason <- ifelse(sseason == "ds", "Dry season",
                  ifelse(sseason == "ws", "Wet season", 0
                  ))

levels(insectdata$level)[levels(insectdata$level) == "Adapter"] <- paste0("Adapter\n(n = ", length(adap$fno), ")")
levels(insectdata$level)[levels(insectdata$level) == "Majority"] <- paste0("Majority\n(n = ", length(major$fno), ")")
levels(insectdata$level)[levels(insectdata$level) == "Drifter"] <- paste0("Drifter\n(n = ", length(drif$fno), ")")
##### select farmer ###
insectdata %>%
        filter(!insect == "0", !level == "NA" ) %>%
        group_by(location, year, season, insect, ins.dvs, level, nofarmers) %>%        
        summarise(n.insect.app = n()) %>%
        mutate(freq = n.insect.app/nofarmers) %>%
        ggplot(., aes(x= insect, y = freq, fill = insect)) + geom_bar(stat = "identity") + facet_grid(level ~ins.dvs, scale = "free" , space ="free") + ylim(0,1) + ggtitle(paste("Insecticide Application in", country, "from Survey Data \nin", sseason, "2013 to 2014", sep = " ")) + mytheme + xlab("Insecticide") + ylab("No. Applications Normalized by No. Farmers/Group\n (applications/season)")  + scale_fill_brewer(palette= "Set3", name = "Active ingredient") #  + theme(legend.position = "right")

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


filename <- paste0("pic/kon/",country,".",sseason,".","insecticide.png")
ggsave(filename, height = 10, width = 14, dpi = 300)
