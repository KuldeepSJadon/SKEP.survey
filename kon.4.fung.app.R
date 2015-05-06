fungdata <- all.pesticide %>%
        filter(location == country & season == sseason) %>% 
        select(location, year, season, fno, fung, fung.dvs) %>%
        group_by(location, year, season, fno, fung, fung.dvs)%>%
        filter(!fung == "0" ) %>%
        summarise(n.fung.app = n()) %>%
        ungroup() %>%
        arrange(fno)

fungdata$fung.dvs <- as.factor(fungdata$fung.dvs)

levels(fungdata$fung.dvs)[levels(fungdata$fung.dvs) == "Tillering"] <- "TR"
levels(fungdata$fung.dvs)[levels(fungdata$fung.dvs) == "pi"] <- "PI"
levels(fungdata$fung.dvs)[levels(fungdata$fung.dvs) == "Milk"] <- "FL"
levels(fungdata$fung.dvs)[levels(fungdata$fung.dvs) == "TL"] <- "TR"
levels(fungdata$fung.dvs)[levels(fungdata$fung.dvs) == "HD"] <- "HE"
levels(fungdata$fung.dvs)[levels(fungdata$fung.dvs) == "0"] <- "AT"
levels(fungdata$fung.dvs)[levels(fungdata$fung.dvs) == "PF"] <- "SD"

fungdata$fung.dvs <- factor(fungdata$fung.dvs, levels = c("SO","TR","ET", "AT", "MT", "PI", "SD", "ME", "BT","EB","MB","HE","FL","ER","AR", "LR", "HA"))

levels(fungdata$fung.dvs)[levels(fungdata$fung.dvs) == "Tillering"] <- "TR"
levels(fungdata$fung.dvs)[levels(fungdata$fung.dvs) == "pi"] <- "PI"
levels(fungdata$fung.dvs)[levels(fungdata$fung.dvs) == "Milk"] <- "FL"

fungdata$level <- ifelse(fungdata$fno %in% major$fn, "Majority",
                           ifelse(fungdata$fno %in% drif$fn,"Drifter",
                                  ifelse(fungdata$fno %in% adap$fn,"Adapter", NA
                                  )))

fungdata$level <- factor(fungdata$level, level = c("Adapter", "Majority", "Drifter")) 


fungdata$nofarmers <- ifelse(fungdata$level == "Adapter", length(adap$fno),
                               ifelse(fungdata$level == "Majority", length(major$fno),
                                      ifelse(fungdata$level == "Drifter", length(drif$fno), 0)))


levels(fungdata$fung)[levels(fungdata$fung) == "Carbendaxim"] <- "Carbendazim" 
levels(fungdata$fung)[levels(fungdata$fung) == "Dinenoconazole"] <- "Difenoconazole" 


### Change the abb to long 
country <- ifelse(country == "idn", "West Java, Indonesia",
                  ifelse(country == "tmn", "Tamil Nadu, India",
                         ifelse(country == "ods", "Odisha, India",
                                ifelse(country == "tha", "Central plain, Thailand",
                                       ifelse(country == "vnm", "Red River Delta, Vietnam", 0
                                       )))))

sseason <- ifelse(sseason == "ds", "Dry season",
                  ifelse(sseason == "ws", "Wet season", 0
                  ))


levels(fungdata$level)[levels(fungdata$level) == "Adapter"] <- paste0("Adapter\n(n = ", length(adap$fno), ")")
levels(fungdata$level)[levels(fungdata$level) == "Majority"] <- paste0("Majority\n(n = ", length(major$fno), ")")
levels(fungdata$level)[levels(fungdata$level) == "Drifter"] <- paste0("Drifter\n(n = ", length(drif$fno), ")")


#=====================================#
##### select farmer                ###
#=====================================#
fungdata %>%
        filter(!fung == "0", !level == "NA" ) %>%
        group_by(location, year, season,fung, fung.dvs, level, nofarmers) %>%        
        summarise(n.fung.app = n()) %>%
        mutate(freq = n.fung.app/nofarmers) %>%
        ggplot(., aes(x=fung, y = freq, fill =fung)) + geom_bar(stat = "identity") + facet_grid(level ~fung.dvs, scale = "free" , space ="free") + ylim(0,1) + ggtitle(paste("Fungicide Application in", country, "from Survey Data \nin", sseason, "2013 to 2014", sep = " ")) + mytheme + xlab("Fungicide") + ylab("No. Applications Normalized by No. Farmers/Group\n (applications/season)") + scale_fill_brewer(palette= "Set3", name = "Active ingredient") # + theme(legend.position = "right")


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


filename <- paste0("pic/kon/",country,".",sseason,".","fungicide.png")
ggsave(filename, height = 10, width = 14, dpi = 300)