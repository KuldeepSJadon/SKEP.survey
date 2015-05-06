data <- all.pesticide %>%
        filter(location == "IDN" & season == "DS") %>% 
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

data$level <- ifelse(data$fno %in% y_in_box$fn, "median",
                     ifelse(data$fno %in% y_low_box$fn,"low",
                            ifelse(data$fno %in% y_high_box$fn,"high", NA
                            )))

data$level <- factor(data$level, level = c("high", "median", "low"))

data$nofarmers <- ifelse(data$level == "high", length(y_high_box$fn),
                         ifelse(data$level == "median", length(y_in_box$fno),
                                ifelse(data$level == "low", length(y_low_box), 0)))


levels(data$fung)[levels(data$fung) == "Carbendaxim"] <- "Carbendazim" 
levels(data$fung)[levels(data$fung) == "Dinenoconazole"] <- "Difenoconazole" 


#=====================================#
##### select farmer                ###
#=====================================#
data %>%
        filter(!fung == "0", !level == "NA" ) %>%
        group_by(location, year, season,fung, fung.dvs, level, nofarmers) %>%        
        summarise(n.fung.app = n()) %>%
        mutate(freq = n.fung.app/nofarmers) %>%
        ggplot(., aes(x=fung, y = freq, fill =fung)) + geom_bar(stat = "identity") + facet_grid(level ~fung.dvs, scale = "free" , space ="free") + ylim(0,1) +ggtitle("Fungicide Application in Red River Delta, Vietnam from Survey Data \nin Dry Season from 2013 to 2014") + mytheme + xlab("Fungicide") + ylab("No. Applications Normalized by No. Farmers/Group\n (applications/season)") + scale_fill_brewer(palette= "Set3", name = "Active ingredient")  + theme(legend.position = "right")

ggsave("pic/idn.ds.fungicide.png", height = 10, width = 14, dpi = 300)
