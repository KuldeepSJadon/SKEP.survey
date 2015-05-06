data <- all.pesticide %>%
        filter(location == "IDN" , season == "DS") %>% # 
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


data$level <- ifelse(data$fno %in% y_in_box$fn, "median",
                     ifelse(data$fno %in% y_low_box$fn,"low",
                            ifelse(data$fno %in% y_high_box$fn,"high", NA
                            )))

data$level <- factor(data$level, level = c("high", "median", "low"))

data$nofarmers <- ifelse(data$level == "high", length(y_high_box$fn),
                         ifelse(data$level == "median", length(y_in_box$fno),
                                ifelse(data$level == "low", length(y_low_box), 0)))

levels(data$insect)[levels(data$insect) == "BPMC"] <- "Fenobucarb" 
levels(data$insect)[levels(data$insect) == "Chlorantranilprolee"] <- "Chlorantraniliprole" 
levels(data$insect)[levels(data$insect) == "Chlorantranilprole"] <- "Chlorantraniliprole" 
levels(data$insect)[levels(data$insect) == "Imidaclaprid"] <- "Imidacloprid" 


##### select farmer ###
data %>%
        filter(!insect == "0", !level == "NA" ) %>%
        group_by(location, year, season, insect, ins.dvs, level, nofarmers) %>%        
        summarise(n.insect.app = n()) %>%
        mutate(freq = n.insect.app/nofarmers) %>%
        ggplot(., aes(x= insect, y = freq, fill = insect)) + geom_bar(stat = "identity") + facet_grid(level ~ins.dvs, scale = "free" , space ="free") + ylim(0,1) +ggtitle("Insecticide Application in West Java, Indonesia from Survey Data \nin Dry Season from 2013 to 2014") + mytheme + xlab("Insecticide") + ylab("No. Applications Normalized by No. Farmers/Group\n (applications/season)")  + scale_fill_brewer(palette= "Set3", name = "Active ingredient") #  + theme(legend.position = "right")

ggsave("pic/idn.ds.insecticide.png", height = 10, width = 14, dpi = 300)
