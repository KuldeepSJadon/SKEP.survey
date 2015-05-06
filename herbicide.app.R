#### Pesticide application in Indonesia #####

data <- all.pesticide %>%
        filter(location == "IDN" & season == "DS")%>%
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

data$level <- ifelse(data$fno %in% y_in_box$fn, "median",
                              ifelse(data$fno %in% y_low_box$fn,"low",
                                     ifelse(data$fno %in% y_high_box$fn,"high", NA
                                            )))

data$level <- factor(data$level, level = c("high", "median", "low"))

data$nofarmers <- ifelse(data$level == "high", length(y_high_box$fn),
                                  ifelse(data$level == "median", length(y_in_box$fno),
                                         ifelse(data$level == "low", length(y_low_box), 0)))
data$herb <- as.factor(data$herb)
levels(data$herb)
levels(data$herb)[levels(data$herb) == "Praquate"] <- "Paraquat" 
levels(data$herb)[levels(data$herb) == "Paraquate-dichloride"] <- "Paraquat" 
levels(data$herb)[levels(data$herb) == "Paraquate dichloride"] <- "Paraquat" 
levels(data$herb)[levels(data$herb) == "Metsulfuron-methy"] <- "Metsulfuron-methyl"
levels(data$herb)[levels(data$herb) == "Pyrosulforon-ethyl"] <- "Pyrasulfuron-ethyl"   
#levels(data$herb)[levels(data$herb) == "Pyrasulforon-ethyl"] <- "Pyrosulfuron-ethyl" 

##### select farmer ###
data %>%
        filter(!herb == "0", !level == "NA" ) %>%
        group_by(location, year, season, herb, wmg.dvs, level, nofarmers) %>%        
        summarise(n.herb.app = n()) %>%
        mutate(freq = n.herb.app/nofarmers) %>%
        ggplot(., aes(x= herb, y = freq, fill = herb)) + 
        geom_bar(stat = "identity") + 
        facet_grid(level ~ wmg.dvs, scale = "free" , space ="free") + 
        ylim(0,1) + 
        ggtitle("Herbicide Application in West Java, Indonesia from Survey Data \nin Dry Season 2013 to 2014") + mytheme + xlab(" Herbicide") + ylab("No. Applications Normalized by No. Farmers/Group\n (applications/season)") + scale_fill_brewer(palette= "Set3", name = "Active ingredient")# +  theme(legend.position = "right")

ggsave("pic/idn.ds.herbicide.png", height = 10, width = 14, dpi = 300)
 