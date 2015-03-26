library(XLConnect)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)

chemfile <- list.files(path = paste("~/Google Drive/SKEP2ProjectData/Farm Survey/", sep = ""), pattern = "[[:graph:]]+.xls$", full.names = TRUE)
chemdata <- loadWorkbook(chemfile[9])
#data.sheet4 <- readWorksheet(data, sheet= "yield", startRow = 1, startCol =1, header = TRUE)
data.fre <- readWorksheet(chemdata, sheet = "FRE" ,startRow = 1, startCol = 1, header = TRUE)
data.chemical <- readWorksheet(chemdata, sheet = "chemical", header = TRUE)
#ppi = 300 

data.sheet4 %>%
        group_by(country, year, season) %>%
        ggplot(aes(country,yield.t.ha)) + 
       # geom_violin(aes(fill=country)) + 
        #geom_boxplot(width = 0.1, fill = "black", outlier.color = NA) +
        geom_boxplot(aes(fill = country))+
       # stat_summary(fun.y = median, geom = "point", shape = 21, size = 2.5, fill = "white") + 
        facet_wrap( ~ season) + 
        ylab("Yield (ton/ha)") + 
        xlab("Country") +
        ggtitle( "Rice Yield on 2012 - 2014 by Season and Country" ) + 
        theme(strip.background = element_rect(fill = "pink"), legend.position = "none")

ggsave("orm.yield.plot.png", width = 24, height = 16, unit = "cm", dpi = 300)

data.fre %>%
        melt(id.vars = c("country", "year", "season", "samp")) %>%
        group_by(country, year, season, variable) %>%
        filter( variable == "freq.mol") %>%
        ggplot(aes(x = season, y = value)) + 
        geom_boxplot(aes(fill = country)) +
        facet_grid(.~ country) +
        ylab("No. of Application")+
        xlab("Season")+ 
        ggtitle("Frequency of Molluscicide Application in 2012 - 2013") +
        theme(legend.position = "none")

ggsave("mol.plot.png", width = 24, height = 16, unit = "cm", dpi = 300)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

levels(data.chemical$season)[levels(data.chemical$season) == "WS"] <- " Wet Season"
levels(data.chemical$season)[levels(data.chemical$season) == "DS"] <- " Dry Season"

data.chemical %>% 
        select(country, season, Location, Saponin:Niclosamide) %>%
        melt(id.vars = c("country", "season", "Location")) %>%
        group_by(country, season, variable) %>%
        ggplot(aes(x = variable, y = value)) +
        scale_fill_manual(values=cbPalette) +
        geom_boxplot(aes(fill = country), outlier.color = "green", outlier.size = 3) + 
        facet_grid( ~ season) + 
        ggtitle("Molluscicide Use in 2012 - 2014 by Country") + 
        ylab("No of Application") +
        xlab("Active Ingredient") +
        theme(
                axis.title.x = element_text(colour = "black",size = 16),
                axis.text.x = element_text(colour = "black", face = "bold" ),
                axis.title.y = element_text(colour = "black",size = 16),
                axis.text.y = element_text(colour = "black", face = "bold"),
                strip.background = element_rect(fill = "pink")
                )

ggsave("mol.chem.plot.png", width = 24, height = 16, unit = "cm", dpi = 300)

data.chemical %>% 
        select(country, season, Location, Glyphosate:Propanil) %>%
        melt(id.vars = c("country", "season", "Location")) %>%
        group_by(country, season, variable) %>%
        ggplot(aes(x = variable, y = value)) + 
        geom_boxplot(aes(fill = country), outlier.color = "green", outlier.size = 3) + 
        facet_grid( ~ season) + 
        ggtitle("Herbicide Use in 2012 - 2014 by Country") + 
        ylab("No of Application") +
        xlab("Active Ingredient") +
        theme(
                axis.title.x = element_text(colour = "black",size = 16),
                axis.text.x = element_text(angle = 30, 
                                           hjust =1, vjust =1, 
                                           colour = "black", face = "bold" ),
                axis.title.y = element_text(colour = "black",size = 16),
                axis.text.y = element_text(colour = "black", face = "bold"),
                strip.background = element_rect(fill = "pink")
        )
        
ggsave("herb.chem.plot.png", width = 24, height = 16, unit = "cm", dpi = 300)

data.chemical %>% 
        select(country, season, Location, Dimehypo:Chlorantraniliprole) %>%
        filter(country == "THA") %>%
        melt(id.vars = c("country", "season", "Location")) %>%
        group_by(country, season, variable) %>%
        ggplot(aes(x = variable, y = value)) + 
        geom_boxplot(aes(fill = country)) + 
        facet_grid( ~ season) + 
        ggtitle("Insecticide Use in 2012 - 2014 by Country") + 
        ylab("No of Application") +
        xlab("Active Ingredient") +
        theme(
                axis.title.x = element_text(colour = "black",size = 16),
                axis.text.x = element_text(angle = 30, 
                                           hjust =1, vjust =1, 
                                           colour = "black", face = "bold" ),
                axis.title.y = element_text(colour = "black",size = 16),
                axis.text.y = element_text(colour = "black", face = "bold"),
                strip.background = element_rect(fill = "pink"))

ggsave("THA.ins.chem.plot.png", width = 24, height = 16, unit = "cm", dpi = 300)

data.chemical %>% 
        select(country, season, Location, Validamycin:Azoxystrobin) %>%
        melt(id.vars = c("country", "season", "Location")) %>%
        group_by(country, season, variable) %>%
        ggplot(aes(x = variable, y = value)) + 
        geom_boxplot(aes(fill = country), outlier.color = "green", outlier.size = 3) + 
        facet_grid( ~ season) + 
        ggtitle("Fungicide Use in 2012 - 2014 by Country") + 
        ylab("No of Application") +
        xlab("Active Ingredient") +
        theme(
                axis.title.x = element_text(colour = "black",size = 16),
                axis.text.x = element_text(angle = 30, 
                                           hjust =1, vjust =1, 
                                           colour = "black", face = "bold" ),
                axis.title.y = element_text(colour = "black",size = 16),
                axis.text.y = element_text(colour = "black", face = "bold"),
                strip.background = element_rect(fill = "pink"))

ggsave("fung.chem.plot.png", width = 24, height = 16, unit = "cm", dpi = 300)





#png("yield.plot.png", width = 4*ppi, height = 4*ppi, rs = ppi)

ggsave("mol.plot.png", width = 24, height = 16, unit = "cm", dpi = 300)
