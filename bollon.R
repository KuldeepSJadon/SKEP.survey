mollus <- select(data.chemical, country, season, Location, Saponin:Niclosamide ) %>%
        melt(id.vars = c("country", "season", "Location")) %>%
        filter( country == c("VNM", "IDN")) %>%
        group_by(country, season, variable) %>%
        summarize(Fre = mean(value))

ggplot(mollus, aes( x = variable, y = country)) +
        facet_grid( ~ season) + 
        ggtitle("Fungicide Use in 2012 - 2014 by Country") + 
        geom_point(aes(size = Fre, fill = variable), shape = 21, color = NA) +
        scale_size_area(max_size = 20, guide = T) +
        scale_fill_discrete() +
        ylab("Mean of No of Application") +
        xlab("Active Ingredient") +
        geom_text(aes(x = as.numeric(variable) - sqrt(Fre)/22, label = round(Fre, digit = 2)), color = "black", size = 3) +
        theme(
                axis.title.x = element_text(colour = "black",size = 16),
                axis.text.x = element_text(angle = 30, 
                                           hjust =1, vjust =1, 
                                           colour = "black", face = "bold" ),
                axis.title.y = element_text(colour = "black",size = 16),
                axis.text.y = element_text(colour = "black", face = "bold"),
                strip.background = element_rect(fill = "pink"),
                legend.position = "none") 
ggsave("mol.bollon.plot.png", width = 36, height = 24, unit = "cm", dpi = 300)

################################################################################
fungi <- select(data.chemical, country, season, Location, Validamycin:Azoxystrobin ) %>% 
        melt(id.vars = c("country", "season", "Location")) %>%
        group_by(country, variable) %>%
        summarize(Fre = mean(value))

ggplot(fungi, aes( x = variable, y = country)) +
        #facet_grid( ) + 
        ggtitle("Fungicide Use in 2012 - 2014 by Country") + 
        geom_point(aes(size = Fre, fill = variable), shape = 21, color = NA) +
        scale_size_area(max_size = 20, guide = T) +
        scale_fill_discrete() +
        ylab("Mean of No of Application") +
        xlab("Active Ingredient") +
        geom_text(aes(x = as.numeric(variable) - sqrt(Fre)/22, label = round(Fre, digit = 2)), color = "black", size = 3) +
        theme(
                axis.title.x = element_text(colour = "black",size = 16),
                axis.text.x = element_text(angle = 30, 
                                           hjust =1, vjust =1, 
                                           colour = "black", face = "bold" ),
                axis.title.y = element_text(colour = "black",size = 16),
                axis.text.y = element_text(colour = "black", face = "bold"),
                strip.background = element_rect(fill = "pink"),
                legend.position = "none") 
ggsave("fung.bollon.plot.png", width = 36, height = 24, unit = "cm", dpi = 300)

################################################################################

insect.vnm <- select(data.chemical, country, season, Location, Dimehypo:Chlorantraniliprole) %>% 
        melt(id.vars = c("country", "season", "Location")) %>%
        filter(country == "VNM") %>%
        group_by(country, variable) %>%
        summarize(Fre = mean(value))

ggplot(insect.vnm, aes( x = variable, y = Fre)) + geom_bar(colour="black", stat="identity")

insect.all <- select(data.chemical, country, season, Location, Dimehypo:Chlorantraniliprole) %>% 
        melt(id.vars = c("country", "season", "Location")) %>%
        #filter(country == "VNM") %>%
        group_by(country, variable) %>%
        summarize(Fre = mean(value))

ggplot(insect.all, aes( x = variable, y = country)) +
        #facet_grid( ~ season) + 
        ggtitle("Insecticide Use in 2012 - 2014 by Country") + 
        geom_point(aes(size = Fre, fill = variable), shape = 21, color = NA) +
        scale_size_area(max_size = 20, guide = T) +
        scale_fill_discrete() +
        ylab("Mean of No of Application") +
        xlab("Active Ingredient") +
        geom_text(aes(x = as.numeric(variable) - sqrt(Fre)/22, label = round(Fre, digit = 2)), color = "black", size = 3) +
        theme(
                axis.title.x = element_text(colour = "black",size = 16),
                axis.text.x = element_text(angle = 30, 
                                           hjust =1, vjust =1, 
                                           colour = "black", face = "bold" ),
                axis.title.y = element_text(colour = "black",size = 16),
                axis.text.y = element_text(colour = "black", face = "bold"),
                strip.background = element_rect(fill = "pink"),
                legend.position = "none") 
ggsave("ins.all.bollon.plot.png", width = 36, height = 24, unit = "cm", dpi = 300)
################################################################################

herbi <- select(data.chemical, country, season, Location, Glyphosate:Propanil) %>% 
        melt(id.vars = c("country", "season", "Location")) %>%
        filter(country == c("VNM","THA", "IDN")) %>%
        group_by(country, variable) %>%
        summarize(Fre = mean(value))

ggplot(herbi, aes( x = variable, y = country)) +
        #facet_grid( ~ season) + 
        ggtitle("Herbicide Use in 2012 - 2014 by Country") + 
        geom_point(aes(size = Fre, fill = variable), shape = 21, color = NA) +
        scale_size_area(max_size = 20, guide = T) +
        scale_fill_discrete() +
        ylab("Mean of No of Application") +
        xlab("Active Ingredient") +
        geom_text(aes(x = as.numeric(variable) - sqrt(Fre)/22, label = round(Fre, digit = 2)), color = "black", size = 3) +
        theme(
                axis.title.x = element_text(colour = "black",size = 16),
                axis.text.x = element_text(angle = 30, 
                                           hjust =1, vjust =1, 
                                           colour = "black", face = "bold" ),
                axis.title.y = element_text(colour = "black",size = 16),
                axis.text.y = element_text(colour = "black", face = "bold"),
                strip.background = element_rect(fill = "pink"),
                legend.position = "none") 
ggsave("herb.bollon.plot.png", width = 36, height = 24, unit = "cm", dpi = 300)


