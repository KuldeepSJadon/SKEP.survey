###############################################################################
#'title         : Load yield data of survey data and plot yield by countries and seasons
#'date          : May, 2015
#'purpose       : load data from the shared Google Drive which is Excel
#'                format
#'author        : Sith Jaisong (s.jaisong@irri.org)
#'modified by   : Adam H. Sparks (a.sparks@irri.org)
#'contact       : s.jaisong@irri.org
#'input         : Excel file of yield data from on-farm surveys
#'output        : gggraph of yield
###############################################################################
##### Loading packages
library(dplyr)
library(ggplot2)
library(reshape)
library(reshape2)
##### End of loading packages

#yfile <- list.files(path = paste("E:/Google Drive/SKEP2_workshop_bandung/presentation/", sep = ""), pattern = "csv$", full.names = TRUE) # For Window
yfile <- list.files(path = paste("~/Google Drive/Data/SYT-SKEP/Survey", sep = ""), pattern = "csv$", full.names = TRUE)

yield.list <- list() # create the list.file

# store csv files in list
for(i in 1: length(yfile)){
        yield.list[[i]] <- read.csv(file = yfile[i])
}

yield <- do.call("bind_rows", yield.list) # combine all file in one data frame

yield <- yield %>%
        transform(
                fno = as.factor(fno),
                year = as.factor(year),
                season = as.factor(season),
                location = as.factor(location)
                )
yield$location <- factor(yield$location, levels = c("IDN", "ods", "tmn", "tha", "vnm"))

# Rename seasons
levels(yield$season)[levels(yield$season)== "ds"] <- "Dry Season"
levels(yield$season)[levels(yield$season)== "ws"] <- "Wet Season"

# Rename locations
levels(yield$location)[levels(yield$location)== "IDN"] <- "Indonesia"
levels(yield$location)[levels(yield$location)== "ods"] <- "Odisha, India"
levels(yield$location)[levels(yield$location)== "tmn"] <- "Tamil Nadu, India"
levels(yield$location)[levels(yield$location)== "vnm"] <- "Vietnam"
levels(yield$location)[levels(yield$location)== "tha"] <- "Thailand"

#yield %>% group_by(location, year, season) %>%
#        summarise(yymean = mean(ymean))

#=====Load mytheme for plot graph =====
source("mytheme.r")
#=====End of loading mytheme =========

##### Graph yield of all countries #####
ggplot(yield,
       aes(x= year, y= ymean, fill = location)) +
        geom_boxplot() +
        facet_grid(. ~ season) +
        ylab("Yield (t/ha)") +
        xlab("Year") +
        labs(fill = "Location") +
        scale_fill_brewer(palette = "Set3") +
        mytheme +
        ggtitle("Yields from Survey data 2013 to 2014")

#ggsave("pic/survey.yield1.png", height = 6, width = 10, dpi = 300)

##### Graph yield of Indonesia #####
ggplot(yield %>% filter(location == "Indonesia" & season == "Dry Season"),
       aes(x = year, y = ymean)) +
  geom_boxplot(aes(fill = location, colour = location), alpha = 0.65) +
  facet_grid(. ~ year, scale = "free" , space = "free") +
  ylab("Yield (t/ha)") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 8),
        plot.title = element_text(face = "bold", size = 15),
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        legend.position = "none") +
  ylab("Yield (tons/ha)") +
  xlab("") +
  scale_fill_brewer(palette = "Set3")  +
  scale_colour_brewer(palette = "Set3")  +
  theme(legend.position = "none") +
  ggtitle("Observed Crop Cut Yields from Indonesia Wet Season Surveys")

ggsave("~/Google Drive/Figures/SYT-SKEP/Survey/Indonesia_Yield.png", width = 10, height = 7, units = "in")

#ggsave("pic/indo.yield.png", height = 6, width = 12, dpi = 300)
#============================================================================
# This script will be used for catagorizing farmers in each country
#===========================================================================
#### select famer by yield #####

#ydata <- yield %>% filter(location == "Indonesia"  & season == "Wet Season")
#boxplot(ydta$ymean)$stats


### select and categorize farmer according to yields ####
#y_in_box <- ydata %>%
#         filter(ymean > boxplot(ydata$ymean)$stats[2,] & ymean < boxplot(ydata$ymean)$stats[4,])
#
# y_low_box <- ydata %>%
#         filter( ymean <= boxplot(ydata$ymean)$stats[2,])
#
# y_high_box <- ydata %>%
#         filter(ymean >= boxplot(ydata$ymean)$stats[4,])


# eos
