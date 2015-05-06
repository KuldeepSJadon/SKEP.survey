#==============================================================
# This script was created for analyse the pesticide application
#============================================================

##### Load package #####
library(dplyr)
library(ggplot2)
library(reshape)
library(reshape2)

##### Set working directory ######
#path <- "~/Google Drive/5.SKEP2Workshop/SKEP2_workshop_bandung/presentation" # For Mac
#path <- "E:/Google Drive/5.SKEP2Workshop/SKEP2_workshop_bandung/presentation" # For Window
#setwd(path)
#getwd()

##### Load source ######
source("R/mytheme.R")

##### Load yield data #####
#yfile <- list.files(path = paste("E:/Google Drive/5.SKEP2Workshop/SKEP2_workshop_bandung/presentation/", sep = ""), pattern = "csv$", full.names = TRUE) # For Window

yfile <- list.files(path = paste("~/Google Drive/Data/SYT-SKEP/Survey", sep = ""), pattern = "csv$", full.names = TRUE) # For Mac



###### Combine yield data #####
yield.list <- list() # create the list.file

for(i in 1: length(yfile)){
        yield.list[[i]] <- read.csv(file = yfile[i])
}

yield <- do.call("bind_rows", yield.list) # combine the list file

yield <- yield %>%
        transform(
                fno = as.factor(fno),
                year = as.factor(year),
                season = as.factor(season),
                location = as.factor(location)
        ) # correct the data types
yield$location <- tolower(yield$location)
