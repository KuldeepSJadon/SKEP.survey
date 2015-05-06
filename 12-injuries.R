###############################################################################
#'title         : Load injuries profiles from survey data and combine in one file
#'date          : May, 2015
#'purpose       : load data from the shared Google Drive which is the exel 
#'                format
#'writed by     : Sith Jaisong (s.jaisong@irri.org)
#'contact       : International Rice Research Institute
#'input         : import excel file
#'output        : all.inj will be used in 13-injuriescom.R
###############################################################################
##### Loading packages
library(dplyr)
library(ggplot2)
library(reshape)
library(reshape2)
##### End of loading packages
##### Set the working directory
#path <- "~/Google Drive/5.SKEP2Workshop/SKEP2_workshop_bandung/presentation" # for Mac
#path <- "E:/Google Drive/5.SKEP2Workshop/SKEP2_workshop_bandung/presentation" # for window
#setwd(path)
getwd()
options(stringAsFactors = FALSE)
##### End od setting working directory #####

library(XLConnect)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)
options(java.parameters = "-Xmx1024m")
#file <- list.files(path = paste("E:/Google Drive/4.SKEP2ProjectData/Farm Survey/", sep = ""), pattern = "20[[:graph:]]+.xlsx$", full.names = TRUE)

# load inuiry data from ggdirve
# please check I share this folder to Adam and Nancy
file <- list.files(path = paste("~/Google Drive/4.SKEP2ProjectData/Farm Survey/", sep = ""), pattern = "20[[:graph:]]+.xlsx$", full.names = TRUE)

idn.2013.ds.filepath <- file[1]
ods.2013.ds.filepath <- file[2]
tha.2013.ds.filepath <- file[3]
tmn.2013.ds.filepath <- file[4]
vnm.2013.ds.filepath <- file[5]
idn.2013.ws.filepath <- file[6]
ods.2013.ws.filepath <- file[7]
tha.2013.ws.filepath <- file[8]
tmn.2013.ws.filepath <- file[9]
idn.2014.ds.filepath <- file[10]
ods.2014.ws.filepath <- file[11]
tmn.2014.ws.filepath <- file[12]
vnm.2014.ws.filepath <- file[13]


vnm.2014.ws <- loadWorkbook(vnm.2014.ws.filepath)
vnm.2013.ds <- loadWorkbook(vnm.2013.ds.filepath)

ods.2014.ws <- loadWorkbook(ods.2014.ws.filepath)
ods.2013.ds <- loadWorkbook(ods.2013.ds.filepath)
ods.2013.ws <- loadWorkbook(ods.2013.ws.filepath)

tmn.2014.ws <- loadWorkbook(tmn.2014.ws.filepath)
tmn.2013.ds <- loadWorkbook(tmn.2013.ds.filepath)
tmn.2013.ws <- loadWorkbook(tmn.2013.ws.filepath)

tha.2013.ds <- loadWorkbook(tha.2013.ds.filepath)
tha.2013.ws <- loadWorkbook(tha.2013.ws.filepath)

idn.2014.ds <- loadWorkbook(idn.2014.ds.filepath)
idn.2013.ds <- loadWorkbook(idn.2013.ds.filepath)
idn.2013.ws <- loadWorkbook(idn.2013.ws.filepath)

####
vnm.2014.ws.injuries <- readWorksheet(vnm.2014.ws, sheet = "injuries")
vnm.2013.ds.injuries <- readWorksheet(vnm.2013.ds, sheet = "injuries")

tmn.2014.ws.injuries <- readWorksheet(tmn.2014.ws, sheet = "injuries")
tmn.2013.ds.injuries <- readWorksheet(tmn.2013.ds, sheet = "injuries")
tmn.2013.ws.injuries <- readWorksheet(tmn.2013.ws, sheet = "injuries")


ods.2014.ws.injuries <- readWorksheet(ods.2014.ws, sheet = "injuries")
ods.2013.ds.injuries <- readWorksheet(ods.2013.ds, sheet = "injuries")
ods.2013.ws.injuries <- readWorksheet(ods.2013.ws, sheet = "injuries")

tha.2013.ws.injuries <- readWorksheet(tha.2013.ws, sheet = "injuries")
tha.2013.ds.injuries <- readWorksheet(tha.2013.ds, sheet = "injuries")


idn.2014.ds.injuries <- readWorksheet(idn.2014.ds, sheet = "injuries")
idn.2013.ds.injuries <- readWorksheet(idn.2013.ds, sheet = "injuries")
idn.2013.ws.injuries <- readWorksheet(idn.2013.ws, sheet = "injuries")

##### check  and and more column for more consistancy ####
idn.2013.ds.injuries$fno <- rep(1:14, c(rep(20,14)))
idn.2013.ds.injuries$location <-  rep("idn",nrow(idn.2013.ds.injuries))
idn.2013.ds.injuries$year <-  rep("2013",nrow(idn.2013.ds.injuries))
idn.2013.ds.injuries$season <-  rep("ds",nrow(idn.2013.ds.injuries))

idn.2014.ds.injuries$fno <- rep(1:15, c(rep(24,15)))
idn.2014.ds.injuries$location <-  rep("idn",nrow(idn.2014.ds.injuries))
idn.2014.ds.injuries$year <-  rep("2014",nrow(idn.2014.ds.injuries))
idn.2014.ds.injuries$season <- rep("ds",nrow(idn.2014.ds.injuries))

idn.2013.ws.injuries$fno <- rep(1:15, c(rep(24,15)))
idn.2013.ws.injuries$location <-  rep("idn",nrow(idn.2013.ws.injuries))
idn.2013.ws.injuries$year <-  rep("2013",nrow(idn.2013.ws.injuries))
idn.2013.ws.injuries$season <-  rep("ws",nrow(idn.2013.ws.injuries))
idn.2013.ws.injuries$fieldno <- NULL
### ind tmn ####

tmn.2013.ds.injuries$fno <- rep(1:15, c(rep(20,15)))
tmn.2013.ds.injuries$location <-  rep("tmn",nrow(tmn.2013.ds.injuries))
tmn.2013.ds.injuries$year <-  rep("2013",nrow(tmn.2013.ds.injuries))
tmn.2013.ds.injuries$season <-  rep("ds",nrow(tmn.2013.ds.injuries))
tmn.2013.ds.injuries$fieldno <- NULL

tmn.2013.ws.injuries$fno <- rep(1:15, c(rep(20,15)))
tmn.2013.ws.injuries$location <-  rep("tmn",nrow(tmn.2013.ws.injuries))
tmn.2013.ws.injuries$year <-  rep("2013",nrow(tmn.2013.ws.injuries))
tmn.2013.ws.injuries$season <-  rep("ws",nrow(tmn.2013.ws.injuries))
tmn.2013.ws.injuries$fieldno <- NULL

tmn.2014.ws.injuries$fno <- rep(1:15, c(rep(24,15)))
tmn.2014.ws.injuries$location <-  rep("tmn",nrow(tmn.2014.ws.injuries))
tmn.2014.ws.injuries$year <-  rep("2014",nrow(tmn.2014.ws.injuries))
tmn.2014.ws.injuries$season <-  rep("ws",nrow(tmn.2014.ws.injuries))
tmn.2014.ws.injuries$Fno <- NULL
tmn.2014.ws.injuries$fieldno <- NULL

##### ind ods #####

ods.2013.ds.injuries$fno <- rep(1:15, c(rep(20,15)))
ods.2013.ds.injuries$location <-  rep("ods",nrow(ods.2013.ds.injuries))
ods.2013.ds.injuries$year <-  rep("2013",nrow(ods.2013.ds.injuries))
ods.2013.ds.injuries$season <-  rep("ds",nrow(ods.2013.ds.injuries))
ods.2013.ds.injuries$fieldno <- NULL
ods.2013.ds.injuries$Fno <- NULL

ods.2013.ws.injuries$fno <- rep(1:12, c(rep(20,12)))
ods.2013.ws.injuries$location <-  rep("ods",nrow(ods.2013.ws.injuries))
ods.2013.ws.injuries$year <-  rep("2013",nrow(ods.2013.ws.injuries))
ods.2013.ws.injuries$season <-  rep("ws",nrow(ods.2013.ws.injuries))
ods.2013.ws.injuries$fieldno <- NULL
ods.2013.ws.injuries$Fno <- NULL

ods.2014.ws.injuries$fno <- rep(1:15, c(rep(24,15)))
ods.2014.ws.injuries$location <-  rep("ods",nrow(ods.2014.ws.injuries))
ods.2014.ws.injuries$year <-  rep("2014",nrow(ods.2014.ws.injuries))
ods.2014.ws.injuries$season <-  rep("ws",nrow(ods.2014.ws.injuries))
ods.2014.ws.injuries$Fno <- NULL
ods.2014.ws.injuries$fieldno <- NULL
##### tha ####

tha.2013.ds.injuries$fno <- rep(1:20, c(rep(24,20)))
tha.2013.ds.injuries$location <-  rep("tha",nrow(tha.2013.ds.injuries))
tha.2013.ds.injuries$year <-  rep("2013",nrow(tha.2013.ds.injuries))
tha.2013.ds.injuries$season <-  rep("ds",nrow(tha.2013.ds.injuries))
tha.2013.ds.injuries$Fno <- NULL
tha.2013.ds.injuries$fieldno <- NULL

tha.2013.ws.injuries$fno <- rep(1:20, c(rep(24,20)))
tha.2013.ws.injuries$location <-  rep("tha",nrow(tha.2013.ws.injuries))
tha.2013.ws.injuries$year <-  rep("2013",nrow(tha.2013.ws.injuries))
tha.2013.ws.injuries$season <-  rep("ws",nrow(tha.2013.ws.injuries))
tha.2013.ws.injuries$Fno <- NULL
tha.2013.ws.injuries$fieldno <- NULL
##### vnm #####

vnm.2013.ds.injuries$fno <- rep(1:30, c(rep(24,30)))
vnm.2013.ds.injuries$location <-  rep("vnm",nrow(vnm.2013.ds.injuries))
vnm.2013.ds.injuries$year <-  rep("2013",nrow(vnm.2013.ds.injuries))
vnm.2013.ds.injuries$season <-  rep("ds",nrow(vnm.2013.ds.injuries))
vnm.2013.ds.injuries$Fno <- NULL
vnm.2013.ds.injuries$fieldno <- NULL

vnm.2014.ws.injuries$fno <- rep(1:30, c(rep(24,30)))
vnm.2014.ws.injuries$location <-  rep("vnm",nrow(vnm.2014.ws.injuries))
vnm.2014.ws.injuries$year <-  rep("2014",nrow(vnm.2014.ws.injuries))
vnm.2014.ws.injuries$season <-  rep("ws",nrow(vnm.2014.ws.injuries))
vnm.2014.ws.injuries$Fno <- NULL
vnm.2014.ws.injuries$fieldno <- NULL

#### bind all ####

all.inj <- bind_rows(idn.2013.ds.injuries,
                     idn.2014.ds.injuries,
                     idn.2013.ws.injuries,
                     tmn.2013.ds.injuries,
                     tmn.2013.ws.injuries,
                     tmn.2014.ws.injuries,
                     ods.2013.ds.injuries,
                     ods.2013.ws.injuries,
                     ods.2014.ws.injuries,
                     tha.2013.ds.injuries,
                     tha.2013.ws.injuries,
                     vnm.2013.ds.injuries,
                     vnm.2014.ws.injuries
        )
# eos
