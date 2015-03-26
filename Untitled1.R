library(XLConnect)
library(dplyr)
file <- list.files(path = paste("~/Google Drive/4.SKEP2ProjectData/survey.SYN/", sep = ""), pattern = "SKEP2[[:graph:]]+.xls$", full.names = TRUE)
source("Functions/function.audpc.R")

options(digits=2)
data <- loadWorkbook(file[3])

injuries.data <- readWorksheet(data, 
                               sheet = "injuries", 
                               header = TRUE)

injuries.data %>% group_by(fieldno, visit) %>%
        select(fieldno, DVS, WS) %>%
        distinct(DVS,WS)

plats <- injuries.data %>% group_by(fieldno) %>%
        summarise(
                Ntmax = max(Nt),
                Npmax = max(Np),
                Nltmax =max(Nlt)
        )

write.csv(plats, file = "VNM.plats.csv")
        
