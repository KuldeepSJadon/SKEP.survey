
library(XLConnect)
library(dplyr)

file <- list.files(path = paste("~/Google Drive/4.SKEP2ProjectData/survey.SYN/", sep = ""), pattern = "SKEP2[[:graph:]]+.xls$", full.names = TRUE)
source("Functions/function.audpc.R")

options(digits=2)
data <- loadWorkbook(file[5])

weed <- readWorksheet(data, 
                               sheet = "weed", 
                               header = TRUE)
weed[,5:6][weed[,5:6] =="0"] <- 0
weed[,5:6][weed[,5:6] =="1"] <- 5
weed[,5:6][weed[,5:6] =="2"] <- 20
weed[,5:6][weed[,5:6] =="3"] <- 45
weed[,5:6][weed[,5:6] =="4"] <- 80


sum.weed <- weed %>%
        group_by(Fno,DVS)%>%
        # find mean 
        mutate(m.WA = mean(WA),
               m.WB = mean(WB)
               )%>%
        group_by(Fno)%>%
        summarise(WAA = audpc(m.WA, DVS),
                WBA = audpc(m.WB, DVS))

write.csv(sum.weed, file = "VNM.weed.csv")
#%>%
#        group_by(Fno)%>%
#        summarise( x.WA = audpc(m.WA, DVS),
#                   x.WB = audpc(m.WB, DVS)
#        )