#====================================================================
######## Load Pesticide application data #####
#====================================================================
<<<<<<< Updated upstream
## Load package
library(XLConnect)
##### combine all the file of pesticide application all countries ######
file <- list.files(path = "~/Google Drive/Data/SYT-SKEP/Survey/", pattern = "20[[:graph:]]+.xlsx$", full.names = TRUE)
=======
## Load package 
library(XLConnect)
##### combine all the file of pesticide application all countries ######
file <- list.files(path = paste("~/Google Drive/Data/SYT-SKEP/Survey", sep = ""), pattern = "20[[:graph:]]+.xlsx$", full.names = TRUE)
>>>>>>> Stashed changes

#### analysis of pesticide application #####
pesticide.list <- list()
options(java.parameters = "-Xmx1024m")

for(i in 1: length(file)){
        filepath <- file[i]
        workbook <- loadWorkbook(filepath)
        pesticide <- readWorksheet(workbook, sheet = "pesticide",
                                   startCol = 1, endCol = 13,
                                   forceConversion = TRUE)
        col_names <- names(pesticide)
        pesticide[,col_names] <- lapply(pesticide[,col_names] , factor)
        pesticide.list[[i]] <- pesticide
}

all.pesticide <- do.call("bind_rows", pesticide.list)

# correct data format
all.pesticide$location <- tolower(all.pesticide$location)
all.pesticide$season <- tolower(all.pesticide$season)

all.pesticide <- all.pesticide %>%
        transform(location = as.factor(location),
                  year = as.factor(location),
                  season = as.factor(season),
                  moll = as.factor(moll),
                  wmg = as.factor(wmg),
                  herb = as.factor(herb),
                  wmg.dvs = as.factor(wmg.dvs),
                  insect = as.factor(insect),
                  ins.dvs = as.factor(ins.dvs),
                  fung = as.factor(fung),
                  fung.dvs = as.factor(fung.dvs)
        )
<<<<<<< Updated upstream

#eos
=======
>>>>>>> Stashed changes
