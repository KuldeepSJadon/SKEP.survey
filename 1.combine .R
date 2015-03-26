##############################################################################
# titile        : 1.combine.R;
# purpose       : combine data from excel file to csv file;
# producer      : Sith Jaisong and A. H. Sparks;
#                 Plant Disease Management team
#                 CESD, IRRI;
# last update   : In Los Baños, 1 Jul. 2014;
# inputs        : Microsoft Excel file in each replication and treatment;
# outputs       : data frame named sheet1, sheet2, sheet3 and sheet4
# remarks 1     : sheet 1 is data of production situation and leaf and tiller injuries
#                 sheet 2 is the systemic injuries
#                 sheet 3 is the seed infestation
#                 sheet 4 is the yield samples;;
# Licence:      : GPL2;
##############################################################################

#### Begin load libraries ####
library(XLConnect)
library(dplyr)
library(reshape)
library(reshape2)
#### End load libraries ####

#### Load functions ####
source("Functions/function.audpc.R")
#### End load functions #####

# Select the country you are inquring
#country <- "Indonesia"
#country <- "India" 
#country <- "Vietnam" 
#country <- "Thailand" 


file <- list.files(path = "~/Google Drive/4.SKEP2ProjectData/Farm Survey/", pattern = "2013[[:graph:]]+.xls$", full.names = TRUE)

nfile <- length(file)
             
 data.all.sheet1 <- list() # strore the data sheet1 of all file in the lists
 data.all.sheet2 <- list() # strore the data sheet2 of all file in the lists
 data.all.sheet3 <- list() # strore the data sheet3 of all file in the lists
 data.all.sheet4 <- list() # strore the data sheet4 of all file in the lists

# Import file from google drive, Pls find the data in share folder, and change the user name, foe example from iSith to your user
       
data <- loadWorkbook(file[1]) # load excel file 
        ## one excel file composed of 4 sheets of data
        # load data sheet 1 the injuries on leave and tiller or hill, combine and merge all the data
         practice.data <- readWorksheet(data, 
                                      sheet = "practice", startRow = 1, startCol =1, header = TRUE) 
            # load data sheet2 the systemic injuires such as viral disease , and hopperburn caused by brown planthopper , save as list
         data.sheet1<- readWorksheet(data, 
                                     sheet = "injuries", startRow = 1, startCol =1, header = TRUE)

         data.sheet2<- readWorksheet(data, 
                                     sheet= "systemic", startRow = 1, startCol =1, header = TRUE) 
         data.sheet3<- readWorksheet(data, 
                                     sheet= "weed", startRow = 1, startCol =1, header = TRUE)
        data.sheet4<- readWorksheet(data, 
                                    sheet= "yield", startRow = 1, startCol =1, header = TRUE)


#####---- Combine all data by sheet
 sheet1 <- do.call("rbind", data.all.sheet1) # merge the list in one 
 sheet2 <- do.call("rbind", data.all.sheet2) # merge the list in one 
 sheet3 <- do.call("rbind", data.all.sheet3) # merge the list in one 
 sheet4 <- do.call("rbind", data.all.sheet4) # merge the list in one 

#####--- Save the data for use in the next analysis
save(sheet1, sheet2, sheet3, sheet4, file = "alldata.RData")
 
# eos
