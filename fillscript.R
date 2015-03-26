# ##############################################################################
# titile        : 1????.combine.R;
# purpose       : combine data from excel file to csv file;
# producer      : Sith Jaisong and A. H. Sparks;
#                 Plant Disease Management team
#                 CESD, IRRI;
# last update   : In Los Baños, 1 Nov. 2014;
# inputs        : Microsoft Excel file in each replication and treatment;
# outputs       : data frame named practice, ferilizer, chemical, injuries, systemic, weed, yield.
# remarks 1     : pactice is data of production situation and leaf and tiller injuries
#                 fertilizer is the systemic injuries
#                 chemical is the seed infestation
#                 injuries is the yield samples;
#                 systemic is the sheet of 
#                 weed is the sheet of weed infastrtion data
#                 yield is the sheet of yield sample in 2 x 2.5 or 5 m
# Licence:      : GPL2;
##############################################################################

# Start loading libraries ####
library(XLConnect)  # function for read xls file
library(dplyr) # manipulate the data
library(plyr) # manipulate the data
library(reshape) # manipulate the data
library(reshape2) # manipulate the data
library(ggplot2) # ggplot graph
#### End load libraries ####

#### Load functions ####
source("Functions/function.audpc.R")
#### End load functions #####

# Select the country you are inquring
#country <- "Indonesia"
#country <- "India" 
#country <- "Vietnam" 
#country <- "Thailand" 


file <- list.files(path = paste("~/Google Drive/4.SKEP2ProjectData/survey.SYN/", sep = ""), pattern = "SKEP2[[:graph:]]+.xls$", full.names = TRUE)

nfile <- length(file)

data.all.injuries <- list()

for( i in 1:nfile){

data <- loadWorkbook(file[i])

injuries.data <- readWorksheet(data, 
                            sheet = "injuries", 
                            header = TRUE)

data.all.injuries[[i]] <- injuries.data
}


injuries <- do.call("rbind", data.all.injuries)
        





sum.inj <- injuries %>% 
        # add the new column to store the variables converted to percent            
        mutate(Nlh = Nt*Nlt, # Number of leave = number of tiller * number of leave per tiller
               # tiller injuries
               DH.percent = (DH/Nt)*100, # Percent of Dead Heart in on hill is number tiller demaged by  dead heart divide by number of tiller *100 
               RT.percent = RT/Nt*100, # Percent of Rat damage in one hill
               #SN.percent = SN/Nt*100, # Percent of Snail damage in one hill
               RB.percent = RB/Nt*100, # Percent of Rice Bug injuries in one hill
               SS.percent = SS/Nt*100, # Percent of Silvershoot in one hill
               WH.percent = WH/Nt*100, # Percent of Whitehead in one hill
               PM.percent = PM/Nt*100, # Percent of panicle mite in one hill
               DP.percent = DP/Nt*100, # Percent of Dirty Panicle in one hill
               FSm.percent = FSm/Nt*100, # Percent of False smut in one hill
               NB.percent = NB/Nt*100, # Percent of Neck Blast in one hill
               ShB.percent = ShB/Nt*100, # Percent of Shealth Blight injuries in one hill
               ShR.percent = ShR/Nt*100, # Percent of Shealth Rot in one hill
               # leave injuries
               LF.percent = LF/Nlh*100, # Percent of Leaffolder in one hill
               LM.percent = LM/Nlh*100, # Percent of Leaf miner in one hill
               RH.percent = RH/Nlh*100, # Percent of Rice hispa in one hill
               WM.percent = WM/Nlh*100, # Percent of Whorl maggot injuries in one hill
               #Defo.percent = Defo/Nlh*100, # Percent of Defoliator in one hill
               #Thrip.percent = Thrip/Nlh*100, # Percent of Thrip in one hill
               BLB.percent = BLB/Nlh*100, # Percent of Bacterial leaf Blight in one hill
               BLS.percent = BLS/Nlh*100, # Percent of Bacterial leaf streak in one hill
               BS.percent = BS/Nlh*100, # Percent of Brown Spot in one hill
               LB.percent = LB/Nlh*100, # Percent of leaf Blight in one hill
               LS.percent = LS/Nlh*100, # Percent of leaf scald in one hill
               NBS.percent = NBS/Nlh*100, # Percent of Narrow brown spot in one hill
               RS.percent = RS/Nlh*100 # Percent of Red stripe in one hill
        ) %>%
        
        # group the variable such as season, year , teatment, rep and DVS
        group_by(fieldno, visit) %>%
        
        summarise(m.LF = mean(LF.percent), # mean within DVS which is following the designed group
                  m.LM = mean(LM.percent),
                  m.RH = mean(RH.percent),
                  m.WM = mean(WM.percent),
                  #m.Defo = mean(Defo.percent),
                  m.BLB = mean(BLB.percent),
                  m.BLS = mean(BLS.percent),
                  m.BS = mean(BS.percent)
                  m.LB = mean(LB.percent),
                  m.LS = mean(LS.percent),
                  m.NBS = mean(NBS.percent),
                  m.RS = mean(RS.percent),
                  m.DH = max(DH.percent),
                  m.RT = max(RT.percent),
                  m.RB = max(RB.percent),
                  m.SS = max(SS.percent),
                  m.WH = max(WH.percent),
                  m.PM = max(PM.percent),
                  m.DP = max(DP.percent),
                  m.FSm = max(FSm.percent),
                  m.NB = max(NB.percent),
                  m.ShB = max(ShB.percent),
                  m.ShR = max(ShR.percent)
        )

