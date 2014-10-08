##############################################################################
# titile        : 1.combine.R;
# purpose       : combine data from excel file to csv file;
# producer      : Sith Jaisong and A. H. Sparks;
#                 Plant Disease Management team
#                 CESD, IRRI;
# last update   : In Los Baños, 1 Jul. 2014;
# inputs        : data frame named  sheet1 the data of leave and tiller injuries of all treatments
#                                   sheet2 the data of systemic injuries of all treatments
#                                   sheet3 the data of weed infastration of all tratments
#                                   sheet4 the data of yield of all treatments
# outputs       : transfrom the raw data of each variables to the right unit
#                 such as the the number of leaf damaged by browns spot to AUDPC
#                         the yield kg per 5 m2 to ton per ha
# remarks 1     : 
# Licence:      : GPL2;
##############################################################################

##### Define output name
output.name <- " 2013WS.OFS.csv" # name your output

##### Load data #####
lnames <- load(file = "alldata.RData") # load RData from the previous step (1.combine.R)
# End load data #

#### Load functions ####
source("Functions/function.audpc.R")
# End load functions #

######----- Water status transformation from scale to percent 
# Water status condition : this formula will turn WSC to -1,0 and 1 
# following the water level and soil type and rice stage
# 1 : No standing water and soil dry and hard
# 2 : No standing water and soil moist and hard
# 3 : No standing water and soil moist and soft
#'4 : No standing water and soil wet and hard
#'5 : No stainding water and soil wet and soft
#'6 : water less than 5 cm and soil hard
#'7 : water less than 5 cm and soil soft
#'8 : water  between 5 and 15 cm , and soil hard
#'9 : water  between 5 and 15 cm, and soil soft
#'10 : water higher than 15 cm, and soil hard
#'11 : water higher than 15 cm, and soil soft
#'
##########

if(sheet1$WS == 1 & sheet1$DVS == "100"){  # if WS = 1 at harvest stage
        sheet1$WSC <- 0
} else {
        if(sheet1$WS == 2 & sheet1$DVS == "100"){ # if WS = 2 at harvest stage
                sheet1$WSC <- 0       
        } else {
                if(sheet1$WS == 3 & sheet1$DVS == c("70", "80" ,"90", "100")){ # if WS = 3 at 
                        sheet1$WSC <- 0
                } else {
                        if(sheet1$WS == 4 & sheet1$DVS == "100"){ # if WS = 4 at harvest stage
                                sheet1$WSC <- 0
                        } else {
                                if(sheet1$WS == 5 & sheet1$DVS == c("70", "80" ,"90", "100")){ # if WS = 5 at DVS = 70 , 80 , 90, 100  | WSC = 0
                                        sheet1$WSC <- 0
                                } else { 
                                        if(sheet1$WS == 6 & sheet1$DVS =="100"){ # if WS = 6 at harvest stage | WSC = 0
                                                sheet1$WSC <- 0
                                        } else {
                                                if( sheet1$WS == 7){ # if WS = 7 at any stage | WSC = 0
                                                        sheet1$WSC <- 0 
                                                } else {
                                                        if(sheet1$WS == 8 & sheet1$DVS == c("70", "80" ,"90", "100")){ # if WS =8 at DVS = 70 , 80 , 90, 100 | WSC = 0
                                                                sheet1$WSC <- 0 
                                                        } else {
                                                                if (sheet1$WS == 8 & sheet1$DVS == c( "10", "20", "30", "40", "50", "60")){ # if WS =8 at DVS = 10 , 20 , 30, 40, 50, 60 | WSC = 1
                                                                        sheet1$WSC <- 1
                                                                } else {
                                                                        if (sheet1$WS == 9){ # if WS = 9 at any stage | WSC = 0
                                                                                sheet1$WSC <- 0
                                                                        } else {
                                                                                if (sheet1$WS == 10){ # if WS = 10 at any stage | WSC = 1
                                                                                        sheet1$WSC <- 1
                                                                                } else {
                                                                                        if( sheet1$WS == 11 & sheet1$DVS == c("30", "40", "50", "60")){ # if WS = 11 at DVS = 30, 40, 50, 60 | WSC = 0
                                                                                                sheet1$WSC <- 0
                                                                                        } else { 
                                                                                                if( sheet1$WS == 11 & sheet1$DVS == c("10", "20", "70", "80", "90", "100")){ # if WS = 11 at DVS = 10, 20, 70, 80 | WSC = 1
                                                                                                        sheet1$WSC <- 1
                                                                                                } else { sheet1$WSC <- -1}
                                                                                        }}}}}}}}}}}}


######----- Analysis sheet 1 leave and tiller injuries-----######

## add more 
sum.sheet1 <- sheet1 %.% 
  # add the new column to store the variables converted to percent            
  mutate(Nlh = Nt*Nlt, # Number of leave = number of tiller * number of leave per tiller
                         # tiller injuries
                         DH.percent = (DH/Nt)*100, # Percent of Dead Heart in on hill is number tiller demaged by  dead heart divide by number of tiller *100 
                         RT.percent = RT/Nt*100, # Percent of Rat damage in one hill
                         SN.percent = SN/Nt*100, # Percent of Snail damage in one hill
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
                         Defo.percent = Defo/Nlh*100, # Percent of Defoliator in one hill
                         Thrip.percent = Thrip/Nlh*100, # Percent of Thrip in one hill
                         BLB.percent = BLB/Nlh*100, # Percent of Bacterial leaf Blight in one hill
                         BLS.percent = BLS/Nlh*100, # Percent of Bacterial leaf streak in one hill
                         BS.percent = BS/Nlh*100, # Percent of Brown Spot in one hill
                         LB.percent = LB/Nlh*100, # Percent of leaf Blight in one hill
                         LS.percent = LS/Nlh*100, # Percent of leaf scald in one hill
                         NBS.percent = NBS/Nlh*100, # Percent of Narrow brown spot in one hill
                         RS.percent = RS/Nlh*100 # Percent of Red stripe in one hill
                          ) %.%
  # group the variable such as season, year , teatment, rep and DVS
  group_by(season, year ,treatment, rep, DVS) %.%
  
  
  summarise(m.LF = mean(LF.percent), # mean within DVS which is following the designed group
            m.LM = mean(LM.percent),
            m.RH = mean(RH.percent),
            m.WM = mean(WM.percent),
            m.Defo = mean(Defo.percent),
            m.BLB = mean(BLB.percent),
            m.BLS = mean(BLS.percent),
            m.LB = mean(LB.percent),
            m.LS = mean(LS.percent),
            m.NBS = mean(NBS.percent),
            m.RS = mean(RS.percent)) %.%

  ### AUDPC # the functions from the agricolae is bugs for this format
# this part is to apply the function to computate the audpc of eheach variables 
  summarise(LF.audpc = audpc(m.LF, DVS),
            LM.audpc = audpc(m.LM, DVS),
            RH.audpc = audpc(m.RH, DVS),
            WM.audpc = audpc(m.WM, DVS),
            Defo.audpc = audpc(m.Defo, DVS),
            BLB.audpc = audpc(m.BLB, DVS),
            BLS.audpc = audpc(m.BLS, DVS),
            LB.audpc = audpc(m.LB, DVS),
            LS.audpc = audpc(m.LS, DVS),
            NBS.audpc = audpc(m.NBS, DVS),
            RS.audpc = audpc(m.RS, DVS))


######----- Analysis sheet 2 systemic injuies-----######
# select the maximum percent of mean along the time of experiment

sum.sheet2 <- sheet2 %.% 
  group_by(season, year ,treatment, rep) %.%
  summarise( xBB = max(mean(BB)),
             xHB = max(mean(HB)),
             xGSD = max(mean(GSD)),
             xRSD = max(mean(RSD)), 
             xRTV = max(mean(YSD))
            )

#####----- Analysis sheet 3 weed infastration-----#####
#Calcuation for Weed sheet3, especially WA and WB
# tranform from scale to percent
#' weed class 0 is 0 percent
#' weed class 1 is up to 10 percent, 
#' weed class 2 is 10 to 30 percent
#' weed class 3 is 30 to 60 percent
#' weed class 4 is 60 to 100 percent 
##########################################################

sheet3[,10:11][sheet3[,10:11] =="0"] <- 0
sheet3[,10:11][sheet3[,10:11] =="1"] <- 5
sheet3[,10:11][sheet3[,10:11] =="2"] <- 20
sheet3[,10:11][sheet3[,10:11] =="3"] <- 45
sheet3[,10:11][sheet3[,10:11] =="4"] <- 80

###
sum.sheet3 <- sheet3 %.%
  group_by(season, year ,treatment, rep, DVS)%.%
# find mean 
mutate(m.WA = mean(WA),
      m.WB = mean(WB),
      m.BLW = mean(BLW),
      m.G = mean(G),
      m.S = mean(S),
      m.SW = mean(SW))%.%
group_by(season, year, treatment, rep)%.%
summarise( x.WA = audpc(m.WA, DVS),
          x.WB = audpc(m.WB, DVS)
          )

#####----- Analysis sheet 4 Yield Evalustion-----#####

sum.sheet4 <- sheet4 %.% 
  mutate(m.Y = (Y/MC)*14, yield.kg.ha = m.Y*2000) %.%
  group_by(season, year, treatment, rep) %.% 
  summarise( mean.ykh = mean(yield.kg.ha))


### The combine output
output.list <- list(sum.sheet1, sum.sheet2, sum.sheet3 , sum.sheet4)
summary.data <- merge_recurse(output.list)
write.csv(summary.data, file = output.name)

# eos
