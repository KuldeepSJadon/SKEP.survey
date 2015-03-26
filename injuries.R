#This script I used it for combine the survey data for Ben 
# created in Nov, 25 2014
# By Sith

library(XLConnect)
library(dplyr)
file <- list.files(path = paste("~/Google Drive/4.SKEP2ProjectData/survey.SYN/", sep = ""), pattern = "SKEP2[[:graph:]]+.xls$", full.names = TRUE)
source("Functions/function.audpc.R")

options(digits=2)
data <- loadWorkbook(file[2])

injuries.data <- readWorksheet(data, 
                               sheet = "injuries", 
                               header = TRUE)
#data.sheet1

IND.ods.injuries <- injuries.data %>% 
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
               RS.percent = RS/Nlh*100) %>%
        
        group_by(fieldno, DVS) %>%
        
        summarise( 
               mDH = mean(DH.percent),
               mWH = mean(WH.percent),
               mSS = mean(SS.percent),
               mRT = mean(RT.percent),
               mRB = mean(RB.percent),
                mLF = mean(LF.percent), # mean within DVS which is following the designed group
               mLM = mean(LM.percent),
               mRH = mean(RH.percent),
               mWM = mean(WM.percent),
               #m.Defo = mean(Defo.percent),
               mBLB = mean(BLB.percent),
               mBLS = mean(BLS.percent),
               mBS = mean(BS.percent),
               mLB = mean(LB.percent),
               mLS = mean(LS.percent),
               mNBS = mean(NBS.percent),
               mRS = mean(RS.percent),
               mShB = mean(ShB.percent),
               mShR = mean(ShR.percent),
               mFSm = mean(FSm.percent),
               mNB = mean(NB.percent), 
               mDP = mean(DP.percent), 
               mPM = mean(PM.percent)
               ) %>%
# Percent of Red stripe in one hill


        # group the variable such as season, year , teatment, rep and DVS
        group_by(fieldno) %>%
        
        summarise( 
                DHX = max(mDH),
                WHW = max(mWH),
                SSX = max(mSS),
                RTX = max(mRT),
                RBX = max(mRB),
                WMA = audpc(mWM, DVS),
                LFA = audpc(mLF, DVS),
                LMA = audpc(mLM, DVS),
                RHA = audpc(mRH, DVS),
                #Defo.audpc = audpc(m.Defo, DVS),
                BLBA = audpc(mBLB, DVS),
                LBA = audpc(mLB, DVS),
                BSA = audpc(mBS, DVS),
                BLSA = audpc(mBLS, DVS),
                NBSA = audpc(mNBS, DVS),
                RSA = audpc(mRS, DVS),
                LSA = audpc(mLS, DVS),
                ShBX = max(mShB),
                ShRX = max(mShR),
                FSmX = max(mFSm),
                NBX = max(mNB), 
                DPX = max(mDP), 
                PMX = max(mPM)
                )

write.csv(IND.ods.injuries, file = "IND.ods.inj.csv")
