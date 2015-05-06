###############################################################################
#'title         : technical correction of injury data
#'date          : May, 2015
#'purpose       : load data from the shared Google Drive which is Excel
#'                format
#'author        : Sith Jaisong (s.jaisong@irri.org)
#'modified by   : Adam Sparks (a.sparks@irri.org), 6 May 2015
#'contact       : s.jaisong@irri.org
#'input         : data from 12-injuires.R
#'output        : gg-graph of injuiry data by country
###############################################################################

##### Loading packages
library(dplyr)
library(ggplot2)
library(reshape)
library(reshape2)
##### End of loading packages

# Load data
source("12-injures.R")

##########################
x.all.inj <- all.inj %>%
        mutate(Nlh = Nt*Nlt, # Number of leave = number of tiller * number of leave per tiller
               # tiller injuries
               DH.percent = (DH/Nt)*100, # Percent of Dead Heart in on hill is number tiller demaged by  dead heart divide by number of tiller *100
               RT.percent = RT/Nt*100, # Percent of Rat damage in one hill
               # SNL.percent = SNL/Nt*100, # Percent of Snail damage in one hill
               RB.percent = RB/Nt*100, # Percent of Rice Bug injuries in one hill
               SS.percent = SS/Nt*100, # Percent of Silvershoot in one hill
               WH.percent = WH/Nt*100, # Percent of Whitehead in one hill
               PM.percent = PM/Nt*100, # Percent of panicle mite in one hill
               DP.percent = DP/Nt*100, # Percent of Dirty Panicle in one hill
               FSm.percent = FSm/Nt*100, # Percent of False smut in one hill
               NB.percent = NB/Nt*100, # Percent of Neck Blast in one hill
               ShB.percent = ShB/Nt*100, # Percent of Shealth Blight injuries in one hill
               ShR.percent = ShR/Nt*100, # Percent of Shealth Rot in one hill
               BKN.percent = BKN/Nt*100, # Percent of Bakanae in one hill
               # leave injuries
               LF.percent = LF/Nlh*100, # Percent of Leaffolder in one hill
               LM.percent = LM/Nlh*100, # Percent of Leaf miner in one hill
               RH.percent = RH/Nlh*100, # Percent of Rice hispa in one hill
               WM.percent = WM/Nlh*100, # Percent of Whorl maggot injuries in one hill
               THR.percent = THR/Nlh*100, # Percent of Thrip in one hill
               BLB.percent = BLB/Nlh*100, # Percent of Bacterial leaf Blight in one hill
               BLS.percent = BLS/Nlh*100, # Percent of Bacterial leaf streak in one hill
               BS.percent = BS/Nlh*100, # Percent of Brown Spot in one hill
               LB.percent = LB/Nlh*100, # Percent of leaf Blight in one hill
               LS.percent = LS/Nlh*100, # Percent of leaf scald in one hill
               NBS.percent = NBS/Nlh*100, # Percent of Narrow brown spot in one hill
               RS.percent = RS/Nlh*100 # Percent of Red stripe in one hill
        ) %>%
        group_by(fno, season, year , location, visit) %>%
        summarise(#m.RT = mean(RT.percent), # Percent of Rat damage in one hill
                #    m.SNL = mean(SNL.percent), # Percent of Snail damage in one hill
                m.DH = mean(DH.percent),
                m.RB = mean(RB.percent), # Percent of Rice Bug injuries in one hill
                m.SS = mean(SS.percent), # Percent of Silvershoot in one hill
                m.WH = mean(WH.percent), # Percent of Whitehead in one hill
                m.PM = mean(PM.percent), # Percent of panicle mite in one hill
                m.DP = mean(DP.percent), # Percent of Dirty Panicle in one hill
                m.FSm = mean(FSm.percent), # Percent of False smut in one hill
                m.NB = mean(NB.percent), # Percent of Neck Blast in one hill
                m.ShB = mean(ShB.percent), # Percent of Shealth Blight injuries in one hill
                m.ShR = mean(ShR.percent), # Percent of Shealth Rot in one hill
                # m.BKN = mean(BKN.percent),
                m.LF = mean(LF.percent), # mean within DVS which is following the designed group
                m.LM = mean(LM.percent),
                m.RH = mean(RH.percent),
                m.WM = mean(WM.percent),
                m.THR = mean(THR.percent),
                m.BLB = mean(BLB.percent),
                m.BLS = mean(BLS.percent),
                m.BS = mean(BS.percent),
                m.LB = mean(LB.percent),
                #  m.LS = mean(LS.percent),
                m.NBS = mean(NBS.percent),
                m.RS = mean(RS.percent)
        )


m.x.all.inj <- melt(as.data.frame(x.all.inj), id.vars = c("fno","season", "year", "location", "visit"))

m.x.all.inj$season <- as.factor(m.x.all.inj$season)
m.x.all.inj$visit <- as.factor(m.x.all.inj$visit)
m.x.all.inj$location <- as.factor(m.x.all.inj$location)

levels(m.x.all.inj$visit)[levels(m.x.all.inj$visit) == "1"] <- "TR"
levels(m.x.all.inj$visit)[levels(m.x.all.inj$visit) == "2"] <- "AR"
levels(m.x.all.inj$season)[levels(m.x.all.inj$season) == "ds"] <- "Dry"
levels(m.x.all.inj$season)[levels(m.x.all.inj$season) == "ws"] <- "Wet"
levels(m.x.all.inj$location)[levels(m.x.all.inj$location) == "idn"] <- "Indonesia"
levels(m.x.all.inj$location)[levels(m.x.all.inj$location) == "ods"] <- "Odisha"
levels(m.x.all.inj$location)[levels(m.x.all.inj$location) == "tmn"] <- "Tamil Nadu"
levels(m.x.all.inj$location)[levels(m.x.all.inj$location) == "vnm"] <- "Vietnam"
levels(m.x.all.inj$location)[levels(m.x.all.inj$location) == "tha"] <- "Thailand"

#lfinj <- c("m.LF", "m.WM", "m.THR", "m.BLB", "m.NBS", "m.RS", "m.BS", "m.LB", "m.LS", "m.BLS", "m.RH", "m.LM")

#m.x.all.inj$injtyp <- ifelse(m.x.all.inj$variable %in% lfinj, "Leaf Injuries", "Tiller Injuries"
)

insectinj <- c("m.RB", "m.SS", "m.WH", "m.DH", "m.WM", "m.LF", "m.TH", "m.LM", "m.RH")
m.x.all.inj$injtyp <- ifelse(m.x.all.inj$variable %in% insectinj, "Insect Injuries", "Disease"
)

levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.RT"] <-  "Rat"

levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.RB"] <- "Rice bug"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.SS"] <- "Silver shoot"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.WH"] <- "Whitehead"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.DP"] <- "Dirty panicle"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.FSm"] <- "False smut"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.NB"] <- "Neck blast"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.ShB"] <- "Sheath blight"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.ShR"] <- "Sheath rot"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.LF"] <- "Leaffolder"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.WM"] <- "Whorl maggot"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.THR"] <- "Thrip"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.BLB"] <- "Bacterial leaf blight"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.NBS"] <- "Narrow brown spot"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.RS"] <- "Red Stripe"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.PM"] <- "Panicle mite"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.LM"] <- "Leaf miner"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.RH"] <- "Rice hispa"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.LB"] <- "Leaf blast"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.BLS"] <- "Bacterial leaf streak"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.LS"] <- "Leaf scald"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.BS"] <- "Brown spot"
levels(m.x.all.inj$variable)[levels(m.x.all.inj$variable) == "m.DH"] <- "Deadheart"


#=======================================
# Injuries Profiles by Country
#=======================================
m.x.all.inj1 <- m.x.all.inj %>% filter( location == "Odisha")

#Indonesia
#Odisha
#Tamil Nadu
#Vietnam
#Thailand

allinju <- m.x.all.inj  %>% group_by(location, year, season, variable) %>%
        summarise(mv = mean(value))
ggplot(data = m.x.all.inj1, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  facet_grid(season*year ~ ., scale = "free", space = "free") +
  ylim(c(1,100)) +
  #scale_fill_brewer(palette= "Set3") +
  labs( fill = " Injuries") +
  #  mytheme +
  xlab("Injuries") +
  ylab("Incidence (%)")+
  ggtitle("Injury profiles of Indonesiaa survey data from 2013 to 2014")
# just edit tyhe color and the legend the
#ggsave(filename = "pic/inj.idn.survey.png", width = 14, height = 10)

ggsave("Indonesia_injury_profile.png", width = 10, height = 7, units = "in")

ggplot(data = subset(m.x.all.inj1, variable != "Rice bug" & variable != "Rice hispa" & variable != "Panicle mite" & variable != "Thrip" & variable != "Dirty panicle" & variable != "Red Stripe" & variable != "Leaf miner" & variable != "Neck blast"), aes(x = variable, y = value)) +
  geom_boxplot(aes(colour = variable, fill = variable), alpha = 0.65, outlier.size = 2) +
  facet_grid(season ~ visit, scale = "free", space = "free") +
  ylim(c(1, 100)) +
  xlab("Injury") +
  ylab("Incidence (%)")+
  ggtitle("Injury profile from Indonesia survey data\n2013 to 2014") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", angle = 35,
                                   size = 9,
                                   hjust = 1,
                                   vjust = 1),
        axis.text.y = element_text(face = "bold", size = 8),
        plot.title = element_text(face = "bold", size = 15),
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        legend.position = "none")

ggsave("Indonesia_injury_profile_growth_stage.png", width = 10, height = 7, units = "in")


#eos
