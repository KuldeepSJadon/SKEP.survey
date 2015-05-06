
#==========================================================
##### select country  and season ##########################
#==========================================================
# idn = Indonesia
# tmn = Tamil Nadu
# ods = Odisha
# tha = Thailand
# vnm = Vientnam

# ds = dry season
# ws = wet season

country <- "vnm"
sseason <- "ws"

##### Computation ####
ydata <- yield %>% filter(location == country  & season == sseason)

adap <- ydata %>% filter(ymean > 6)
major <- ydata %>% filter(ymean >= 4 & ymean <= 6)
drif <- ydata %>% filter(ymean < 4)
