#load yield data from 11-yieldcombine.R
#
#==========================================================
##### select country  and season ##########################
#==========================================================
# idn = Indonesia
# tmn = Tamil Nadu
# ods = Odisha
# tha = Thailand
# vnm = Vietnam

# ds = dry season
# ws = wet season

country <- "vnm"
sseason <- "ws"

##### Computation ####
ydata <- yield %>% filter(location == country  & season == sseason)

y_in_box <- ydata %>% 
        filter(ymean > boxplot(ydata$ymean)$stat[2,] & ymean < boxplot(ymean$ymean)$stat[4,])

y_low_box <- ydata %>% 
        filter(ymean < boxplot(ydata$ymean)$stats[2,])

y_high_box <- ydata %>%
        filter(ymean > boxplot(ymean$ymean)$stat[4,])
