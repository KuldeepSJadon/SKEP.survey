#load yield data from 1-yieldcombine.R
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

country <- "Indonesia"
sseason <- "Wet Season"

##### Computation ####
ydata <- yield %>% filter(location == country  & season == sseason)

y_in_box <- ydata %>%
        filter(ydata$ymean > boxplot(ydata$ymean)$stat[2, ] & ydata$ymean < boxplot(ydata$ymean)$stat[4, ])

y_low_box <- ydata %>%
        filter(ydata$ymean < boxplot(ydata$ymean)$stats[2, ])

y_high_box <- ydata %>%
        filter(ydata$ymean > boxplot(ydata$ymean)$stat[4, ])

#eos
