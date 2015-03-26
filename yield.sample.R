sum.sheet4 <- data.sheet4 %.% 
        mutate(m.Y = (Y/MC)*14, yield.kg.ha = m.Y*2000) %>%
        group_by(season, year, treatment, rep) %>% 
        summarise( mean.ykh = mean(yield.kg.ha))