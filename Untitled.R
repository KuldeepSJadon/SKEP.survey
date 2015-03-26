sum.yield <- yield %>% 
        mutate(m.Y = (y.g.sqm./MC)*14) %>%
        group_by(fieldno) %>% 
        summarise(mean.ykh = mean(m.Y))
write.csv(sum.yield, file = "sum.yield.csv")


data <- loadWorkbook("allforben.xls")
weed <- readWorksheet(data, sheet = 3, header = T)

write.csv(sum.weed, file = "weed.csv")
