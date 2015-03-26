sheet1$DVS<- as.character(sheet1$DVS)
sheet1$Wstress <- ifelse(sheet1$WS == 1 & sheet1$DVS == "100", 0, 
                ifelse(sheet1$WS == 2 & sheet1$DVS == "100", 0,
                       ifelse(sheet1$WS == 3 & sheet1$DVS == c("70", "80" ,"90", "100"), 0,
                              ifelse(sheet1$WS == 4 & sheet1$DVS == "100", 0 ,
                                     ifelse(sheet1$WS == 5 & sheet1$DVS == c("70", "80" ,"90", "100"), 0,
                                            ifelse(sheet1$WS == 6 & sheet1$DVS =="100", 0,
                                                   ifelse(sheet1$WS == 7, 0,
                                                          ifelse(sheet1$WS == 8 & sheet1$DVS == c("70", "80" ,"90", "100"), 0,
                                                                 ifelse(sheet1$WS == 9, 0,
                                                                        ifelse(sheet1$WS == 10, 1,
                                                                               ifelse(sheet1$WS == 11 & sheet1$DVS == c("30", "40", "50", "60"), 0,
                                                                                      ifelse(sheet1$WS == 11 & sheet1$DVS == c("70", "80", "90", "100"), 1,
                                                                                             ifelse(sheet1$WS == 11 & sheet1$DVS == c("30", "40", "50", "60"), 0, -1
                                                                                      )))))))))))))
                

write.csv(sheet1, file = "sheet1.csv")
