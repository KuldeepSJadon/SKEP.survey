all.con <- rbind(IND.ODS.DS.sum.sheet1,
                 IND.THA.DS.sum.sheet1,
                 IND.ODS.WS.sum.sheet1,
                 IND.THA.DS.sum.sheet1,
                 IDN.DS.sum.sheet1,
                 IDN.WS.sum.sheet1,
                 THA.sum.sheet1,
                 VNM.sum.sheet1)

write.csv(all.con, file = "all.con.csv" )
