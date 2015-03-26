# conbine and create the graph

all.inj <- read.csv("all.con.csv")
all.inj <- all.inj[-1]
all.inj$visit <- factor(all.inj$visit)
all.inj$visit <-revalue(all.inj$visit, c( "1" = "Panicle initiation", "2" = "Harvesting"))

#all.inj.v1 <- filter(all.inj, visit == 1)
#all.inj.v2 <- filter(all.inj, visit == 2)

select.ins.leaf.inj <- select(all.inj, fieldno, visit, country, season, m.LF:m.WM)
names(select.ins.leaf.inj)[names(select.ins.leaf.inj) =="m.LF" ]  <- c("Leaf folder")
names(select.ins.leaf.inj)[names(select.ins.leaf.inj) =="m.LM" ]  <- c("Leaf miner")
names(select.ins.leaf.inj)[names(select.ins.leaf.inj) =="m.RH" ]  <- c("Rice hispa")
names(select.ins.leaf.inj)[names(select.ins.leaf.inj) =="m.WM" ]  <- c("Whorl maggot")

select.dis.leaf.inj <- select(all.inj, fieldno, visit, country, season, m.BLB:m.NBS)
names(select.dis.leaf.inj)[names(select.dis.leaf.inj) =="m.BLB" ]  <- c("Bacterial leaf blight")
names(select.dis.leaf.inj)[names(select.dis.leaf.inj) =="m.BLS" ]  <- c("Bacterial leaf streak")
names(select.dis.leaf.inj)[names(select.dis.leaf.inj) =="m.LB" ]  <- c("Leaf blast")
names(select.dis.leaf.inj)[names(select.dis.leaf.inj) =="m.LS" ]  <- c("Brown spot")
names(select.dis.leaf.inj)[names(select.dis.leaf.inj) =="m.NBS" ]  <- c("Narrow brown spot")
names(select.dis.leaf.inj)[names(select.dis.leaf.inj) =="m.RS" ]  <- c("Red stripe")

select.dis.pan.inj <- select(all.inj, fieldno, visit, country, season, m.PM:m.ShR)
names(select.dis.pan.inj)[names(select.dis.pan.inj) =="m.PM" ]  <- c("Panicle mite")
names(select.dis.pan.inj)[names(select.dis.pan.inj) =="m.DP" ]  <- c("Dirty Panicle")
names(select.dis.pan.inj)[names(select.dis.pan.inj) =="m.FSm" ]  <- c("False smut")
names(select.dis.pan.inj)[names(select.dis.pan.inj) =="m.NB" ]  <- c("Neck blast")
names(select.dis.pan.inj)[names(select.dis.pan.inj) =="m.ShB" ]  <- c("Sheath blight")
names(select.dis.pan.inj)[names(select.dis.pan.inj) =="m.ShR" ]  <- c("Sheath rot")

select.ins.pan.inj <- select(all.inj, fieldno, visit, country, season, m.DH:m.WH)
names(select.ins.pan.inj)[names(select.ins.pan.inj) =="m.DH" ]  <- c("Dead heart")
names(select.ins.pan.inj)[names(select.ins.pan.inj) =="m.RT" ]  <- c("Rat")
names(select.ins.pan.inj)[names(select.ins.pan.inj) =="m.RB" ]  <- c("Rice bug")
names(select.ins.pan.inj)[names(select.ins.pan.inj) =="m.SS" ]  <- c("Silver shoot")
names(select.ins.pan.inj)[names(select.ins.pan.inj) =="m.WH" ]  <- c("White head")

#===============================================================================
#
select.ins.pan.inj %>% 
        melt(id.vars = c("fieldno","visit","country","season")) %>%
        group_by(country, season, variable) %>%
        ggplot(aes(x = variable, y = value)) + 
        geom_boxplot(aes(fill = country), outlier.shape = NA) + 
        scale_y_continuous(limits = c(0, 100)) +
        facet_grid(visit ~ season) + 
        ggtitle("Tiller Injuries by Animal Pests in 2012 - 2014 by Country") + 
        ylab("Incidence(%)") +
        xlab("Injuires") +
        labs(fill = "Country")+
        theme(
                axis.title.x = element_text(colour = "black",size = 16),
                axis.text.x = element_text(angle = 30, 
                                           hjust =1, vjust =1, 
                                           colour = "black", face = "bold" ),
                axis.title.y = element_text(colour = "black",size = 16),
                axis.text.y = element_text(colour = "black", face = "bold"),
                strip.background = element_rect(fill = "pink"))

ggsave("till.inj.ins.plot.png", width = 36, height = 24, unit = "cm", dpi = 300)

#===============================================================================
#
select.ins.leaf.inj %>% 
        melt(id.vars = c("fieldno", "visit", "country","season")) %>%
        group_by(country, season, variable) %>%
        ggplot(aes(x = variable, y = value)) + 
        geom_boxplot(aes(fill = country), outlier.shape = NA) + 
        scale_y_continuous(limits = c(0, 100)) +
        facet_grid(visit~season) + 
        ggtitle("Leaf Injuries by Animal Pests in 2012 - 2014 by Country") + 
        ylab("Incidence(%)") +
        xlab("Injuires") +
        labs(fill = "Country")+
        theme(
                axis.title.x = element_text(colour = "black",size = 16),
                axis.text.x = element_text(angle = 30, 
                                           hjust =1, vjust =1, 
                                           colour = "black", face = "bold" ),
                axis.title.y = element_text(colour = "black",size = 16),
                axis.text.y = element_text(colour = "black", face = "bold"),
                strip.background = element_rect(fill = "pink"))

ggsave("leaf.inj.ins.plot.png", width = 36, height = 24, unit = "cm", dpi = 300)

#===============================================================================
#
select.dis.pan.inj %>% 
        melt(id.vars = c("fieldno", "visit", "country","season")) %>%
        group_by(country, season, variable) %>%
        ggplot(aes(x = variable, y = value)) + 
        geom_boxplot(aes(fill = country), outlier.shape = NA) + 
        scale_y_continuous(limits = c(0, 100)) +
        facet_grid(visit~season) + 
        ggtitle("Tiller Injuries by Disease in 2012 - 2014 by Country") + 
        ylab("Incidence(%)") +
        xlab("Injuires") +
        labs(fill = "Country")+
        theme(
                axis.title.x = element_text(colour = "black",size = 16),
                axis.text.x = element_text(angle = 30, 
                                           hjust =1, vjust =1, 
                                           colour = "black", face = "bold" ),
                axis.title.y = element_text(colour = "black",size = 16),
                axis.text.y = element_text(colour = "black", face = "bold"),
                strip.background = element_rect(fill = "pink"))

ggsave("till.inj.dis.plot.png", width = 36, height = 24, unit = "cm", dpi = 300)

#===============================================================================
#
select.dis.leaf.inj %>% 
        melt(id.vars = c("fieldno", "visit", "country","season")) %>%
        group_by(country, season, variable) %>%
        ggplot(aes(x = variable, y = value)) + 
        geom_boxplot(aes(fill = country), outlier.shape = NA) + 
        scale_y_continuous(limits = c(0, 100)) + 
        facet_grid(visit ~ season) + 
        ggtitle("Leaf Injuries by Disease in 2012 - 2014 by Country") + 
        ylab("Incidence(%)") +
        xlab("Injuires") +
        labs(fill = "Country")+
        theme(
                axis.title.x = element_text(colour = "black",size = 16),
                axis.text.x = element_text(angle = 30, 
                                           hjust =1, vjust =1, 
                                           colour = "black", face = "bold" ),
                axis.title.y = element_text(colour = "black",size = 16),
                axis.text.y = element_text(colour = "black", face = "bold"),
                strip.background = element_rect(fill = "pink"))

ggsave("leaf.inj.dis.plot.png", width = 35, height = 24, unit = "cm", dpi = 300)
