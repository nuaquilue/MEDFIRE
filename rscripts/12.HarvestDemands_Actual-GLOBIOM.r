library(tidyverse)
rm(list=ls())

## GLOBIOM DEMADNS / SCNEARIOS for Catalonia
dmnd.globiom <- read.table("//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/ins/CatDemandsGLOBIOM.txt", header=T)
ggplot(filter(dmnd.globiom, Product=="Sawlogs"), aes(x=Year, y=Volume/1000, color=Management)) + geom_line(size=1.5) + 
  scale_color_manual(values=c("green3", "purple3", "orange2")) + ggtitle("SAWLOGS") + ylab("1000 m3")
ggsave("//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/outs/finalmeeting/DemandSawlogs.jpeg", width=4.5, height=3)
ggplot(filter(dmnd.globiom, Product=="Primary"), aes(x=Year, y=Volume/1000, color=Management)) + geom_line(size=1.5) + 
  scale_color_manual(values=c("green3", "purple3", "orange2")) + ggtitle("PRIMARY SOURCES") + ylab("1000 m3")
ggsave("//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/outs/finalmeeting/DemandPrimary.jpeg", width=4.5, height=3)



## CAT SAWLOGS AND WOOD EXTRACTED
aprofitaments <- read.table("//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/ins/HarvestStatsCounty_2002-2016.txt", header=T)
aprofit.sawlogs <- group_by(aprofitaments, Year) %>% summarise(Volume=sum(Conifer+Deciduous)) %>% mutate(Product="Sawlogs")
aprofit.sawlogs <- rbind(aprofit.sawlogs, data.frame(Year=2017, Volume=781538, Product="Sawlogs"))  # OFC
  # Wood data is in tones, to convert to volume (m3) assume that all wood comes from deciduous species
  # and the average density is 1000 kg/m3
aprofit.wood <- group_by(aprofitaments, Year) %>% summarise(Volume=sum(Wood)) %>% mutate(Product="Primary")
aprofit.wood <- rbind(aprofit.wood, data.frame(Year=2017, Volume=203442*1000/770, Product="Primary"))  # OFC
aprofit.wood <- rbind(aprofit.wood, data.frame(Year=2018, Volume=224046*1000/770, Product="Primary"))  # OFC
ggplot(rbind(aprofit.sawlogs, aprofit.wood), aes(x=Year, y=Volume/1000, color=Product)) + geom_line(size=1.5) + 
  scale_color_manual(values=c("red3", "blue2")) +  ylab("1000 m3") 
  # geom_smooth(method = "lm")
ggsave("//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/outs/finalmeeting/HarvestedCatalonia.jpeg", width=4.5, height=3)


## Merge both products and sources (actual harvested and projected)
dmnd.cat <- rbind(aprofit.sawlogs, aprofit.wood)
dmnd.cat$Stats <- "Actual"
dmnd.ref <- filter(dmnd.globiom, Management=="Reference" & Year<= 2030) %>% select(-Management)
dmnd.ref <- dmnd.ref[,c(1,3,2)]
dmnd.ref$Stats <- "GLOBIOM"
dta <- rbind(dmnd.cat, dmnd.ref)
ggplot(dta, aes(x=Year, y=Volume/1000, color=Product)) + geom_line(aes(linetype=Stats), size=1.5) + 
  scale_color_manual(values=c("red3", "blue2", "orange2")) +  ylab("1000 m3") 
ggsave("//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/outs/finalmeeting/HarvestedProjected.jpeg", width=4.5, height=3)


## OPTION 1: Sum both products
dta.all <- filter(dta, Year!=2018 | Stats!="Actual") %>% group_by( Year, Stats) %>% summarize(Volume=sum(Volume))
ggplot(dta.all, aes(x=Year, y=Volume/1000)) + geom_line(aes(linetype=Stats), size=1.5) +  ylab("1000 m3") 
ggsave("//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/outs/finalmeeting/HarvestedProjectedTogether.jpeg", width=4.5, height=3)


## OPTION 2:  Use GLOBIOM trends to produce new demands, departing from observed values
increment  <- read.table("//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/ins/IncrementsGLOBIOM.txt", header=T)
sawlog.2010 <- filter(dmnd.cat, Product=="Sawlogs", Year %in% c(2009,2010,2011))  %>% summarise(Volume=mean(Volume))
primary.2010 <- filter(dmnd.cat, Product=="Primary", Year %in% c(2009,2010,2011)) %>% summarise(Volume=mean(Volume))
# 1. Project SAWLOGS in REFERENCE
ref.sawlogs <- filter(increment, Management=="Reference", Product=="Sawlogs")
ref.sawlogs$Volume <- unlist(sawlog.2010)
for(i in 2:nrow(ref.sawlogs))
  ref.sawlogs$Volume[i] <- ref.sawlogs$Volume[i-1]*(1+ref.sawlogs$Delta[i])
# 2. Project PRIMARY in REFERENCE
ref.primary <- filter(increment, Management=="Reference", Product=="Primary")
ref.primary$Volume <- unlist(primary.2010)
for(i in 2:nrow(ref.primary))
  ref.primary$Volume[i] <- ref.primary$Volume[i-1]*(1+ref.primary$Delta[i])
# 3. Project SAWLOGS in BIOENERGY
bio.sawlogs <- filter(increment, Management=="Bioenergy", Product=="Sawlogs")
bio.sawlogs$Volume <- unlist(sawlog.2010)
for(i in 2:nrow(bio.sawlogs))
  bio.sawlogs$Volume[i] <- bio.sawlogs$Volume[i-1]*(1+bio.sawlogs$Delta[i])
# 4. Project PRIMARY in BIOENERGY
bio.primary <- filter(increment, Management=="Bioenergy", Product=="Primary")
bio.primary$Volume <- unlist(primary.2010)
for(i in 2:nrow(bio.primary))
  bio.primary$Volume[i] <- bio.primary$Volume[i-1]*(1+bio.primary$Delta[i])
# 5. Project SAWLOGS in GLOBAL
glo.sawlogs <- filter(increment, Management=="Global", Product=="Sawlogs")
glo.sawlogs$Volume <- unlist(sawlog.2010)
for(i in 2:nrow(glo.sawlogs))
  glo.sawlogs$Volume[i] <- glo.sawlogs$Volume[i-1]*(1+glo.sawlogs$Delta[i])
# 6. Project PRIMARY in GLOBAL
glo.primary <- filter(increment, Management=="Global", Product=="Primary")
glo.primary$Volume <- unlist(primary.2010)
for(i in 2:nrow(glo.primary))
  glo.primary$Volume[i] <- glo.primary$Volume[i-1]*(1+glo.primary$Delta[i])
# Plots
dmnd.trend <- rbind(ref.primary, ref.sawlogs, bio.primary, bio.sawlogs, glo.primary, glo.sawlogs)
dmnd.trend <- dmnd.trend[,-4]
dmnd.trend$Source <- "Trend"
dmnd.globiom$Source <- "Globiom"
dmnds <- rbind(dmnd.trend, dmnd.globiom)
ggplot(filter(dmnds, Product=="Sawlogs"), aes(x=Year, y=Volume/1000, color=Management)) + 
  geom_line(aes(linetype=Source), size=1.5) + 
  scale_color_manual(values=c("green3", "purple3", "orange2")) + ggtitle("SAWLOGS") + ylab("1000 m3")
ggsave("//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/outs/finalmeeting/DemandTrendSawlogs.jpeg", width=4.5, height=3)
ggplot(filter(dmnds, Product=="Primary"), aes(x=Year, y=Volume/1000, color=Management)) + 
  geom_line(aes(linetype=Source), size=1.5) + 
  scale_color_manual(values=c("green3", "purple3", "orange2")) + ggtitle("PRIMARY SOURCES") + ylab("1000 m3")
ggsave("//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/outs/finalmeeting/DemandTrendPrimary.jpeg", width=4.5, height=3)


## ACTUAL AND GLOBIOM DEMANDS FOR SPAIN
aprofitaments <- read.table("//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/ins/HarvestStatsSpain_2005-2016.txt", header=T)
aprofit.sawlogs <- group_by(aprofitaments, Year) %>% summarise(Volume=sum(Conifer+Deciduous)) %>% mutate(Product="Roundwood")
# Wood data is in tones, to convert to volume (m3) assume that all wood comes from deciduous species
# and the average density is 770 kg/m3
aprofit.wood <- group_by(aprofitaments, Year) %>% summarise(Volume=sum(Wood)*1000/770) %>% mutate(Product="Primary")
      # ggplot(rbind(aprofit.sawlogs, aprofit.wood), aes(x=Year, y=Volume/1000, color=Product)) + geom_line(size=1.5) + 
      #   scale_color_manual(values=c("red3", "blue2")) +  ylab("1000 m3") 
dmnd.globiom <- read.table("//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/ins/SpDemandsGLOBIOM.txt", header=T)
dmnd.sp <- rbind(aprofit.sawlogs, aprofit.wood)
dmnd.sp$Stats <- "Actual"
dmnd.ref <- filter(dmnd.globiom, Management=="Reference" & Year<= 2030) %>% select(-Management)
dmnd.ref <- dmnd.ref[,c(1,3,2)]
dmnd.ref$Stats <- "GLOBIOM"
dta <- rbind(dmnd.sp, dmnd.ref)
ggplot(dta, aes(x=Year, y=Volume/10^6, color=Product)) + geom_line(aes(linetype=Stats), size=1.5) + 
  scale_color_manual(values=c("red3", "blue2", "orange2")) +  ylab("10^6 m3") +ggtitle ("Harvesting in Spain")
ggsave("//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/outs/finalmeeting/HarvestedProjectedSpain.jpeg", width=4.5, height=3)





# library(tidyverse)
# library(readxl)
# 
# setwd("//serverprocess/nu/")
# dmnd <- read.table("futurebioecon/globiom/CatDmnd_Reference.txt", header=T)
# 
# head(dmnd)
# aprofitaments <- read_xls("FutureBioEcon/HarvestingStats/ProduccióComarcal/GF_aprofitaments_forestals_comarques_evolucio_2002_2016.xlsx",
#                           sheet=3, col_names = T)
# names(aprofitaments)
# 
# tot.annual.cat <- group_by(aprofitaments, Year) %>%  
#   summarise(vol.fusta=sum(Conifer+Deciduous), vol.conifer=sum(Conifer), 
#             vol.decid=sum(Deciduous), llenya_t=sum(Wood)) %>%
#   mutate(llenya=llenya_t*1000/740)
# 
# dta <- left_join(dmnd, tot.annual.cat)
# 
# a <- filter(dta, Year<=2016) %>% select(Year, Sawlogs) %>% mutate(source="GLOBIOM_Sawlog")
# b <- filter(dta, Year<=2016) %>% select(Year, TotHarvest) %>% mutate(source="GLOBIOM_Harvest")
# c <- filter(dta, Year<=2016) %>% select(Year, vol.fusta) %>% mutate(source="Cat_Fusta")
# d <- filter(dta, Year<=2016) %>% select(Year, Primary) %>% mutate(source="GLOBIOM_Wood")
# e <- filter(dta, Year<=2016) %>% select(Year, llenya) %>% mutate(source="Cat_Llenya")
# names(a)[2] <- names(b)[2] <- names(c)[2] <- names(d)[2] <-names(e)[2] <-"vol"
# 
# kk <- rbind(a,b,c,d,e)
# 
# tiff("c:/work/MEDMOD/R.MEDFIRE_II/outs/CompareHarvestDmnd_Cat-GLOBIOM.tiff", width=600, height=500)
# ggplot(kk, aes(x=Year, y=vol/10^6, group=source))  +
#   geom_line(aes(color=source))+  geom_point(aes(color=source)) +
#   scale_color_brewer(palette="Set1")
# dev.off()
