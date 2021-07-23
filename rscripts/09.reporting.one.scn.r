play <- function(){
  rm(list=ls())  
  library(tidyverse)
  options(dplyr.summarise.inform=F)
  source("rscripts/09.reporting.one.scn.r")

  list.scn <- c("NULL", "LC", "WF", "CC", "FM", "WF_FS")
  #               "WF_FS", "LC_WF", "LC_WF_FS",
  #                "CC", "CC_LC", "CC_WF", "CC_WF_FS", "CC_LC_WF", "CC_LC_WF_FS")
  
  scn.name <- "CC_LC_FM_WF_FS"
  scn.name <- "NULL_1run"
  
  for(scn.name in list.scn){
    plot.abundance(scn.name)
    plot.drought(scn.name)
    plot.cohort(scn.name)
    plot.afforest(scn.name)
    # plot.age.class(scn.name)
    plot.land.covers(scn.name)
    # plot.harvest(scn.name)
    plot.burnt(scn.name)
    plot.sqi(scn.name)
  } 
  
  ##  t     |     year    |   decade
  ##  0     |     2009
  ##  1     |     2010    |   2010
  ##  ...
  ##  10    |     2019    |   2010
  ##  11    |     2020    |   2020
  ##  ...
  ##  20    |     2029    |   2020
  ##  ...
  ##  81    |     2090    |   2090
  ##  ...
  ##  90    |     2099    |   2090
}

species.name.color <- function(){
  species <- data.frame(spp=1:14, name=c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster",
                                         "puncinata", "aalba", "qilex", "qsuber", "qfaginea", "qhumilis",
                                         "fsylvatica", "other", "shrub"),
                        name.sort=c("aalba","fsylvatica","other","phalepensis","pnigra","ppinaster","ppinea",
                                    "psylvestris", "puncinata", "qfaginea", "qhumilis", "qilex", "qsuber", "shrub"),
                        color=c("chartreuse3", "darkolivegreen1", "darkseagreen", "forestgreen",
                                "olivedrab4", "darkslategrey", "blue4", "gold", "saddlebrown",
                                "sienna2", "palegoldenrod", "red3", "purple3", "grey70"),
                        color.sort=c("royalblue4","red4", "purple3", "chartreuse2", "darkolivegreen1",
                                     "darkslategrey",  "seagreen1", "forestgreen", "grey10",   
                                     "darkorange2", "palegoldenrod", "gold", "saddlebrown", "grey70"),
                        color.cohort=c("royalblue4", "red4", "chartreuse3",  "darkolivegreen1", "olivedrab4",
                                       "darkseagreen", "forestgreen", "darkslategrey", "sienna2",
                                       "palegoldenrod",  "gold", "saddlebrown", "purple3", "grey70"),
                        type=c("conifer", "conifer", "conifer", "conifer", "conifer",
                               "conifer", "conifer", "deciduous", "deciduous", "deciduous", "deciduous",
                               "deciduous", "deciduous", "shrub"))
  
  
  save(species, file="rscripts/ins/species.rdata")
}


plot.drought <- function(scn.name){
  
  load("rscripts/ins/species.rdata")  
  
  ## Existences
  dta.land <- read.table(paste0("outputs/Scn_", scn.name, "/Land.txt"), header=T)
  dta.land <- filter(dta.land, year>10)
  dta.land <- mutate(dta.land, decade=((dta.land$year-1) %/% 10)*10+2010) %>%
              group_by(decade, spp, run) %>% summarise(area=sum(area))%>%
              group_by(decade, spp) %>% summarise(area=mean(area)) 
  ## Area and pctg killed by drought
  dta.drought <- read.table(paste0("outputs/Scn_", scn.name, "/Drought.txt"), header=T)
  dta.drought <- filter(dta.drought, year>10)
  dta.drought <- mutate(dta.drought, decade=((year-1) %/% 10)*10+2010) %>%
                 group_by(decade, spp, run) %>% summarise(ha=sum(ha)) %>%
                 group_by(decade, spp) %>% summarise(ha=mean(ha)) %>% 
                 left_join(dta.land, by=c("decade", "spp")) %>% 
                 left_join(select(species, spp, name), by="spp") %>%
                 mutate(pctg.kill=100*ha/area)
  ## Accumualted area and pctg killed by drought
  dta.accum <- data.frame(decade=2020, spp=1:13)
  dta.accum <- left_join(dta.accum, filter(dta.drought, decade==2020) %>% select(decade, spp, ha), by=c("decade", "spp")) %>%
               left_join(dta.land, by = c("decade", "spp"))
  dta.accum$ha[is.na(dta.accum$ha)] <- 0
  for(d in seq(2030,2090,10)){
    aux <- data.frame(decade=d, spp=1:13)
    aux <- left_join(aux,  filter(dta.drought, decade==d) %>% select(decade, spp, ha), by = c("decade", "spp"))%>%
           left_join(dta.land, by = c("decade", "spp"))
    aux$ha[is.na(aux$ha)] <- 0
    dta.accum <- rbind(dta.accum,aux)
    dta.accum$ha[dta.accum$decade==d] <- dta.accum$ha[dta.accum$decade==d] + dta.accum$ha[dta.accum$decade==(d-10)]
  }
  dta.accum$pctg.kill <- 100*dta.accum$ha/dta.accum$area
  dta.accum$name <- rep(species$name[-14],1)
    
  ## PLOT Area killed by decade (km2) 
  idx <- which(species$name.sort %in% sort(unique(dta.drought$name)))
  p1 <- ggplot(data=dta.drought, aes(x=as.factor(decade), y=ha/100, fill=name)) + geom_bar(stat="identity") +
        scale_fill_manual("Species", values=as.character(species$color.sort[idx])) + 
        ggtitle("Area killed by drought per decade") + ylab("km2") + xlab("period") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  ## PLOT Pctg killed over existence by decade (%) 
  p2 <- ggplot(data=dta.drought, aes(x=as.factor(decade), y=pctg.kill, fill=name)) + geom_bar(stat="identity") +
        scale_fill_manual("Species", values=as.character(species$color.sort[idx])) + 
        ggtitle("Pctg of area killed (over current existences) by drought per decade") + ylab("%") + xlab("period") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  ## PLOT Accumulated area killed by decade (km2) 
  p3 <- ggplot(data=dta.accum, aes(x=as.factor(decade), y=ha/100, fill=name)) + geom_bar(stat="identity") +
        scale_fill_manual("Species", values=as.character(species$color.sort)) + 
        ggtitle("Accumulated area killed by drought per decade") + ylab("km2") + xlab("period") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  ## PLOT Pctg killed over existence by decade (%) 
  ## Aquest p4 no està ben calculat
  p4 <- ggplot(data=dta.accum, aes(x=as.factor(decade), y=pctg.kill, fill=name)) + geom_bar(stat="identity") +
        scale_fill_manual("Species", values=as.character(species$color.sort)) + 
        ggtitle("Accumulated pctg of actual area killed by drought per decade") + ylab("%") + xlab("period") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  
  ## Save in tiff
  tiff(paste0("rscripts/outs/01.DroughtKilled_", scn.name, ".tiff"), width=1200, height=800)
  gridExtra::grid.arrange(p1,p3,p2, nrow=2)
  dev.off()
}


plot.cohort <- function(scn.name){
  
  load("rscripts/ins/species.rdata") 
  
  ## Area of species replaced after drought
  dta.cohort <- read.table(paste0("outputs/Scn_", scn.name, "/cohort.txt"), header=T)
  dta.cohort <- filter(dta.cohort, year>10)
  dta.cohort <- group_by(dta.cohort, run, spp.out, spp.in) %>% summarise(ha=sum(ha)) %>%
                group_by(spp.out, spp.in) %>% summarise(ha=mean(ha)) %>% 
                left_join(select(species, spp, name), by=c("spp.out"="spp") ) %>%
                mutate(name.out=name) %>% select(-name) %>%
                left_join(select(species, spp, name), by=c("spp.in"="spp") ) %>%
                mutate(name.in=name) %>% select(-name)
  all.out <- group_by(dta.cohort, spp.out) %>% summarise(tot.ha=sum(ha))
  dta.cohort <- left_join(dta.cohort, all.out, by="spp.out") %>% mutate(pctg=100*ha/tot.ha)
    
  ##  PLOT area replaced by new spp / killed spp
  select.color <- filter(species, name %in% unique(dta.cohort$name.in)) %>% select(color.cohort)
  p1 <- ggplot(data=dta.cohort, aes(x=name.out, y=ha/100, fill=name.in)) + geom_bar(stat="identity") +
        scale_fill_manual("Species", values=as.character(select.color$color.cohort) )+ 
        ggtitle("Species replacement after drought") + ylab("km2") + xlab("species killed") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  ## PLOT pctg of area replaced by new spp / killed spp
  p2 <- ggplot(data=dta.cohort, aes(x=name.out, y=pctg, fill=name.in)) + geom_bar(stat="identity") +
        scale_fill_manual("Species", values=as.character(select.color$color.cohort) )+ 
        ggtitle("Percentage of species replacement after drought") + ylab("%") + xlab("species killed") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
 
  ## Save in tiff
  tiff(paste0("rscripts/outs/02.CohortReplace_", scn.name, ".tiff"), width=1200, height=500)
  gridExtra::grid.arrange(p1,p2, nrow=1)
  dev.off() 
}


plot.abundance <- function(scn.name){
  
  load("rscripts/ins/species.rdata")  
  
  ## Existences
  dta.land <- read.table(paste0("outputs/Scn_", scn.name, "/Land.txt"), header=T)
  dta.land <- dta.land %>% filter(year>10) %>% mutate(year=year+2009) %>% 
              group_by(run, year, spp) %>% 
              summarise(area=sum(area), vol=sum(vol), volbark=sum(volbark), carbon=sum(carbon))
  dta.ini <- filter(dta.land, year==2020) 
  names(dta.ini)[4:7] <- paste0(names(dta.ini)[4:7], ".ini") 
  dta.ini <- dta.ini[, c(1,3:7)]
  dta.land <- left_join(dta.land, dta.ini, by=c("run","spp")) %>%
              mutate(pctg.area=100*area/area.ini, pctg.vol=100*vol/vol.ini,
                     pctg.volbark=100*volbark/volbark.ini,
                     pctg.carbon=100*carbon/carbon.ini,
                     rel.vol=vol/area, rel.volbark=volbark/area, rel.carbon=carbon/area) %>%
              left_join(select(species, spp, name, type), by="spp")
  
  ## PLOT Abundance area
  p1 <- ggplot(data=filter(dta.land,spp<=13), aes(x=year, y=area/100, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) +facet_wrap(.~type) +  #
        scale_color_manual("Species", values=as.character(species$color.sort)) + 
        ggtitle("Abundance - Area") + ylab("km2") + theme_bw()
  ## PLOT Abundance volume with bark
  p2 <- ggplot(data=filter(dta.land,spp<=13), aes(x=year, y=volbark/10^6, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) + facet_wrap(.~type) +
        scale_color_manual("Species",values=as.character(species$color.sort)) + 
        ggtitle("Abundance - Volume with bark") + ylab("m3/10^6") + theme_bw() 
  ## PLOT Abundance carbon
  p3 <- ggplot(data=filter(dta.land,spp<=13), aes(x=year, y=carbon/10^6, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) + facet_wrap(.~type) +
        scale_color_manual("Species", values=as.character(species$color.sort)) + 
        ggtitle("Abundance - Carbon") + ylab("t/10^6") + theme_bw()
  ## PLOT Percentage area
  p4 <- ggplot(data=filter(dta.land,spp<=13), aes(x=year, y=pctg.area, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) + facet_wrap(.~type) +
        scale_color_manual("Species", values=as.character(species$color.sort)) + 
        ggtitle("Percentage - Area") + ylab("%") + theme_bw()
  ## PLOT Percentage volume with bark
  p5 <- ggplot(data=filter(dta.land,spp<=13), aes(x=year, y=pctg.volbark, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) + facet_wrap(.~type) +
        scale_color_manual("Species",values=as.character(species$color.sort)) + 
        ggtitle("Percentage - Volume with bark") + ylab("%") + theme_bw() 
  ## PLOT Percentage carbon
  p6 <- ggplot(data=filter(dta.land,spp<=13), aes(x=year, y=pctg.carbon, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) + facet_wrap(.~type) +
        scale_color_manual("Species", values=as.character(species$color.sort)) + 
        ggtitle("Percentage - Carbon") + ylab("%") + theme_bw()
  ## PLOT Relative increment volume with bark
  p7 <- ggplot(data=filter(dta.land,spp<=13), aes(x=year, y=rel.volbark, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) + facet_wrap(.~type) +
        scale_color_manual("Species",values=as.character(species$color.sort)) + 
        ggtitle("Relative abundance - Volume with bark") + ylab("m3/ha") + theme_bw() 
  ## PLOT Relative increment carbon
  p8 <- ggplot(data=filter(dta.land,spp<=13), aes(x=year, y=rel.carbon, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) + facet_wrap(.~type) +
        scale_color_manual("Species", values=as.character(species$color.sort)) + 
        ggtitle("Relative abundance - Carbon") + ylab("t/ha") + theme_bw()
  
  ## PLOTs for SHRUBS
  p9 <- ggplot(data=filter(dta.land,spp==14), aes(x=year, y=area/100, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) + 
        scale_color_manual("Species", values=as.character(species$color.sort)[14]) + 
        ggtitle("Abundance - Area") + ylab("km2") + theme_bw()
  p10 <- ggplot(data=filter(dta.land,spp==14), aes(x=year, y=vol/10^9, colour=name)) +
         geom_smooth(formula=y~x, method = "loess", size=1.5) + 
         scale_color_manual("Species",values=as.character(species$color.sort)[14]) + 
         ggtitle("Abundance - Mass") + ylab("t/10^9") + theme_bw() 
  p11 <- ggplot(data=filter(dta.land,spp==14), aes(x=year, y=pctg.area, colour=name)) +
         geom_smooth(formula=y~x, method = "loess", size=1.5) +
         scale_color_manual("Species", values=as.character(species$color.sort)[14]) + 
         ggtitle("Percentage - Area") + ylab("%") + theme_bw()
  p12 <- ggplot(data=filter(dta.land,spp==14), aes(x=year, y=pctg.vol, colour=name)) +
         geom_smooth(formula=y~x, method = "loess", size=1.5) + 
         scale_color_manual("Species",values=as.character(species$color.sort)[14]) + 
         ggtitle("Percentage - Mass") + ylab("%") + theme_bw() 
  
  
  ## Save in tiff
  tiff(paste0("rscripts/outs/03.Abundance_", scn.name, ".tiff"), width=1800, height=500)
  gridExtra::grid.arrange(p1,p2,p3, nrow=1)
  dev.off() 
  tiff(paste0("rscripts/outs/04.AbundancePctg_", scn.name, ".tiff"), width=1800, height=500)
  gridExtra::grid.arrange(p4,p5,p6, nrow=1)
  dev.off() 
  tiff(paste0("rscripts/outs/05.AbundanceRel_", scn.name, ".tiff"), width=1200, height=500)
  gridExtra::grid.arrange(p7,p8, nrow=1)
  dev.off() 
  tiff(paste0("rscripts/outs/06.AbundanceShrub_", scn.name, ".tiff"), width=1200, height=500)
  gridExtra::grid.arrange(p9,p10,p11,p12, nrow=2)
  dev.off() 
  
  
}


plot.afforest <- function(scn.name){
  
  load("rscripts/ins/species.rdata") 
  
  ## Area of species replaced after drought
  dta.afforest <- read.table(paste0("outputs/Scn_", scn.name, "/Afforestation.txt"), header=T)
  dta.afforest <- filter(dta.afforest, year>10)
  dta.afforest <- mutate(dta.afforest, decade=((year-1) %/% 10)*10+2010) %>%
                  group_by(decade, spp, run) %>% summarise(ha=sum(ha))%>%
                  group_by(decade, spp) %>% summarise(ha=mean(ha)) %>% 
                  left_join(select(species, spp, name), by="spp") #%>%
  afforest.decade <- group_by(dta.afforest, decade) %>% summarise(tot.ha=sum(ha))
  dta.afforest <- left_join(dta.afforest, afforest.decade, by="decade") %>%
                  mutate(pctg.ha=100*ha/tot.ha)
  
  ## PLOT Area colonized  by decade (km2) 
  p1 <- ggplot(data=dta.afforest, aes(x=as.factor(decade), y=ha/100, fill=name)) + geom_bar(stat="identity") +
        scale_fill_manual("Species", values=as.character(species$color.sort)) + 
        ggtitle("Area colonized by tree species per decade") + ylab("km2") + xlab("period") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  ## PLOT Pctg area colonized over total colonized  by decade (%) 
  p2 <- ggplot(data=dta.afforest, aes(x=as.factor(decade), y=pctg.ha, fill=name)) + geom_bar(stat="identity") +
        scale_fill_manual("Species", values=as.character(species$color.sort)) + 
        ggtitle("Pctg of area colonized by tree species per decade") + ylab("%") + xlab("period") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  
  ## Save in tiff
  tiff(paste0("rscripts/outs/07.Afforestation_", scn.name, ".tiff"), width=1200, height=500)
  gridExtra::grid.arrange(p1,p2, nrow=1)
  dev.off() 
}


plot.land.covers <- function(scn.name){

  ## Existences
  dta.land <- read.table(paste0("outputs/Scn_", scn.name, "/Land.txt"), header=T)
  dta.land <- filter(dta.land, year>10) %>% mutate(year=year+2009)  %>%    
              group_by(run, year, spp) %>% summarise(area=sum(area), volbark=sum(volbark))
  dta.ini <- filter(dta.land, year==2020) 
  dta.ini <- dta.ini[,-2]
  names(dta.ini)[3:4] <- paste0(names(dta.ini)[3:4], ".ini") 
  dta.lc <- dta.land %>% left_join(dta.ini, by=c("run","spp")) %>%
              mutate(lc=ifelse(spp<=13, "forest", ifelse(spp==14, "shrub", ifelse(spp==15, "grass", 
                        ifelse(spp<=17, "crop", ifelse(spp<=19, "other", "urban")))))) %>% 
              group_by(run, year, lc) %>% summarise(area=sum(area), area.ini=sum(area.ini),
                                                    volbark=sum(volbark), volbark.ini=sum(volbark.ini)) %>% 
              mutate(pctg.area=100*area/area.ini, pctg.volbark=100*volbark/volbark.ini)
  dta.plot <- dta.lc %>% filter(lc!="other") %>% filter(lc!="urban")
  
  p1 <- ggplot(data=dta.plot, aes(x=year, y=pctg.area, colour=lc)) +
    geom_smooth(formula=y~x, method = "loess", size=1.5) + 
    scale_color_manual(values=c("gold", "forestgreen", "yellowgreen", "saddlebrown", "grey70")) + 
    ggtitle("Percentage - Area") + ylab("%") + theme_bw()
  
  p2 <- ggplot(data=filter(dta.plot, lc=="forest"), aes(x=year, y=pctg.volbark, colour=lc)) +
    geom_smooth(formula=y~x, method = "loess", size=1.5) + 
    scale_color_manual(values="forestgreen") + 
    ggtitle("Percentage - VolBark") + ylab("%") + theme_bw()

  tiff(paste0("rscripts/outs/08.AbundanceRelLandCover_", scn.name, ".tiff"), width=1000, height=400)
  gridExtra::grid.arrange(p1, p2, nrow=1)
  dev.off()

}


plot.sqi <- function(scn.name){
  
  load("rscripts/ins/species.rdata")  
  
  ## Existences a nivell d'espècie
  dta.land.spp <- read.table(paste0("outputs/Scn_", scn.name, "/LandSQI.txt"), header=T) %>% 
                  mutate(year=year+2010, sqi=ifelse(sqi==1, "1.low", ifelse(sqi==2, "2.high", "3.optimal"))) %>% 
                  left_join(select(species, spp, name), by="spp")
  dta.t_1.spp <- filter(dta.land.spp, year<2100) %>% mutate(area_1=area, vol_1=vol, volbark_1=volbark, year=year+1) %>% 
                 select(-area, -vol, -volbark)
  dta.t.spp <- filter(dta.land.spp, year>2010) %>% left_join(dta.t_1.spp, by=c("run", "year", "name", "sqi")) %>% 
                mutate(inc.area=area-area_1, inc.vol=vol-vol_1, inc.volbark=volbark-volbark_1)                                                    
  dta.land.spp <- dta.land.spp %>% mutate(ratio=vol/area, ratiobark=volbark/area) %>% 
                  group_by(year, spp, sqi, name) %>% 
                  summarise(area=mean(area), vol=mean(vol), volbark=mean(volbark),
                    ratio=mean(ratio), ratiobark=mean(ratiobark)) %>% mutate(area.km2=area*0.01)
  
  ## Existències totals
  dta.land <- dta.land.spp %>% group_by(year, sqi) %>% 
              summarise(area=sum(area), vol=sum(vol), volbark=sum(volbark)) %>% 
              mutate(ratio=vol/area, ratiobark=volbark/area)  %>% group_by(year, sqi) %>% 
              summarise(area=mean(area), vol=mean(vol), volbark=mean(volbark), 
                        ratio=mean(ratio), ratiobark=mean(ratiobark)) %>% filter(year>2020) %>% 
              mutate(area.km2=area*0.01)
  dta.t <- dta.t.spp %>% group_by(year, sqi) %>% 
           summarise(area=sum(area), vol=sum(vol), volbark=sum(volbark),
                     area_1=sum(area_1, na.rm=T), vol_1=sum(vol_1, na.rm=T), volbark_1=sum(volbark_1, na.rm=T)) %>% 
           mutate(inc.area=area-area_1, inc.vol=vol-vol_1, inc.volbark=volbark-volbark_1)  %>% filter(year>2020) %>% 
           group_by(year, sqi) %>% summarise(inc.area=mean(inc.area), inc.vol=mean(inc.vol), inc.volbark=mean(inc.volbark)) 
  
  ## area and volum per sqi, and increments per sqi
  p1 <- ggplot(dta.land, aes(x=year, y=area.km2, fill=sqi)) + geom_area(alpha=1, size=0.5, colour="grey70") +
        scale_fill_viridis(discrete = T)+theme_classic()
  p2 <- ggplot(dta.land, aes(x=year, y=volbark/10^6, fill=sqi)) + geom_area(alpha=1, size=0.5, colour="grey70") +
    scale_fill_viridis(discrete = T)+theme_classic()
  p3 <- ggplot(dta.t, aes(x=year, y=inc.area, fill=sqi)) + geom_area(alpha=1, size=0.5, colour="grey70") +
    scale_fill_viridis(discrete = T)+theme_classic()
  p4 <- ggplot(dta.t, aes(x=year, y=inc.volbark, fill=sqi)) + geom_area(alpha=1, size=0.5, colour="grey70") +
    scale_fill_viridis(discrete = T)+theme_classic()
  tiff(paste0("rscripts/outs/09.SQI_", scn.name, ".tiff"), width=1000, height=400)
  gridExtra::grid.arrange(p1,p2, nrow=1)
  dev.off()
  # gridExtra::grid.arrange(p1,p2,p3,p4, nrow=2)

  ## area and volum per sqi and species  
  tiff(paste0("rscripts/outs/09.SQIspp_", scn.name, ".tiff"), width=1000, height=800)
  ggplot(filter(dta.land.spp, spp<13), aes(x=year, y=area.km2, fill=sqi)) +
    geom_area(alpha=1, size=0.5, colour="grey70") +
    scale_fill_viridis(discrete = T)+ theme_classic() + facet_wrap(.~name, scales = "free")
  dev.off()
  ggplot(filter(dta.land.spp, spp<13), aes(x=year, y=volbark/10^6, fill=sqi)) +
    geom_area(alpha=1, size=0.5, colour="grey70") +
    scale_fill_viridis(discrete = T)+ theme_classic() + facet_wrap(.~name, scales = "free")

  
}


plot.harvest <- function(scn.name){
  
  load("rscripts/ins/species.rdata")  
  
  ## Harvest
  dta.harvest <- read.table(paste0("outputs/Scn_", scn.name, "/Harvest.txt"), header=T)
  dta.harvest <- dta.harvest %>% mutate(year=year+2010) %>% left_join(species, by="spp")
  
  ## Group per forest type
  dta.all <-  dta.harvest %>% group_by(run, year, type) %>% 
              summarise(vol.sawlog=sum(vol.sawlog), vol.wood=sum(vol.wood)) %>% 
              group_by(year, type) %>% 
              summarise(vol.sawlog=mean(vol.sawlog), vol.wood=mean(vol.wood)) %>% 
              pivot_longer(cols=c("vol.sawlog", "vol.wood"),   values_to="volume") %>% 
              mutate(what=paste0(type, substr(name,4,20)))
  
  ggplot(dta.all, aes(x=year, y=volume/10^3, fill=what)) +
    geom_area(alpha=1, size=0.5, colour="grey70") +
    scale_fill_viridis(discrete = T)+ theme_classic()  #+ facet_wrap(.~name, scales = "free")
  
}


plot.burnt <- function(scn.name){
  
  load("rscripts/ins/species.rdata")  
  
  ## Harvest
  dta.burnt <- read.table(paste0("outputs/Scn_", scn.name, "/BurntSpp.txt"), header=T)
  dta.burnt <- dta.burnt %>% mutate(year=year+2010) %>% left_join(species, by="spp")
  
  ## Group per forest type
  dta.all <-  dta.burnt %>% group_by(run, year, spp) %>% 
    summarise(aburnt=sum(aburnt), bburnt=sum(bburnt)) %>% 
    group_by(year, spp) %>% summarise(aburnt=mean(aburnt), bburnt=mean(bburnt)) %>% 
    filter(spp<=14) 
  dta.year <- dta.all %>% group_by(year) %>%  
    summarise(tot.aburnt=sum(aburnt), tot.bburnt=sum(bburnt))
  dta.pct <- dta.all %>% left_join(dta.year, by="year") %>% 
    mutate(pcta=aburnt/tot.aburnt, pctb=bburnt/tot.bburnt)
  
  ggplot(dta.pct, aes(x=year, y=pcta, fill=as.factor(spp))) + 
    geom_area() + theme_classic() + 
    scale_fill_manual(values=species$color.sort) 
  
  
  
}

## @@@@@@@@@@@@@@ to be developed
## a graph per species (with its colors) and 3 abundance lines (or relative abundances), one 
## line per age class differenciated by the trace (solid, dashed, points)
## Or use age.class as facet variable, and have a plot per age class with the (relative)
## abundances of all species (each one with its color)
plot.age.class <- function(scn.name){
  
  load("rscripts/ins/species.rdata")  
  
  ## Existences
  dta.land <- read.table(paste0("outputs/Scn_", scn.name, "/Land.txt"), header=T)
  dta.land <- mutate(dta.land, year=year+2010)
  dta.ini <- filter(dta.land, year==2010) %>% select(-year)
  names(dta.ini)[3:6] <- paste0(names(dta.ini)[3:6], ".ini")  
  
}