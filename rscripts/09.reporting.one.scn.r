plot.drought = function(scn.name, species, species.sort){
  
  ## Existences
  dta.land = read.table(paste0("outputs/Scn_", scn.name, "/Land.txt"), header=T)
  dta.land = filter(dta.land, year>10)
  dta.land = mutate(dta.land, decade=((dta.land$year-1) %/% 10)*10+2010) %>%
              group_by(decade, spp, run) %>% summarise(area=sum(area))%>%
              group_by(decade, spp) %>% summarise(area=mean(area)) 
  ## Area and pctg killed by drought
  dta.drought = read.table(paste0("outputs/Scn_", scn.name, "/Drought.txt"), header=T)
  dta.drought = filter(dta.drought, year>10)
  dta.drought = mutate(dta.drought, decade=((year-1) %/% 10)*10+2010) %>%
                 group_by(decade, spp, run) %>% summarise(ha=sum(ha)) %>%
                 group_by(decade, spp) %>% summarise(ha=mean(ha)) %>% 
                 left_join(dta.land, by=c("decade", "spp")) %>% 
                 left_join(species, by="spp") %>%
                 mutate(pctg.kill=100*ha/area)
  ## Accumualted area and pctg killed by drought
  dta.accum = data.frame(decade=2020, spp=1:13)
  dta.accum = left_join(dta.accum, filter(dta.drought, decade==2020) %>% select(decade, spp, ha), by=c("decade", "spp")) %>%
               left_join(dta.land, by = c("decade", "spp"))
  dta.accum$ha[is.na(dta.accum$ha)] = 0
  for(d in seq(2030,2090,10)){
    aux = data.frame(decade=d, spp=1:13)
    aux = left_join(aux,  filter(dta.drought, decade==d) %>% select(decade, spp, ha), by = c("decade", "spp"))%>%
           left_join(dta.land, by = c("decade", "spp"))
    aux$ha[is.na(aux$ha)] = 0
    dta.accum = rbind(dta.accum,aux)
    dta.accum$ha[dta.accum$decade==d] = dta.accum$ha[dta.accum$decade==d] + dta.accum$ha[dta.accum$decade==(d-10)]
  }
  dta.accum$pctg.kill = 100*dta.accum$ha/dta.accum$area
  dta.accum = left_join(dta.accum, species, by="spp")
 
  ## PLOT Area killed by decade (km2) 
  p1 = ggplot(data=dta.drought, aes(x=as.factor(decade), y=ha/100, fill=name)) + geom_bar(stat="identity") +
        scale_fill_manual("Species", values=as.character(species.sort$color[1:13])) + 
        ggtitle("Area killed by drought per decade") + ylab("km2") + xlab("period") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  ## PLOT Pctg killed over existence by decade (%) 
  p2 = ggplot(data=dta.drought, aes(x=as.factor(decade), y=pctg.kill, fill=name)) + geom_bar(stat="identity") +
        scale_fill_manual("Species", values=as.character(species.sort$color[1:13])) + 
        ggtitle("Pctg of area killed (over current existences) by drought per decade") + ylab("%") + xlab("period") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  ## PLOT Accumulated area killed by decade (km2) 
  p3 = ggplot(data=dta.accum, aes(x=as.factor(decade), y=ha/100, fill=name)) + geom_bar(stat="identity") +
        scale_fill_manual("Species", values=as.character(species.sort$color[1:13])) + 
        ggtitle("Accumulated area killed by drought per decade") + ylab("km2") + xlab("period") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  ## PLOT Pctg killed over existence by decade (%) 
  ## Aquest p4 no està ben calculat
  p4 = ggplot(data=dta.accum, aes(x=as.factor(decade), y=pctg.kill, fill=name)) + geom_bar(stat="identity") +
        scale_fill_manual("Species", values=as.character(species.sort$color[1:13])) + 
        ggtitle("Accumulated pctg of actual area killed by drought per decade") + ylab("%") + xlab("period") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  
  ## Save in tiff
  tiff(paste0("plots/01.DroughtKilled_", scn.name, ".tiff"), width=1200, height=800)
  gridExtra::grid.arrange(p1,p3,p2, nrow=2)
  dev.off()
}


plot.cohort = function(scn.name, species, species.sort){
  
  ## Area of species replaced after drought
  dta = read.table(paste0("outputs/Scn_", scn.name, "/cohort.txt"), header=T)
  dta.cohort = filter(dta, year>10)
  dta.cohort = group_by(dta.cohort, run, spp.out, spp.in) %>% summarise(ha=sum(ha)) %>%
                group_by(spp.out, spp.in) %>% summarise(ha=mean(ha)) %>% 
                left_join(species, by=c("spp.out"="spp") ) %>%
                mutate(name.out=name) %>% select(-name) %>%
                left_join(species, by=c("spp.in"="spp") ) %>%
                mutate(name.in=name) %>% select(-name)
  all.out = group_by(dta.cohort, spp.out) %>% summarise(tot.ha=sum(ha))
  dta.cohort = left_join(dta.cohort, all.out, by="spp.out") %>% mutate(pctg=100*ha/tot.ha)
    
  ##  PLOT area replaced by new spp / killed spp
  spp = filter(species.sort, name %in% unique(dta.cohort$name.in))
  p1 = ggplot(data=dta.cohort, aes(x=name.out, y=ha/100, fill=name.in)) + geom_bar(stat="identity") +
        scale_fill_manual("Species", values=as.character(spp$color))+ 
        ggtitle("Species replacement after drought") + ylab("km2") + xlab("species killed") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  p1
  ## PLOT pctg of area replaced by new spp / killed spp
  p2 = ggplot(data=dta.cohort, aes(x=name.out, y=pctg, fill=name.in)) + geom_bar(stat="identity") +
        scale_fill_manual("Species", values=as.character(spp$color))+ 
        ggtitle("Percentage of species replacement after drought") + ylab("%") + xlab("species killed") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  p2
  ## Save in tiff
  tiff(paste0("plots/02.CohortReplace_", scn.name, ".tiff"), width=1200, height=500)
  gridExtra::grid.arrange(p1,p2, nrow=1)
  dev.off() 
  
  ## PAPER: plot pctg of area replaced by new spp without shrubs as a flower plot
  dta.in = filter(dta, year>10) %>% group_by(run, spp.in) %>% summarise(ha=sum(ha)) %>%
            group_by(spp.in) %>% summarise(ha=mean(ha)) %>% 
            left_join(species, by=c("spp.in"="spp")) 
  dta.in$pct = dta.in$ha/sum(dta.in$ha)*100
  dta.in$Group = ifelse(dta.in$spp.in %in% c(2,3,5,6,7), "Conifers",
                    ifelse(dta.in$spp.in==1, "Pinus halepensis",
                      ifelse(dta.in$spp.in==4, "Pinus sylvestris",    
                        ifelse(dta.in$spp.in==8, "Quercus ilex",
                            ifelse(dta.in$spp.in==14, "Shrublands",     
                               ifelse(dta.in$spp.in==11, "Quercus humilis", "Other broadleaves"))))))
  dta.plot = filter(dta.in, spp.in!=14) %>% group_by(Group) %>% 
    summarise(pct=round(sum(pct),1))
  ggplot(filter(dta.in, spp.in!=14, Group!="Other broadleaves"), aes(x=Group, y=pct, fill=Group)) + 
    geom_col() +  coord_polar()+ theme_classic() +
    scale_fill_manual(values=c( "forestgreen", "red4","chartreuse3","black",  "palegoldenrod", "gold"))
    # scale_fill_manual(values=c("royalblue4", "red4",  "chartreuse3", "darkolivegreen1",
    #                            "darkslategrey", "seagreen1", "forestgreen", "grey10", "darkorange2",
    #                            "palegoldenrod", "gold", "saddlebrown"))  #"purple3",
  
  group_by(dta.in, Group) %>% summarise(pct=round(sum(pct),1))
  
  ## Cohort replacement between 2020-2060 and 2060-2100
  dta.in = filter(dta, year>10) %>% mutate(period=ifelse(year<=50, "2020-2060", "2060-2100")) %>% 
           group_by(run, period, spp.in) %>% summarise(ha=sum(ha)) %>%
           group_by(period, spp.in) %>% summarise(ha=mean(ha)) %>% 
           left_join(species, by=c("spp.in"="spp")) 
  total = group_by(dta.in, period) %>% summarise(tot=sum(ha));total
  dta.in = left_join(dta.in, total, by="period")   %>% 
    mutate(pct=100*ha/tot)
  dta.in$Group = ifelse(dta.in$spp.in %in% c(2,3,5,6,7), "Conifers",
                        ifelse(dta.in$spp.in==1, "Pinus halepensis",
                               ifelse(dta.in$spp.in==4, "Pinus sylvestris",    
                                      ifelse(dta.in$spp.in==14, "Shrublands",     
                                             ifelse(dta.in$spp.in==8, "Quercus ilex",
                                                    ifelse(dta.in$spp.in==11, "Quercus humilis", "Other broadleaves"))))))
  # flower plot
  ggplot(filter(dta.in, spp.in!=14), aes(x=Group, y=pct, fill=Group)) + 
    geom_col() +  coord_polar()+ theme_classic() + facet_grid(.~period) +
    scale_fill_manual(values=c( "forestgreen", "red4","chartreuse3","black",  "palegoldenrod", "gold"))
  # table
  group_by(dta.in, period, Group) %>% summarise(pct=round(sum(pct),1)) %>% 
      pivot_wider(names_from = period, values_from = pct)
     
}


plot.abundance = function(scn.name, species, species.sort){
  
  ## Existences
  dta.land = read.table(paste0("outputs/Scn_", scn.name, "/Land.txt"), header=T)
  dta.land = dta.land %>% filter(year>10) %>% mutate(year=year+2009) %>% 
              group_by(run, year, spp) %>% 
              summarise(area=sum(area), vol=sum(vol), volbark=sum(volbark), carbon=sum(carbon))
  dta.ini = filter(dta.land, year==2020) 
  names(dta.ini)[4:7] = paste0(names(dta.ini)[4:7], ".ini") 
  dta.ini = dta.ini[, c(1,3:7)]
  dta.land = left_join(dta.land, dta.ini, by=c("run","spp")) %>%
              mutate(pctg.area=100*area/area.ini, pctg.vol=100*vol/vol.ini,
                     pctg.volbark=100*volbark/volbark.ini,
                     pctg.carbon=100*carbon/carbon.ini,
                     rel.vol=vol/area, rel.volbark=volbark/area, rel.carbon=carbon/area) %>%
              left_join(species, by="spp")
  
  ## PLOT Abundance area
  p1 = ggplot(data=filter(dta.land,spp<=13), aes(x=year, y=area/100, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) +facet_wrap(.~type) +  
        scale_color_manual("Species", values=as.character(species.sort$color)) + 
        ggtitle("Abundance - Area") + ylab("km2") + theme_bw()
  ## PLOT Abundance volume with bark
  p2 = ggplot(data=filter(dta.land,spp<=13), aes(x=year, y=volbark/10^6, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) + facet_wrap(.~type) +
        scale_color_manual("Species", values=as.character(species.sort$color)) + 
        ggtitle("Abundance - Volume with bark") + ylab("m3/10^6") + theme_bw() 
  ## PLOT Abundance carbon
  p3 = ggplot(data=filter(dta.land,spp<=13), aes(x=year, y=carbon/10^6, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) + facet_wrap(.~type) +
        scale_color_manual("Species", values=as.character(species.sort$color)) + 
        ggtitle("Abundance - Carbon") + ylab("t/10^6") + theme_bw()
  ## PLOT Percentage area
  p4 = ggplot(data=filter(dta.land,spp<=13), aes(x=year, y=pctg.area, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) + facet_wrap(.~type) +
        scale_color_manual("Species", values=as.character(species.sort$color)) + 
        ggtitle("Percentage - Area") + ylab("%") + theme_bw()
  ## PLOT Percentage volume with bark
  p5 = ggplot(data=filter(dta.land,spp<=13), aes(x=year, y=pctg.volbark, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) + facet_wrap(.~type) +
        scale_color_manual("Species", values=as.character(species.sort$color)) + 
        ggtitle("Percentage - Volume with bark") + ylab("%") + theme_bw() 
  ## PLOT Percentage carbon
  p6 = ggplot(data=filter(dta.land,spp<=13), aes(x=year, y=pctg.carbon, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) + facet_wrap(.~type) +
        scale_color_manual("Species", values=as.character(species.sort$color)) + 
        ggtitle("Percentage - Carbon") + ylab("%") + theme_bw()
  ## PLOT Relative increment volume with bark
  p7 = ggplot(data=filter(dta.land,spp<=13), aes(x=year, y=rel.volbark, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) + facet_wrap(.~type) +
        scale_color_manual("Species", values=as.character(species.sort$color)) + 
        ggtitle("Relative abundance - Volume with bark") + ylab("m3/ha") + theme_bw() 
  ## PLOT Relative increment carbon
  p8 = ggplot(data=filter(dta.land,spp<=13), aes(x=year, y=rel.carbon, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) + facet_wrap(.~type) +
        scale_color_manual("Species", values=as.character(species.sort$color)) + 
        ggtitle("Relative abundance - Carbon") + ylab("t/ha") + theme_bw()
  
  ## PLOTs for SHRUBS
  p9 = ggplot(data=filter(dta.land,spp==14), aes(x=year, y=area/100, colour=name)) +
        geom_smooth(formula=y~x, method = "loess", size=1.5) + 
        scale_color_manual("Species", values=as.character(species.sort$color)[14]) + 
        ggtitle("Abundance - Area") + ylab("km2") + theme_bw()
  p10 = ggplot(data=filter(dta.land,spp==14), aes(x=year, y=vol/10^9, colour=name)) +
         geom_smooth(formula=y~x, method = "loess", size=1.5) + 
         scale_color_manual("Species", values=as.character(species.sort$color)[14]) + 
         ggtitle("Abundance - Mass") + ylab("t/10^9") + theme_bw() 
  p11 = ggplot(data=filter(dta.land,spp==14), aes(x=year, y=rel.vol, colour=name)) +
         geom_smooth(formula=y~x, method = "loess", size=1.5) +
         scale_color_manual("Species", values=as.character(species.sort$color)[14]) + 
         ggtitle("Relative abundance - Area") + ylab("%") + theme_bw()
  p12 = ggplot(data=filter(dta.land,spp==15), aes(x=year, y=area/100, colour=name)) +
         geom_smooth(formula=y~x, method = "loess", size=1.5) + 
         scale_color_manual("Species", values=as.character(species.sort$color)[15]) + 
         ggtitle("Abundance - Area") + ylab("km2") + theme_bw() 
  
  
  ## Save in tiff
  tiff(paste0("plots/03.Abundance_", scn.name, ".tiff"), width=1800, height=500)
  gridExtra::grid.arrange(p1,p2,p3, nrow=1)
  dev.off() 
  tiff(paste0("plots/04.AbundancePctg_", scn.name, ".tiff"), width=1800, height=500)
  gridExtra::grid.arrange(p4,p5,p6, nrow=1)
  dev.off() 
  tiff(paste0("plots/05.AbundanceRel_", scn.name, ".tiff"), width=1200, height=500)
  gridExtra::grid.arrange(p7,p8, nrow=1)
  dev.off() 
  tiff(paste0("plots/06.AbundanceShrubGrass_", scn.name, ".tiff"), width=1200, height=500)
  gridExtra::grid.arrange(p9,p10,p11,p12, nrow=2)
  dev.off() 
}


plot.afforest = function(scn.name, species, species.sort){
  
  ## Area of species replaced after drought
  dta.afforest = read.table(paste0("outputs/Scn_", scn.name, "/Afforestation.txt"), header=T)
  dta.afforest = filter(dta.afforest, year>10)
  dta.afforest = mutate(dta.afforest, decade=((year-1) %/% 10)*10+2010) %>%
                  group_by(decade, spp, run) %>% summarise(ha=sum(ha))%>%
                  group_by(decade, spp) %>% summarise(ha=mean(ha)) %>% 
                  left_join(species, by="spp") 
  afforest.decade = group_by(dta.afforest, decade) %>% summarise(tot.ha=sum(ha))
  dta.afforest = left_join(dta.afforest, afforest.decade, by="decade") %>%
                  mutate(pctg.ha=100*ha/tot.ha)
  
  ## PLOT Area colonized  by decade (km2) 
  p1 = ggplot(data=dta.afforest, aes(x=as.factor(decade), y=ha/100, fill=name)) + geom_bar(stat="identity") +
        scale_fill_manual("Species", values=as.character(species.sort$color)) + 
        ggtitle("Area colonized by tree species per decade") + ylab("km2") + xlab("period") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  ## PLOT Pctg area colonized over total colonized  by decade (%) 
  p2 = ggplot(data=dta.afforest, aes(x=as.factor(decade), y=pctg.ha, fill=name)) + geom_bar(stat="identity") +
        scale_fill_manual("Species", values=as.character(species.sort$color)) + 
        ggtitle("Pctg of area colonized by tree species per decade") + ylab("%") + xlab("period") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90)) 
  
  ## Save in tiff
  tiff(paste0("plots/07.Afforestation_", scn.name, ".tiff"), width=1200, height=500)
  gridExtra::grid.arrange(p1,p2, nrow=1)
  dev.off() 
  
  ## PAPER: Annual rate in NULL
  kk = group_by(dta.afforest, decade) %>% summarise(ha=sum(ha)) 
  
}


plot.land.covers = function(scn.name){

  ## Existences
  dta.land = read.table(paste0("outputs/Scn_", scn.name, "/Land.txt"), header=T)
  dta.land = filter(dta.land, year>10) %>% mutate(year=year+2009)  %>%    
              group_by(run, year, spp) %>% summarise(area=sum(area), volbark=sum(volbark))
  dta.ini = filter(dta.land, year==2020) 
  dta.ini = dta.ini[,-2]
  names(dta.ini)[3:4] = paste0(names(dta.ini)[3:4], ".ini") 
  dta.lc = dta.land %>% left_join(dta.ini, by=c("run","spp")) %>%
            mutate(lc=ifelse(spp<=13, "forest", ifelse(spp==14, "shrub", ifelse(spp==15, "grass", 
                      ifelse(spp<=17, "crop", ifelse(spp<=19, "other", "urban")))))) %>% 
            group_by(run, year, lc) %>% summarise(area=sum(area), area.ini=sum(area.ini),
                   volbark=sum(volbark), volbark.ini=sum(volbark.ini)) %>% 
            mutate(pctg.area=100*area/area.ini, pctg.volbark=100*volbark/volbark.ini)
  dta.plot = dta.lc %>% filter(lc!="other") %>% filter(lc!="urban")
  
  p1 = ggplot(data=dta.plot, aes(x=year, y=pctg.area, colour=lc)) +
    geom_smooth(formula=y~x, method = "loess", size=1.5) + 
    scale_color_manual(values=c("gold", "forestgreen", "deeppink", "grey70")) + 
    ggtitle("Percentage - Area") + ylab("%") + theme_bw()

  tiff(paste0("plots/08.AbundanceRelLandCover_", scn.name, ".tiff"), width=400, height=400)
  gridExtra::grid.arrange(p1, nrow=1)
  dev.off()
}


plot.sqi = function(scn.name, species){
  
  ## Existences a nivell d'espècie
  dta.land.spp = read.table(paste0("outputs/Scn_", scn.name, "/LandSQI.txt"), header=T) %>% 
                  mutate(year=year+2009, SQI=ifelse(sqi==1, "low", ifelse(sqi==2, "high", "optimal"))) %>% 
                  left_join(species, by="spp")
  dta.land.spp$SQI = factor(dta.land.spp$SQI, levels=c("optimal", "high", "low"))
  dta.land.spp = dta.land.spp %>% mutate(ratio=vol/area, ratiobark=volbark/area) %>% 
                  group_by(year, spp, SQI, name, namefull) %>% 
                  summarise(area=mean(area), vol=mean(vol), volbark=mean(volbark),
                  ratio=mean(ratio), ratiobark=mean(ratiobark)) %>% 
                  mutate(area.km2=area*0.01) %>% filter(year>=2020)
  
  ## Existències totals
  dta.land = dta.land.spp %>% group_by(year, SQI) %>% 
              summarise(area=sum(area), vol=sum(vol), volbark=sum(volbark)) %>% 
              mutate(ratio=vol/area, ratiobark=volbark/area)  %>% group_by(year, SQI) %>% 
              summarise(area=mean(area), vol=mean(vol), volbark=mean(volbark), 
                        ratio=mean(ratio), ratiobark=mean(ratiobark)) %>% filter(year>=2020) %>% 
              mutate(area.km2=area*0.01)
  
  ## area and volum per sqi, and increments per sqi
  p1 = ggplot(dta.land, aes(x=year, y=area.km2, fill=SQI)) + geom_area() + #alpha=1, size=0.5, colour="grey70") +
        theme_cowplot() + scale_y_continuous(expression(km^2)) + scale_fill_manual(values=c("grey5", "grey50", "grey85")) +
        theme(legend.position ="bottom")
        #scale_fill_brewer(palette = 6, direction=-1) #scale_fill_brewer(palette = 4, direction=-1)
  p1
  p2 = ggplot(dta.land, aes(x=year, y=volbark/10^6, fill=SQI)) + geom_area() +
        theme_cowplot() + scale_y_continuous(expression(m^3)) + 
        scale_fill_manual(values=c("grey5", "grey50", "grey85")) +
        theme(legend.position ="bottom")
  p2
  p5 = ggplot(filter(dta.land.spp, spp<13), aes(x=year, y=area.km2, fill=SQI)) + geom_area() + 
        # scale_fill_brewer(palette = 5, direction=-1) +  ## for .ppt
        scale_fill_manual(values=c("grey5", "grey50", "grey85")) + 
        theme_cowplot() + facet_wrap(.~namefull, scales = "free") +
        scale_y_continuous(expression(km^2)) + theme(legend.position ="bottom")
  p5
  p6 = ggplot(filter(dta.land.spp, spp<13), aes(x=year, y=volbark/10^6, fill=SQI)) +
        geom_area() + scale_fill_manual(values=c("grey5", "grey50", "grey85")) +
        theme_cowplot() + facet_wrap(.~namefull, scales = "free") +
        scale_y_continuous(expression(m^3)) + theme(legend.position ="bottom")
  p6      
  
  ## Area and volume per SQI over time
  tiff(paste0("plots/09.SQI_", scn.name, ".tiff"), width=800, height=400)
  gridExtra::grid.arrange(p1,p2, nrow=1)
  dev.off()
  ## Area and volum per SQI and per species, over time
  tiff(paste0("plots/10.SQIsppArea_", scn.name, ".tiff"), width=900, height=500)
  gridExtra::grid.arrange(p5, nrow=1)
  dev.off()
  tiff(paste0("plots/11.SQIsppVol_", scn.name, ".tiff"), width=900, height=500)
  gridExtra::grid.arrange(p6, nrow=1)
  dev.off()
  
  ## for .ppt when scn.name is "CC_LC_FM_WF_FS"
  dta.land.spp$namefull = factor(dta.land.spp$namefull, 
      levels=c("Pinus halepensis", "Pinus nigra", "Pinus pinea", "Pinus pinaster",
               "Pinus sylvestris", "Pinus uncinata", "Abies alba", "Fagus sylvatica",
               "Quercus ilex", "Quercus faginea", "Quercus humilis", "Quercus suber"))
  dta.land.spp$SQI = factor(dta.land.spp$SQI, levels=c("optimal", "high", "low"))
  ppt = ggplot(filter(dta.land.spp, spp<13), aes(x=year, y=area.km2, fill=SQI)) + geom_area() + 
      # scale_fill_brewer(palette = 5, direction=-1) +
      # scale_fill_viridis_d(direction=-1, option="C")+
    # scale_fill_manual(values=c("grey5", "grey50", "grey85")) + 
    scale_fill_manual(values=c("darkslategrey", "forestgreen", "darkolivegreen1")) +
    theme_cowplot() + facet_wrap(.~namefull, scales = "free") +
    scale_y_continuous(expression(km^2)) + theme(legend.position ="bottom") 
    # geom_vline(xintercept = 2060, col="grey95", size=1.5, linetype = "solid") 
  png(paste0("plots/ppt_paper/SQIsppAreaGreen_", scn.name, ".png"), width=800, height=500)
  ppt  
  dev.off()  

  
  ## PAPER, values
  area.ini = filter(dta.land.spp, spp<13, year==2020) %>%
             group_by(spp, name, namefull) %>% summarise(area0=sum(area))
  area.mid = filter(dta.land.spp, spp<13, year==2020+40) %>%
    group_by(spp, name, namefull) %>% summarise(area1=sum(area))
  area.end = filter(dta.land.spp, spp<13, year==2099) %>%
             group_by(spp, name, namefull) %>% summarise(area=sum(area))
  area.end %>% left_join(area.ini, by=c("spp", "name", "namefull")) %>% 
    mutate(delta= 100*(area-area0)/area0)
  # total forest
  filter(dta.land.spp, spp<=13, year==2010) %>% group_by(year) %>% summarise(area=sum(area))
  filter(dta.land.spp, spp<=13, year==2020) %>% group_by(year) %>% summarise(area=sum(area))
  filter(dta.land.spp, spp<=13, year==2060) %>% group_by(year) %>% summarise(area=sum(area))
  filter(dta.land.spp, spp<=13, year==2099) %>% group_by(year) %>% summarise(area=sum(area))

  forest=filter(dta.land.spp, spp<=13) %>% group_by(year) %>% summarise(area=sum(area)/10^6)
  plot(forest$area~forest$year)
  
}


plot.age.class = function(scn.name, species){
  ## Existences
  dta.land = read.table(paste0("outputs/Scn_", scn.name, "/Land.txt"), header=T)
  dta.land = mutate(dta.land, year=year+2010) %>% filter(!is.na(age.class)) 
  dta.tot = dta.land %>% group_by(run, year, spp) %>% summarise(tot.area=sum(area), 
            tot.vol=sum(vol), tot.volbark=sum(volbark), tot.carbon=sum(carbon))
  dta.pct = left_join(dta.land, dta.tot, by=c("run", "year", "spp")) %>% 
    mutate(pct.area=area/tot.area, pct.vol=vol/tot.vol, pct.volbark=volbark/tot.volbark,
           pct.carbon=carbon/tot.carbon) 
  dta.plot = dta.pct %>% filter(spp<13) %>% group_by(year, spp, age.class) %>% 
    summarise(pct.area=mean(pct.area), pct.vol=mean(pct.vol)) %>% 
    left_join(species, by="spp") 
  dta.plot$age.class = factor(dta.plot$age.class, levels = c("old", "mature", "young"))
  #
  p1 = ggplot(dta.plot, aes(x=year, y=pct.area, fill=age.class)) + 
    geom_area(alpha=1, size=0.5, colour="grey70") + facet_wrap(.~name, scales = "free") +
    scale_fill_viridis(discrete = T)+theme_classic()
  tiff(paste0("plots/12.AgeClass_", scn.name, ".tiff"), width=800, height=800)
  gridExtra::grid.arrange(p1, nrow=1)
  dev.off() 
}


plot.burnt = function(scn.name, species){
  ## Burnt
  dta.burnt = read.table(paste0("outputs/Scn_", scn.name, "/BurntSpp.txt"), header=T)
  dta.burnt = dta.burnt %>% mutate(year=year+2010) %>% left_join(species, by="spp")
  ## Group per species
  dta.spp = dta.burnt %>% group_by(run, year, name) %>% 
            summarise(aburnt=sum(aburnt), bburnt=sum(bburnt)) %>% 
            group_by(year, name) %>% summarise(aburnt=mean(aburnt), bburnt=mean(bburnt))
  dta.year = dta.spp %>% group_by(year) %>%  
              summarise(tot.aburnt=sum(aburnt), tot.bburnt=sum(bburnt))
  dta.pct.spp = dta.spp %>% left_join(dta.year, by="year") %>% 
             mutate(pcta=aburnt/tot.aburnt, pctb=bburnt/tot.bburnt)
  ## Group per forest type
  dta.ftype = dta.burnt %>% group_by(run, year, type) %>% 
              summarise(aburnt=sum(aburnt), bburnt=sum(bburnt)) %>% 
              group_by(year, type) %>% summarise(aburnt=mean(aburnt), bburnt=mean(bburnt))
  dta.year = dta.ftype %>% group_by(year) %>%  
    summarise(tot.aburnt=sum(aburnt), tot.bburnt=sum(bburnt))
  dta.pct.ftype = dta.ftype %>% left_join(dta.year, by="year") %>% 
                  mutate(pcta=aburnt/tot.aburnt, pctb=bburnt/tot.bburnt)
  # Plot pctg burnt
  p1 = ggplot(dta.pct.spp, aes(x=year, y=pcta, fill=name)) + geom_area() + theme_classic() + 
       scale_fill_manual(values=as.character(species.sort$color[order(species.sort$name)]))
  p2 = ggplot(dta.pct.ftype, aes(x=year, y=aburnt, fill=type)) + geom_area() + theme_classic() + 
       scale_fill_manual(values=c( "forestgreen", "gold",  "saddlebrown", "grey70")) 
  tiff(paste0("plots/13.SppBurnt_", scn.name, ".tiff"), width=800, height=800)
  gridExtra::grid.arrange(p1, p2, nrow=1)
  dev.off()
  
  
  ##PAPER: % burnt conifers do not regenerate
  dta.conif = dta.burnt %>% filter(spp<=7) %>% group_by(run) %>% summarise(aburnt=sum(aburnt)) 
  dta.noreg = dta.burnt %>% filter(spp %in% c(2,3,4,6,7)) %>% group_by(run) %>% summarise(aburnt=sum(aburnt)) 
  mean(100*dta.noreg$aburnt/dta.conif$aburnt)  
  
}


plot.harvest = function(scn.name, species){
  ## Harvest
  dta.harvest = read.table(paste0("outputs/Scn_", scn.name, "/Harvest.txt"), header=T)
  dta.harvest = dta.harvest %>% mutate(year=year+2010) %>% left_join(species, by="spp")
  ## Group per forest type
  dta.all =  dta.harvest %>% group_by(run, year, type) %>% 
    summarise(vol.sawlog=sum(vol.sawlog), vol.wood=sum(vol.wood)) %>% 
    group_by(year, type) %>% 
    summarise(vol.sawlog=mean(vol.sawlog), vol.wood=mean(vol.wood)) %>% 
    pivot_longer(cols=c("vol.sawlog", "vol.wood"),   values_to="volume") %>% 
    mutate(what=paste0(type, substr(name,4,20)))
  p1 = ggplot(dta.all, aes(x=year, y=volume/10^3, fill=what)) + 
       geom_area() + theme_classic() +
       scale_fill_manual(values=c( "forestgreen", "darkolivegreen1",  "saddlebrown", "palegoldenrod")) 
  tiff(paste0("plots/14.HarvestType_", scn.name, ".tiff"), width=800, height=800)
  gridExtra::grid.arrange(p1, nrow=1)
  dev.off()
}


