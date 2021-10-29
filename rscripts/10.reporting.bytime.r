plot.rel.lc = function(list.scn){
  
  ## Count existences
  rm(dta.all)
  for(scn in list.scn){
    cat(paste(scn, "\n"))
    dta.land = read.table(paste0("outputs/Scn_", scn, "/Land.txt"), header=T)
    dta.land$scn = scn
    dta.land = filter(dta.land, year>10) %>% mutate(year=year+2009)  %>%    
               group_by(run, scn, year, spp) %>% 
               summarise(area=sum(area), vol=sum(volbark))
    dta.ini = filter(dta.land, year==2020) %>% 
              mutate(area.ini=area, vol.ini=vol) %>% select(-area, -vol)
    dta.ini = dta.ini[,-3]  # no colum 'year'
    dta.lc = dta.land %>% left_join(dta.ini, by=c("run", "scn", "spp")) %>%
             mutate(lc = ifelse(spp<=7, "conifers", ifelse(spp<=13, "deciduous",  
                         ifelse(spp==14, "shrub", ifelse(spp==15, "grass", 
                         ifelse(spp<=17, "croplands", ifelse(spp<=19, "other", "urban"))))))) %>% 
              group_by(run, scn, year, lc) %>% 
              summarise(area=sum(area), area.ini=sum(area.ini),
                        vol=sum(vol), vol.ini=sum(vol.ini)) %>% 
              mutate(pctg.area=100*area/area.ini, pctg.vol=100*vol/vol.ini,
                     inc.area=100*(area-area.ini)/area.ini, 
                     inc.vol=100*(vol-vol.ini)/vol.ini) %>% 
              filter(lc!="other") %>% filter(lc!="urban")
    ## Add drivers identifiers
    dta.lc$scenario =  ifelse(scn=="CC", "NULL", sub("CC_", "", scn)) 
    dta.lc$climate = ifelse(length(grep("CC", scn))==0, "Baseline", "RCP 8.5")
    dta.lc$land.chg = ifelse(length(grep("LC", scn))==0, "NO", "YES")
    dta.lc$fire = ifelse(length(grep("WF", scn))==0, "NO", "YES")
    dta.lc$supp = ifelse(length(grep("FS", scn))==0, "NO", "YES")
    dta.lc$mgmt = ifelse(length(grep("FM", scn))==0, "NO", "YES")
    if(exists("dta.all")){
      dta.all = rbind(dta.all, dta.lc)
    }
    else{
      dta.all = dta.lc
    }
  } # scn
  

  ## Summarize data to plot means with standard error
  dta = filter(dta.all, year %in% c(2099-40, 2099)) %>% 
        mutate(land.cover = factor(lc, levels=c("conifers", "deciduous", "shrub", "grass", "croplands"))) %>% 
        group_by(scn, scenario, climate, land.chg, fire, supp, mgmt, land.cover, year) %>% 
        summarise(mn.inc.area=mean(inc.area), sd.inc.area=sd(inc.area)) %>% 
        mutate(scenario = factor(scenario, levels=c("LC_FM_WF_FS", "LC_FM_WF", "LC_WF_FS", "LC_WF", "LC_FM", "LC",
                              "FM_WF_FS",   "FM_WF",  "WF_FS", "WF", "FM", "NULL"))) %>% 
        mutate(year=year+1) 
  
  
  ## 3 Graphics
  dta.plot = dta %>% mutate(x=paste0(year,"_", climate)) %>% filter(x!="2060_Baseline") %>% 
    filter(mgmt=="NO")
  p1 = ggplot(dta.plot, aes(x=mn.inc.area, y=scenario, col=land.cover))+ #, shape=as.factor(year)
    geom_point(size=3)+ geom_vline(xintercept = 0, col="grey50") +
    geom_errorbar(aes(xmin=mn.inc.area-sd.inc.area, xmax=mn.inc.area+sd.inc.area),
                  position=position_dodge(0), size=0.2) +
     theme_bw() + # ggtitle("Relative change of land-cover types") +
    facet_grid(.~climate+year) +  scale_x_continuous("Relative area change (%)") +
    # scale_color_manual(values=c("forestgreen", "brown", "grey50", "darkorchid2", "darkorange2"))
  scale_color_manual(values=c("darkgreen", "darkolivegreen3", "chocolate4", "darkorchid2", "darkgoldenrod1")) 
  tiff("plots/ppt/RelAreaChange_LandCovers.tiff", width=600, height=300)
  p1 
  dev.off()
  
  
  ## Stats
  mod1 = lm(mn.inc.area~climate+land.chg+mgmt+fire+supp, filter(dta, year==2100, land.cover=="conifers"))
  summary(mod1)
  mod2 = lm(mn.inc.area~climate+land.chg+mgmt+fire+supp, filter(dta, year==2100, land.cover=="deciduous"))
  summary(mod2)
  mod3 = lm(mn.inc.area~climate+land.chg+mgmt+fire+supp, filter(dta, year==2100, land.cover=="shrub"))
  summary(mod3)
  mod4 = lm(mn.inc.area~climate+land.chg+mgmt+fire+supp, filter(dta, year==2100, land.cover=="grass"))
  summary(mod4)
  mod5 = lm(mn.inc.area~climate+land.chg+mgmt+fire+supp, filter(dta, year==2100, land.cover=="croplands"))
  summary(mod5)
  
  ## PAPER
  filter(dta.plot, climate=="Baseline", land.chg=="YES", fire=="NO")
  filter(dta.plot, climate=="Baseline",  land.cover=="shrub")
  
}


plot.volume = function(list.scn){
  
  rm(dta.all)
  for(scn in list.scn){
    cat(paste(scn, "\n"))
    ## Existences
    dta.land = read.table(paste0("outputs/Scn_", scn, "/Land.txt"), header=T)
    dta.land$scn = scn
    dta.land$year = dta.land$year+2009
    dta.land = filter(dta.land, year>=2020)  %>% filter(spp<=13) ## from 2020 to 2099, forest
    ## Add drivers identifiers
    dta.land$ftype = ifelse(dta.land$spp<=7, "Conifers", ifelse(dta.land$spp<=13, "Deciduous", NA))
    dta.land$scenario =  ifelse(scn=="CC", "NULL", sub("CC_", "", scn)) 
    dta.land$scenario = factor(dta.land$scenario, levels=c("NULL", "LC", "WF", "WF_FS", "FM", "FM_WF", "FM_WF_FS"))
    dta.land$climate = ifelse(length(grep("CC", scn))==0, "Baseline", "RCP 8.5")
    dta.land$land.chg = ifelse(length(grep("LC", scn))==0, "NO", "YES")
    dta.land$fire = ifelse(length(grep("WF", scn))==0, "NO", "YES")
    dta.land$supp = ifelse(length(grep("FS", scn))==0, "NO", "YES")
    dta.land$mgmt = ifelse(length(grep("FM", scn))==0, "NO", "YES")
    ## Mean (Total / forest type / year)
    if(exists("dta.all")){
      aux = filter(dta.land, !is.na(ftype)) %>% 
            group_by(run, year, scn, scenario, climate, land.chg, fire, supp, mgmt, ftype) %>% 
            summarise(area=sum(area), volbark=sum(volbark), carbon=sum(carbon)) 
      dta.all = rbind(dta.all, aux)
    }
    else
      dta.all = filter(dta.land, !is.na(ftype)) %>% 
                group_by(run, year, scn, scenario, climate, land.chg, fire, supp, mgmt, ftype) %>% 
                summarise(area=sum(area), volbark=sum(volbark), carbon=sum(carbon)) 
  }
  

  ## It does not make sense to try to plot a smooth band around the mean, because the variability
  ## is so small that the region gets overlapped #####
  
  ## Summarize data to plot Total standing volume and relative volume to area over time
  dta.tot = dta.all %>% group_by(run, year, scn, scenario, climate, land.chg, fire, supp, mgmt) %>% 
            summarise(area=sum(area), volbark=sum(volbark), carbon=sum(carbon)) %>% 
            group_by(year, scn, scenario, climate, land.chg, fire, supp, mgmt) %>% 
            summarise(area=mean(area), volbark=mean(volbark), carbon=mean(carbon))
  ylab1 = expression(paste(m^3%.%10^{6}))
  ylab2 = expression(paste(m^3%.%ha^{-1}))
  p1 = ggplot(filter(dta.tot, land.chg=="NO"), aes(x=year, y=volbark/10^6, col=scenario, linetype=climate)) + 
       geom_line(size=1.5) + theme_cowplot() + ggtitle("Standing volume") + scale_y_continuous(ylab1) +
       scale_color_manual(values=c("grey70", "red3", "darkslateblue",  "chartreuse3", "orange2", "darkturquoise")) 
  p2 = ggplot(filter(dta.tot, land.chg=="NO"), aes(x=year, y=volbark/area, col=scenario, linetype=climate)) + 
       geom_line(size=1.5) + theme_cowplot() + ggtitle("Volume per unit area") + scale_y_continuous(ylab2) +
       scale_color_manual(values=c("grey70", "red3", "darkslateblue",  "chartreuse3", "orange2", "darkturquoise")) 
  l = get_legend(p1)
  p1 = p1 + theme(legend.position = "none")
  p2 = p2 + theme(legend.position = "none")
  pp = plot_grid(p1,p2, nrow=1) # ncol=2, labels = c("a)", "b)", "c)", "d)","e)","f)"))
  tiff("plots/15.Volume_NoLandChg.tiff", width=1000, height=500)
  plot_grid(pp,l,ncol=2, rel_widths = c(0.85,0.15))
  dev.off()
  
  ## Total standing volume and relative volume to area, per forest type, over time
  dta.ft = dta.all %>% group_by(year, scn, scenario, climate, land.chg, fire, supp, mgmt, ftype) %>% 
           summarise(area=mean(area), volbark=mean(volbark), carbon=mean(carbon))
  p3 = ggplot(filter(dta.ft, land.chg=="NO"), aes(x=year, y=volbark/10^6, col=scenario, linetype=climate)) + 
       geom_line(size=1.5) + theme_cowplot() + ggtitle("Standing volume") + scale_y_continuous(ylab1) +
       scale_color_manual(values=c("grey70", "red3", "darkslateblue",  "chartreuse3", "orange2", "darkturquoise")) +
       facet_wrap(~ftype, scales="free") 
  p4 = ggplot(filter(dta.ft, land.chg=="NO"), aes(x=year, y=volbark/area, col=scenario, linetype=climate)) + 
       geom_line( size=1.5) +  theme_cowplot() + ggtitle("Volume per unit area") + scale_y_continuous(ylab2) +
       scale_color_manual(values=c("grey70", "red3", "darkslateblue",  "chartreuse3", "orange2", "darkturquoise")) +
       facet_wrap(~ftype) #, scales="free") 
  tiff("plots/ppt/RelVolFtype.tiff", width=800, height=500)
  p4
  dev.off()
  l = get_legend(p3)
  p3 = p3 + theme(legend.position = "none")
  p4 = p4 + theme(legend.position = "none")
  pp = plot_grid(p3,p4, nrow=1) 
  tiff("plots/16.VolumeFtype_NoLandChg.tiff", width=1500, height=500)
  plot_grid(pp,l,ncol=2, rel_widths = c(0.85,0.15))
  dev.off()

  ## Difference at the end of the period 
  ## change the name of the scenarios with LC for plotting purposes
  # p5 = ggplot(filter(dta.all, year==2099), aes(x=scenario, y=volbark/10^6, color=scenario)) + 
  #      geom_boxplot() + facet_grid(land.chg~climate) + ggtitle("Volume in  2100") +
  #      scale_color_manual(values=c("black", "chartreuse3", "red3", "darkslateblue", "orange2", "darkturquoise", "pink")) +
  #      theme(axis.text.x = element_blank())
  # p5
  # p6 = ggplot(filter(dta.all, year==2099), aes(x=scn.lc, y=volbark/area, color=scn.lc)) + 
  #   geom_boxplot(notch=F) + facet_grid(land.chg~climate) +  ggtitle("AGB/Area in 2100") +
  #   scale_color_manual(values=c("black", "chartreuse3", "red3", "darkslateblue", "orange2", "darkturquoise")) +
  #   theme(axis.text.x = element_blank())
  # p6

  ## Volume in 2100
  dta.end = dta.all %>% filter(land.chg=="NO", year==2099)
  p7 = ggplot(dta.end, aes(x=climate, y=volbark/10^6, color=scenario)) + 
        geom_boxplot(position="dodge", size=1) + facet_wrap(~ftype,  scales = "free") + 
        ggtitle("Volume in 2100") + scale_y_continuous(ylab1) +
        scale_color_manual(values=c("grey70", "red3", "darkslateblue",  "chartreuse3", "orange2", "darkturquoise")) +
        theme_cowplot()
  p8 =  ggplot(dta.end, aes(x=climate, y=volbark/area, color=scenario)) + 
        geom_boxplot(position="dodge", size=1) + facet_wrap(~ftype,  scales = "free") + 
        ggtitle("Volume per unit area in 2100") + scale_y_continuous(ylab2) +
        scale_color_manual(values=c("grey70", "red3", "darkslateblue",  "chartreuse3", "orange2", "darkturquoise")) +
        theme_cowplot()
  tiff("plots/ppt/VolUnitArea2100Ftype.tiff", width=700, height=400)
  p8
  dev.off()
  l = get_legend(p7)
  p7 = p7 + theme(legend.position = "none")
  p8 = p8 + theme(legend.position = "none")
  pp = plot_grid(p7,p8, nrow=1) 
  tiff("plots/17.Volume2100_NoLandChg.tiff", width=1500, height=500)
  plot_grid(pp,l,ncol=2, rel_widths = c(0.85,0.15))
  dev.off()
  
  
  ## Carbon
  ylab3 = expression(paste(Mg%.%10^{-6}))
  p9 = ggplot(dta.end, aes(x=climate, y=carbon/10^6, color=scenario)) + 
       geom_boxplot(position="dodge", size=1) + facet_wrap(~ftype,  scales = "free") + 
       scale_y_continuous(ylab3) +  #ggtitle("Carbon stocks in 2100") 
    scale_color_manual(values=c("grey70", "red3", "darkslateblue",  "chartreuse3", "orange2", "darkturquoise")) +
    theme_cowplot()
  tiff("plots/ppt/CarbonStocks2100_NoLandChg.tiff", width=700, height=400)
  p9 
  dev.off()
  
  
  
  ## Stats
  mod1 = lm(volbark/10^6~climate+land.chg+mgmt+fire+supp, filter(dta.all, year==2099, ftype=="Conifers"))
  summary(mod1)
  mod2 = lm(volbark/10^6~climate+land.chg+mgmt+fire+supp, filter(dta.all, year==2099, ftype=="Deciduous"))
  summary(mod2)
  mod3 = lm(volbark/area~climate+land.chg+mgmt+fire+supp, filter(dta.all, year==2099, ftype=="Conifers"))
  summary(mod3)
  mod4 = lm(volbark/area~climate+land.chg+mgmt+fire+supp, filter(dta.all, year==2099, ftype=="Deciduous"))
  summary(mod4)
  ## Coefficients models  
  res = rbind(mod1$coefficients, mod2$coefficients, mod3$coefficients, mod4$coefficients)
  t(res)
  
}


taxo.rich = function(list.scn){
  
  rm(dta.all)
  for(scn in list.scn){
    cat(paste(scn, "\n"))
    ## Existences
    dta.land = read.table(paste0("outputs/Scn_", scn, "/Land.txt"), header=T)
    dta.land$scn = scn
    dta.land$year = dta.land$year+2009
    dta.land = filter(dta.land, year>=2020)  %>% filter(spp<=14) ## from 2020 to 2099, forest and shrub
    ## Add drivers identifiers
    dta.land$scenario =  ifelse(scn=="CC", "NULL", sub("CC_", "", scn)) 
    dta.land$climate = ifelse(length(grep("CC", scn))==0, "Baseline", "RCP 8.5")
    dta.land$land.chg = ifelse(length(grep("LC", scn))==0, "NO", "YES")
    dta.land$fire = ifelse(length(grep("WF", scn))==0, "NO", "YES")
    dta.land$supp = ifelse(length(grep("FS", scn))==0, "NO", "YES")
    dta.land$mgmt = ifelse(length(grep("FM", scn))==0, "NO", "YES")
    ## Mean (Total / forest type / year)
    if(exists("dta.all")){
      aux = dta.land %>% 
            group_by(run, year, scn, scenario, climate, land.chg, fire, supp, mgmt, spp, age.class) %>% 
            summarise(area=sum(area), vol=sum(volbark)) 
      dta.all = rbind(dta.all, aux)
    }
    else
      dta.all = dta.land %>% 
                group_by(run, year, scn, scenario, climate, land.chg, fire, supp, mgmt, spp, age.class) %>% 
                summarise(area=sum(area), vol=sum(volbark)) 
  }
  
  
  ## Shannon index per age class based on all the species (unit: area)
  dta.spp = dta.all %>% filter(year==2099, spp<=13) %>% select(-vol) %>% 
            pivot_wider(names_from=spp, values_from=area, names_prefix="spp")
  dta.spp$spp9[is.na(dta.spp$spp9)] = 0
  dta.spp$shannon = exp(vegan::diversity(dta.spp[,-(1:10)], "shannon"))
  dta1 = dta.spp %>% group_by(scn, scenario, climate, land.chg, fire, supp, mgmt, age.class) %>% 
         summarise(val=mean(shannon), error=sd(shannon))

  ## Stats
  mod1 = lm(shannon~climate+land.chg+fire+supp+mgmt, filter(dta.spp, age.class=="young"))
  summary(mod1)
  mod2 = lm(shannon~climate+land.chg+fire+supp+mgmt, filter(dta.spp, age.class=="mature"))
  summary(mod2)
  mod3 = lm(shannon~climate+land.chg+fire+supp+mgmt, filter(dta.spp, age.class=="old"))
  summary(mod3)

  ## Pctg area per age.class
  dta.tot = dta.all %>% filter(year==2099, spp<=13) %>% 
    group_by(scn, scenario, climate, land.chg, fire, supp, mgmt) %>% 
    summarise(x=sum(area))
  dta.area = dta.all %>% filter(year==2099, spp<=13) %>% 
    group_by(scn, scenario, climate, land.chg, fire, supp, mgmt, age.class) %>% 
    summarise(area=sum(area)) %>% 
    left_join(dta.tot, by=c("scn", "scenario", "climate", "land.chg", "fire", "supp", "mgmt")) %>% 
    mutate(rel.abund=area/x)
  dta.area = dta.area[,c("scn", "age.class", "rel.abund")]
  
  ## Select scenarios with significative drivers 
  dta.plot = dta1 %>% filter(land.chg=="NO", supp=="NO")
  dta.plot$age.class = factor(dta.plot$age.class, levels=c("young", "mature", "old")) 
  dta.plot$scenario = factor(dta.plot$scenario, levels=c("NULL",  "FM", "WF", "FM_WF")) #, "WF_FS", "FM_WF_FS"))
  dta.plot = dta.plot %>% left_join(dta.area, by=c("scn", "age.class"))
  p1 = ggplot(dta.plot, aes(x=scenario, y=val, col=age.class, shape=age.class, size=rel.abund)) +
       geom_point() + facet_grid(.~climate) + theme_bw() +
       geom_errorbar(aes(ymin=val-error, ymax=val+error), position=position_dodge(0), size=0.2) +
       scale_y_continuous("Shannon index") + scale_color_viridis_d(option="viridis")
  p1
    
  
  
  ## Shannon index per age class based on 5 functional types (unit: area)
  dta.ft = dta.all %>% filter(year==2099, spp<=13) %>% select(-vol) %>% 
           # mutate(ftype=ifelse(spp %in% c(1,2,3,5), "pine.med", ifelse(spp %in% c(4,6,7), "mountain.conif", 
           #        ifelse(spp==8, "alzina",  ifelse(spp %in% c(9,10,11), "oak", "decid"))))) %>%
           mutate(ftype=ifelse(spp %in% c(1,2,3,5), "pine.med", ifelse(spp %in% c(4,6,7), "mountain.conif", 
                  ifelse(spp %in% c(8,9,10,11), "quercus", "decid")))) %>% 
           group_by(run, scn, scenario, climate, land.chg, fire, supp, mgmt, age.class, ftype) %>% 
           summarise(area=sum(area))%>% pivot_wider(names_from=ftype, values_from=area)
  dta.ft$shannon = exp(vegan::diversity(dta.ft[,-(1:9)], "shannon"))
  dta2 = dta.ft %>% group_by(scn, scenario, climate, land.chg, fire, supp, mgmt, age.class) %>% 
         summarise(val=mean(shannon), error=sd(shannon))
  
  dta.plot = dta2 %>% filter(land.chg=="NO", supp=="NO")
  dta.plot$age.class = factor(dta.plot$age.class, levels=c("young", "mature", "old")) 
  dta.plot$scenario = factor(dta.plot$scenario, levels=c("NULL", "FM", "WF", "FM_WF")) #, "WF_FS", "FM_WF_FS"))
  dta.plot = dta.plot %>% left_join(dta.area, by=c("scn", "age.class"))
  p2 = ggplot(dta.plot, aes(x=scenario, y=val, col=age.class, shape=age.class, size=rel.abund)) +
    geom_point() + facet_grid(.~climate) + theme_bw() +
    geom_errorbar(aes(ymin=val-error, ymax=val+error), position=position_dodge(0), size=0.2) +
    scale_y_continuous("Shannon index") + scale_color_viridis_d(option="viridis")
  p2
  

  ## Taxo Richness per age class
  l = get_legend(p1)
  p1 = p1 + theme(legend.position = "none")
  p2 = p2 + theme(legend.position = "none")
  pp = plot_grid(p1,p2, nrow=1, labels=c("a)", "b)")) 
  tiff("plots/18.ShannonAgeClass.tiff", width=700, height=300)
  plot_grid(pp,l,ncol=2, rel_widths = c(0.85,0.15))
  dev.off()
  
}


plot.ab.at = function(list.scn){
  
  rm(dta.all)
  for(scn in list.scn){
    cat(paste(scn, "\n"))
    ## Burnt 
    dta.burnt = read.table(paste0("outputs/Scn_", scn, "/Fires.txt"), header=T)
    if(nrow(dta.burnt)>0){
      dta.burnt$scn = scn
      dta.burnt$scenario = ifelse(scn=="CC", "NULL", sub("CC_", "", scn)) 
      dta.burnt$year = dta.burnt$year+2009
      ## Add drivers identifiers
      dta.burnt$climate = ifelse(length(grep("CC", scn))==0, "Baseline", "RCP 8.5")
      dta.burnt$land.chg = ifelse(length(grep("LC", scn))==0, "NO", "YES")
      dta.burnt$supp = ifelse(length(grep("FS", scn))==0, "NO", "YES")
      dta.burnt$mgmt = ifelse(length(grep("FM", scn))==0, "NO", "YES")
      ## Total annual
      if(exists("dta.all")){
        aux = group_by(dta.burnt, run, year, scn, scenario, climate, land.chg, supp, mgmt) %>% 
              summarise(sever=max(clim.sever), aburnt=sum(aburnt.highintens+aburnt.lowintens), 
                    aburnt.high=sum(aburnt.highintens), 
                    asupp=sum(asupp.fuel+asupp.sprd), atarget=sum(atarget))
        dta.all = rbind(dta.all, aux)
      }
      else{
        dta.all = group_by(dta.burnt, run, year, scn, scenario, climate, land.chg, supp, mgmt) %>% 
                  summarise(sever=max(clim.sever), aburnt=sum(aburnt.highintens+aburnt.lowintens), 
                            aburnt.high=sum(aburnt.highintens),
                            asupp=sum(asupp.fuel+asupp.sprd), atarget=sum(atarget))
      }
    }
  } # list.scn
  
  ## Pctg burnt and suppress per year
  dta.all$pct.ab = dta.all$aburnt/dta.all$atarget
  dta.all$pct.as = dta.all$asupp/dta.all$atarget
  dta.all$pct.abh = dta.all$aburnt.high/dta.all$aburnt
  
  
  ## Pctg burnt, pctg suppress and mean fire return interval over the period
  load("inputlyrs/rdata/mask.rdata")
  dta.period = group_by(dta.all, run, scn, scenario, climate, land.chg, supp, mgmt) %>% 
                summarise(aburnt=sum(aburnt), aburnt.high=sum(aburnt.high),
                asupp=sum(asupp), atarget=sum(atarget)) %>% 
              mutate(pct.ab=aburnt/atarget, pct.as=asupp/atarget, pct.abh=aburnt.high/aburnt,
                     fri=ncell(MASK)*80/aburnt)
  
  ## Stats --> land.cover change
  mod1 = lm(fri~climate+land.chg+mgmt+supp, dta.period)
  summary(mod1)
  mod2 = lm(pct.abh*100~climate+land.chg+mgmt+supp, dta.period)
  summary(mod2)
  ## Generalized linear mixed model
  mod3 = lmer(aburnt/100~climate+land.chg+mgmt+supp + (1|year),  na.action = "na.fail", dta.all)
  summary(mod3)
  mod4 = lmer(100*aburnt.high/aburnt~climate+land.chg+mgmt+supp + (1|year),  na.action = "na.fail", dta.all)
  summary(mod4)
  
  
  ## Annual area burnt and pctg of burnt in high intensity
  dta.plot = dta.all %>% filter(land.chg=="NO") %>% 
    group_by(year, scenario, climate) %>% 
    summarise(mn.ab=mean(aburnt), sd.ab=sd(aburnt), mn.pct.abh=mean(pct.abh), sd.pct.abh=sd(pct.abh))
  dta.plot$scenario = factor(dta.plot$scenario, levels=c("WF", "WF_FS", "FM_WF", "FM_WF_FS")) 
  p1 = ggplot(dta.plot, aes(x=year, y=mn.ab/100, col=scenario)) +
       facet_grid(.~climate) + theme_bw() + #ggtitle("Total area burnt") + 
       geom_smooth(formula=y~x, method="loess", size=1.5) + 
       scale_color_manual(values=c("red3", "darkslateblue", "orange2", "darkturquoise")) +
       scale_y_continuous(expression(km^{2}))
  p1
  p2 = ggplot(dta.plot, aes(x=year, y=mn.pct.abh*100, col=scenario)) +
       facet_grid(.~climate) + theme_bw() + #ggtitle("Pct burnt in high intensity") + 
       geom_smooth(formula=y~x, method="loess", size=1.5) + 
       scale_color_manual(values=c("red3", "darkslateblue", "orange2", "darkturquoise")) +
       scale_y_continuous(name="%")
  p2
  
  ## Taxo Richness per age class
  l = get_legend(p1)
  p1 = p1 + theme(legend.position = "none")
  p2 = p2 + theme(legend.position = "none")
  pp = plot_grid(p1,p2, nrow=1, labels=c("a)", "b)")) 
  tiff("plots/19.BurntArea.tiff", width=700, height=300)
  plot_grid(pp,l,ncol=2, rel_widths = c(0.85,0.15))
  dev.off()
  
  
  ## presentations
  p1 = p1 +  ggtitle("Total burnt area") 
  p2 = p2 +  ggtitle("Percentage of high-intensity burnt area") 
  tiff("plots/ppt/BurntArea_PctHighIntens.tiff", width=800, height=300)
  pp = plot_grid(p1,p2, nrow=1) 
  plot_grid(pp,l,ncol=2, rel_widths = c(0.85,0.15))
  dev.off()
  
  ## Doble y axis 
  ggplot(dta.plot, aes(x=year)) +
    # geom_line(aes(y=mn.ab/100, color=scenario), size=1, linetype=1) + 
    # geom_line(aes(y=mn.pct.abh*250, color=scenario), size=1, linetype=2) +
    # geom_line(aes(y=mn.ab/100), size=1, color="red3") + 
    # geom_line(aes(y=mn.pct.abh*250), size=1, color="grey20") +
    geom_smooth(aes(y=mn.ab/100), formula=y~x, method="loess", size=1, color="red3") + 
    geom_smooth(aes(y=mn.pct.abh*250), formula=y~x, method="loess", size=1, color="grey20") +
    scale_y_continuous(
      name = "Total area burnt (km2)",
      sec.axis = sec_axis(~./250, name="Proportion burnt in high-intensity")
    ) + 
    facet_grid(scenario~climate) + theme_bw() 
    # scale_color_manual(values=c("red3", "darkslateblue", "orange2", "darkturquoise")) 
  


  
}

plot.age = function(list.scn){
  
  rm(dta.all); rm(dta.tot)
  for(scn in list.scn){
    ## Existences
    dta.land = read.table(paste0("outputs/Scn_", scn, "/Land.txt"), header=T)
    order.scn = which(scn==list.scn)
    order.scn = ifelse(order.scn<=12, order.scn, order.scn-12)
    order.scn = ifelse(order.scn<10, paste0("0", order.scn), order.scn)
    dta.land$scn = paste0(order.scn, ".", ifelse(scn=="CC", "NULL", sub("CC_", "", scn)))
    print(unique(dta.land$scn))
    dta.land$year = dta.land$year+2009
    dta.land$climate = ifelse(length(grep("CC", scn))==0, "current", "rcp8.5")
    dta.land$land.chg = ifelse(length(grep("LC", scn))==0, "NO", "YES")
    # dta.land$ftype = ifelse(dta.land$spp<=7, "conif", ifelse(dta.land$spp<=13, "decid", NA))
    ## from 2020 to 2099
    dta.land = filter(dta.land, year>=2020) %>% filter(spp<13)
    ## mean (Total / year)
    if(exists("dta.all")){
      aux = filter(dta.land, spp<=13) %>% group_by(scn, climate, run, year, age.class, land.chg) %>% 
        summarise(area=sum(area), volbark=sum(volbark)) 
      dta.all = rbind(dta.all, aux)
      aux = filter(dta.land, spp<=13) %>% group_by(scn, climate, run, year, land.chg) %>% 
        summarise(area=sum(area), volbark=sum(volbark)) 
      dta.tot = rbind(dta.tot, aux)
    }
    else{
      dta.all = filter(dta.land, spp<=13) %>% group_by(scn, climate, run, year, age.class, land.chg) %>%   # spp>7 &
        summarise(area=sum(area), volbark=sum(volbark)) 
      dta.tot= filter(dta.land, spp<=13) %>% group_by(scn, climate, run, year, land.chg) %>% 
        summarise(area=sum(area), volbark=sum(volbark)) 
    }
  }
  rm(aux); rm(order.scn); rm(dta.land)
  
  ## Pct each age class
  names(dta.tot)[6:7] = paste0(names(dta.tot)[6:7], ".tot")
  dta = left_join(dta.all, dta.tot, by=c("scn", "climate", "run", "year", "land.chg")) %>% 
    mutate(pct.area=area/area.tot, pct.volbark=volbark/volbark.tot) %>% 
    group_by(scn, climate, year, age.class, land.chg) %>% summarise(area=mean(area), volbark=mean(volbark),
    pct.area=mean(pct.area), pct.volbark=mean(pct.volbark)) 
    
  dta$age.class= ifelse(dta$age.class=="young", paste0("1.", dta$age.class),
                        ifelse(dta$age.class=="mature", paste0("2.", dta$age.class), 
                               paste0("3.", dta$age.class)))
  
  ## chart plot
  ggplot(dta, aes(x=year, y=pct.area, fill=age.class)) + 
    geom_area(alpha=1 , size=.5, colour="grey70") + scale_fill_viridis(discrete = T) +
    facet_grid(climate~scn) + theme_classic() + theme(legend.position="bottom") + ggtitle("all species - area")

  ## flower plot - 2100
  ggplot(filter(dta, year==2099, land.chg=="NO"), aes(x=age.class, y=pct.area, fill=age.class)) + 
    geom_col() + facet_grid(climate~scn) + coord_polar()+ theme_classic() + 
    scale_fill_viridis(discrete = T) +  theme(legend.position="bottom") 
    # scale_fill_manual(values=c("forestgreen", "gold2", "saddlebrown", "darksalmon"))
  
  
}


plot.carbon.burnt = function(list.scn){
  
  options(warn=-1); rm(dta.all)
  eq.ba.carbon = read.table("inputfiles/EqBasalAreaCarbon.txt", header=T)
  
  for(scn in list.scn){
    ## Burnt per species
    dta.burnt = read.table(paste0("outputs/Scn_", scn, "/BurntSpp.txt"), header=T)
    if(nrow(dta.burnt)>0){
      order.scn = which(scn==list.scn)
      order.scn = ifelse(order.scn<=6, order.scn, order.scn-6)
      dta.burnt$scn = paste0(order.scn, ".", ifelse(scn=="CC", "NULL", sub("CC_", "", scn)))
      dta.burnt$year = dta.burnt$year+2009
      dta.burnt$climate = ifelse(length(grep("CC", scn))==0, "current", "rcp8.5")
      ## Compute forest carbon burnt  
      ## mean (Total / year)
      if(exists("dta.all")){
        aux = filter(dta.burnt, spp<=13) %>% left_join(eq.ba.carbon, by="spp") %>% 
          mutate(carbon=(bburnt/aburnt)*c) %>% group_by(run, year, scn, climate) %>% 
          summarise(carbon=sum(carbon))
        dta.all = rbind(dta.all, aux)
      }
      else{
        dta.all = filter(dta.burnt, spp<=13) %>% left_join(eq.ba.carbon, by="spp") %>% 
        mutate(carbon=(bburnt/aburnt)*c) %>% group_by(run, year, scn, climate) %>% 
        summarise(carbon=sum(carbon))
      }
      rm(aux)
    }
  } # list.scn

  
  ## mean per year
  dta = group_by(dta.all, scn, climate, year) %>% summarise(carbon=mean(carbon)) 
  ## evolution carbon burnt over time per scenario
  ggplot(dta, aes(x=year, y=carbon/10^6, col=scn, linetype=climate))  +
    geom_line(aes(colour=scn, linetype=climate), size=1) + 
    geom_smooth(formula=y~x, method="loess", size=1.5) + #geom_smooth(method="lm", se=F) +
    scale_color_viridis(option="plasma", discrete = T) + theme_classic() + ggtitle("Carbon burnt")
  ## total carbon burnt during all the period (boxplot)
  dta.period = group_by(dta.all, scn, climate, run) %>% summarise(carbon=sum(carbon)/10^6)
  ggplot(dta.period, aes(x=scn, y=carbon, fill=scn)) + geom_boxplot(notch=T) + facet_wrap(~climate) + 
    theme_classic() + scale_fill_viridis(option="plasma", discrete=T) +  ggtitle("Carbon burnt") #geom_violin()
  
}


plot.spp.burnt = function(list.scn){
  
  rm(dta.all)
  for(scn in list.scn){
    ## Burnt per species
    dta.burnt = read.table(paste0("outputs/Scn_", scn, "/BurntSpp.txt"), header=T)
    if(nrow(dta.burnt)>0){
      order.scn = which(scn==list.scn)
      order.scn = ifelse(order.scn<=12, order.scn, order.scn-12)
      order.scn = ifelse(order.scn<10, paste0("0", order.scn), order.scn)
      dta.burnt$scn = paste0(order.scn, ".", ifelse(scn=="CC", "NULL", sub("CC_", "", scn)))
      print(unique(dta.burnt$scn))
      dta.burnt$year = dta.burnt$year+2009
      dta.burnt$climate = ifelse(length(grep("CC", scn))==0, "current", "rcp8.5")
      dta.burnt$land.chg = ifelse(length(grep("LC", scn))==0, "NO", "YES")
      dta.burnt$supp = ifelse(length(grep("FS", scn))==0, "NO", "YES")
      dta.burnt$mgmt = ifelse(length(grep("FM", scn))==0, "NO", "YES")
      if(exists("dta.all")){
        aux = filter(dta.burnt, spp<=17) %>% mutate(cover=ifelse(spp<=7, "conif",
               ifelse(spp<=13, "decid", ifelse(spp==14, "shrub", ifelse(spp %in% c(16,17), "crop", "grass" ))))) %>% 
                group_by(run, year, scn, climate, cover, land.chg, supp, mgmt) %>% summarise(aburnt=sum(aburnt))
        dta.all = rbind(dta.all, aux)
      }
      else{
        dta.all = filter(dta.burnt, spp<=17) %>% mutate(cover=ifelse(spp<=7, "conif",
                   ifelse(spp<=13, "decid", ifelse(spp==14, "shrub", ifelse(spp %in% c(16,17), "crop", "grass"))))) %>%
                   group_by(run, year, scn, climate, cover, land.chg, supp, mgmt) %>% summarise(aburnt=sum(aburnt))
      }
      rm(aux)
    }
  } # list.scn
  
  ## mean per year
  dta = group_by(dta.all, scn, climate, year, cover, mgmt, supp, land.chg) %>% 
         summarise(aburnt=mean(aburnt)) %>% filter(cover!="grass")
  ## evolution area burnt over time per scenario
  ggplot(dta, aes(x=year, y=aburnt/100, col=scn, linetype=climate))  +
    geom_line(aes(colour=scn, linetype=climate), size=1) + 
    geom_smooth(formula=y~x, method="loess", size=1.5) + facet_wrap(~cover, scales="free_y") +
    scale_color_viridis(option="plasma", discrete = T) + theme_classic() + ggtitle("Area burnt / cover")
  ## total area burnt during all the period (boxplot)
  dta.period = group_by(dta.all, scn, climate, run, cover, land.chg, mgmt, supp) %>% summarise(aburnt=sum(aburnt)) %>% filter(cover!="grass") 
  ggplot(dta.period, aes(x=scn, y=aburnt/100, fill=scn)) + geom_boxplot(notch=F) + facet_grid(cover~climate, scales="free_y") + 
    theme_classic() + scale_fill_viridis(option="plasma", discrete=T) +  ggtitle("LC burnt") 
  ## mean total area burnt during all the period (flowerplot)
  dta.period.mean = group_by(dta.period, scn, climate, cover, mgmt, land.chg, supp) %>% 
    summarise(aburnt=mean(aburnt))  
  ggplot(filter(dta.period.mean, supp=="NO"), aes(x=cover, y=aburnt/10^6, fill=cover)) + 
    geom_col() + facet_grid(climate~scn) + coord_polar()+ theme_classic() + 
    scale_fill_manual(values=c("forestgreen", "gold2", "saddlebrown", "darksalmon")) 
  
}


dyn.afforest = function(list.scn){
  
  rm(dta.spp); rm(dta.all)
  for(scn in list.scn){
    cat(paste(scn, "\n"))
    ## Existences
    dta.afforest = read.table(paste0("outputs/Scn_", scn, "/Afforestation.txt"), header=T)
    aux = filter(dta.afforest, year>10) %>% group_by(run) %>% summarise(area=sum(ha))
    dta.afforest = filter(dta.afforest, year>10) %>%   
                   group_by(spp, run) %>% summarise(area=sum(ha)) %>%
                   group_by(spp) %>% summarise(mn.area=mean(area), sd.area=sd(area)) %>% 
                   left_join(species, by="spp") 
    ## Add drivers identifiers
    aux$scenario = dta.afforest$scenario =  ifelse(scn=="CC", "NULL", sub("CC_", "", scn)) 
    aux$climate = dta.afforest$climate = ifelse(length(grep("CC", scn))==0, "Baseline", "RCP 8.5")
    aux$land.chg = dta.afforest$land.chg = ifelse(length(grep("LC", scn))==0, "NO", "YES")
    aux$fire = dta.afforest$fire = ifelse(length(grep("WF", scn))==0, "NO", "YES")
    aux$supp = dta.afforest$supp = ifelse(length(grep("FS", scn))==0, "NO", "YES")
    aux$mgmt = dta.afforest$mgmt = ifelse(length(grep("FM", scn))==0, "NO", "YES")
    ## Mean (Total / forest type / year)
    if(exists("dta.spp")){
      dta.spp = rbind(dta.spp, dta.afforest)
      dta.all = rbind(dta.all, aux)
    }
    else{
      dta.spp = dta.afforest
      dta.all = aux
    }
  }
  
  period = 80
  k = group_by(dta.all, scenario, climate) %>% summarise(tot=mean(area)/100, ypct=mean(area)/(100*period))
  filter(k, scenario=="NULL")
  
  ## Stats
  mod1 = lm(area/100~climate+land.chg+fire+supp+mgmt, dta.all)
  summary(mod1)
  mod2 = lm(area/8000~climate+land.chg+fire+supp+mgmt, dta.all)
  summary(mod2)
  
  ## Flower plot of species
  ggplot(filter(dta.spp, scenario %in% c("NULL", "WF")), aes(x=name, y=mn.area/10^2, fill=name)) + 
    geom_col() + facet_grid(climate~scenario) + coord_polar()+ theme_classic() +
    # scale_fill_manual(values=species.sort$color[1:13]) 
    scale_fill_manual(values=c("royalblue4", "red4", "purple3", "chartreuse3", "darkolivegreen1",
                               "darkslategrey", "seagreen1", "forestgreen", "grey10", "darkorange2",
                               "palegoldenrod", "gold", "saddlebrown"))
  
  
  
  ## PAPER
  rm(rate)
  for(scn in list.scn){
    dta.afforest = read.table(paste0("outputs/Scn_", scn, "/Afforestation.txt"), header=T)
    aux = filter(dta.afforest, year>10) %>% group_by(run, year) %>% summarise(area=sum(ha))
    # annual rate in 80 y, first 40y, second 40y
    r1 = aux %>% group_by(run) %>% summarise(area=sum(area)); round(mean(r1$area/80))
    r2 = aux %>% filter(year<=50) %>% group_by(run) %>% summarise(area=sum(area)); mean(r2$area/40)
    r3 = aux %>% filter(year>50) %>% group_by(run) %>% summarise(area=sum(area)); mean(r3$area/40)
    if(exists("rate")){
      rate=rbind(rate,data.frame(scn=scn, r1=round(mean(r1$area)/80), r2=round(mean(r2$area)/40), r3=round(mean(r3$area)/40)))
    }
    else{
      rate=data.frame(scn=scn, r1=round(mean(r1$area)/80), r2=round(mean(r2$area)/40), r3=round(mean(r3$area)/40))
    }
  }
  
  ## PAPER
  rate$r1[rate$scn=="WF"]-rate$r1[rate$scn=="NULL"]
  rate$r1[rate$scn=="CC_WF"]-rate$r1[rate$scn=="CC"]

  
}


dyn.encroach = function(list.scn){
  
  rm(rate)
  for(scn in list.scn){
    cat(paste(scn, "\n"))
    ## Existences
    dta = read.table(paste0("outputs/Scn_", scn, "/Encroachment.txt"), header=T)
    aux = filter(dta, year>10) %>% group_by(run, year) %>% summarise(area=sum(ha))
    r = aux %>% group_by(run) %>% summarise(area=sum(area))
    if(exists("rate")){
      rate=rbind(rate,data.frame(scn=scn, r=round(mean(r$area)/80)))
    }
    else{
      rate=data.frame(scn=scn, r=round(mean(r$area)/80))
    }
  }
  
  ## PAPER
  rate$r[rate$scn=="WF"]-rate$r[rate$scn=="NULL"]
  rate$r[rate$scn=="CC_WF"]-rate$r[rate$scn=="CC"]
  
}


plot.post.fire = function(){
  
  ## Area of species replaced after fires
  rm(dta.all)
  for(scn in list.scn){
    dta = read.table(paste0("outputs/Scn_", scn, "/PostFire.txt"), header=T)
    dta.burnt = filter(dta, year>10) %>% filter(ha>0) %>% 
                group_by(run, spp.out, spp.in) %>% summarise(ha=sum(ha)) %>%
                group_by(spp.out, spp.in) %>% summarise(ha=mean(ha)) %>% 
                left_join(species, by=c("spp.out"="spp") ) %>%
                mutate(name.out=name) %>% select(-name) %>%
                left_join(species, by=c("spp.in"="spp") ) %>%
                mutate(name.in=name) %>% select(-name)
    all.out = group_by(dta.burnt, spp.out) %>% summarise(tot.ha=sum(ha))
    dta.burnt = left_join(dta.burnt, all.out, by="spp.out") %>% mutate(pctg=100*ha/tot.ha)
    dta.burnt$climate = ifelse(length(grep("CC", scn))==0, "Baseline", "RCP 8.5")
    if(exists("dta.all") & nrow(dta.burnt)>0){
      dta.all = rbind(dta.all, data.frame(scn=scn, dta.burnt))
    }
    else if(nrow(dta.burnt)>0){
      dta.all = data.frame(scn=scn,dta.burnt)
    }
  }
  
  
  ##  PAPER: plot area replaced by new spp / killed spp
  spp = filter(species.sort, name %in% unique(dta.burnt$name.in))
  dta.plot = filter(dta.all, scn %in% c("WF",  "CC_WF")) #"WF_FS", , "CC_WF_FS"
  min.ha.report = group_by(dta.plot, scn, spp.out) %>%  summarise(area=round(sum(ha))) %>%
    pivot_wider(values_from = area, names_from=scn) 
  min.ha.report
  spp.report = group_by(dta.plot, scn, spp.out) %>%  summarise(area=round(sum(ha))) %>%
    mutate(x=area>3000) %>% group_by(spp.out) %>% summarise(include=sum(x)) %>% 
    filter(include>0)
  spp.report
  dta.plot = dta.plot %>% filter(spp.out %in% spp.report$spp.out)
  p1 = ggplot(data=dta.plot, aes(x=namefull.x, y=ha/100, fill=namefull.y)) + geom_bar(stat="identity") +
    scale_fill_manual("Species", values=as.character(spp$color[-3]))+ 
   ylab(expression(km^2)) + xlab("Burnt species") +  #  ggtitle("Species replacement after fire") +
    theme_bw() + theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(~climate) #, scales="free_y") 
  jpeg("plots/ppt/SppChangeAfterFire.jpg", width=700, height=400, quality=99)
  p1
  dev.off()

}

plot.kkruta = function(){
  rm(list = ls())
  load("rscripts/ins/species.rdata")  
  scn.name = "CC_noSPIN"
  dta.land.spp = read.table(paste0("outputs/Scn_", scn.name, "/LandSQI.txt"), header=T) %>% 
    mutate(year=year+2010, sqi=ifelse(sqi==1, "1.low", ifelse(sqi==2, "2.high", "3.optimal"))) %>% 
    left_join(select(species, spp, name), by="spp") %>% mutate(scn="CC")
  scn.name = "NULL_noSPIN"
  aux = read.table(paste0("outputs/Scn_", scn.name, "/LandSQI.txt"), header=T) %>% 
    mutate(year=year+2010, sqi=ifelse(sqi==1, "1.low", ifelse(sqi==2, "2.high", "3.optimal"))) %>% 
    left_join(select(species, spp, name), by="spp") %>% mutate(scn="NULL")
  dta.land.spp = rbind(dta.land.spp, aux)
  
  
  dta.land = dta.land.spp %>% group_by(scn, run, year, sqi) %>% 
    summarise(area=sum(area), vol=sum(vol), volbark=sum(volbark)) %>% 
    group_by(scn, year, sqi) %>% 
    summarise(area=mean(area), vol=mean(vol), volbark=mean(volbark))
  
  dta.year = dta.land %>%  group_by(scn, year) %>% 
    summarise(area=sum(area), vol=sum(vol), volbark=sum(volbark))
  
  p1 = ggplot(dta.year, aes(x=year, y=area/10^3, group=scn)) + 
    geom_line(aes(color=scn), size=2) + theme_classic()
  p2 = ggplot(dta.year, aes(x=year, y=volbark/10^6, group=scn)) + 
    geom_line(aes(color=scn), size=2) + theme_classic()
  gridExtra::grid.arrange(p1,p2, nrow=1)
  
  
}


## spider radar
# kk = filter(dta.all, year==2099) %>% group_by(scn, climate) %>% 
#   summarise(area=mean(area), volbark=mean(volbark), vol.area=mean(volbark/area)) %>% 
#   select(-area, -volbark) %>% 
#   pivot_wider(names_from = scn, values_from = vol.area)
# mx = filter(dta.all, year==2099) %>% group_by(scn) %>% summarise(mx=mean(volbark/area)) %>% 
#   summarise(mx=max(mx))
# aux = matrix(c(rep(mx$mx,6), rep(350,6)), nrow=2, byrow=T)
# aux = as.data.frame(rbind(aux, as.matrix(kk[,-1])))
# names(aux) = c("NULL", "LC", "WF", "LC_WF", "WF_FS", "LC_WF_FS")
# tiff(paste0("plots/17.RadarAGB2100_", set.name, ".tiff"), width=500, height=400)
# radarchart(aux, maxmin=T, axistype=0, seg=4, pty=16,  vlcex=1,       
#            pcol= c("forestgreen", "gold3"), plty=1, plwd=3,  
#            cglcol="grey30", axislabcol="black", centerzero=T, na.itp=F, cex.main=1.4)
# legend(1.2,0.5,legend=c("current","rcp8.5"),
#        col=c("forestgreen", "gold3"), lwd=3, horiz=F, cex=1.3, seg.len=1, bty="n", x.intersp=0.1)
# dev.off()