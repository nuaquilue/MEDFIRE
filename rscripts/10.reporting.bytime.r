play <- function(){
  rm(list=ls())  
  library(raster)
  library(fmsb)
  library(tidyverse)
  library(viridis)
  source("rscripts/10.reporting.bytime.r")
  list.scn <- c("NULL", "LC", "FM", "WF", "WF_FS", "FM_WF", "FM_WF_FS",
                "LC_FM", "LC_WF",  "LC_WF_FS", "LC_FM_WF", "LC_FM_WF_FS",
                "CC", "CC_LC", "CC_FM", "CC_WF", "CC_WF_FS", "CC_FM_WF", "CC_FM_WF_FS",
                "CC_LC_FM", "CC_LC_WF", "CC_LC_WF_FS", "CC_LC_FM_WF", "CC_LC_FM_WF_FS")
  # plot outputs
  plot.agb(list.scn)
  taxo.rich(list.scn)
  plot.age(list.scn)
  plot.carbon.burnt()
  plot.spp.burnt()
  plot.ab.at()
  
}


plot.agb <- function(list.scn){
  
  rm(dta.all); rm(dta.all.ft)
  for(scn in list.scn){
    ## Existences
    dta.land <- read.table(paste0("outputs/Scn_", scn, "/Land.txt"), header=T)
    order.scn <- which(scn==list.scn)
    order.scn <- ifelse(order.scn<=12, order.scn, order.scn-12)
    order.scn <- ifelse(order.scn<10, paste0("0", order.scn), order.scn)
    dta.land$scn <- paste0(order.scn, ".", ifelse(scn=="CC", "NULL", sub("CC_", "", scn)))
    print(unique(dta.land$scn))
    dta.land$year <- dta.land$year+2009
    dta.land$clim <- ifelse(length(grep("CC", scn))==0, "current", "rcp8.5")
    dta.land$land.chg <- ifelse(length(grep("LC", scn))==0, "NO", "YES")
    dta.land$ftype <- ifelse(dta.land$spp<=7, "conif", ifelse(dta.land$spp<=13, "decid", NA))
    ## from 2020 to 2099
    dta.land <- filter(dta.land, year>=2020)  %>% filter(spp<13)
    ## mean (Total / year)
    if(exists("dta.all")){
      aux <- filter(dta.land, spp<=13) %>% group_by(scn, clim, run, year, land.chg) %>% 
        summarise(area=sum(area), volbark=sum(volbark), carbon=sum(carbon)) 
      dta.all <- rbind(dta.all, aux)
    }
    else
      dta.all <- filter(dta.land, spp<=13) %>% group_by(scn, clim, run, year, land.chg) %>% 
                 summarise(area=sum(area), volbark=sum(volbark), carbon=sum(carbon)) 
    ## mean (Total / forest type / year)
    if(exists("dta.all.ft")){
      aux <- filter(dta.land, !is.na(ftype)) %>% group_by(scn, clim, ftype, run, year, land.chg) %>% 
        summarise(area=sum(area), volbark=sum(volbark), carbon=sum(carbon)) 
      dta.all.ft <- rbind(dta.all.ft, aux)
    }
    else
      dta.all.ft <- filter(dta.land, !is.na(ftype)) %>% group_by(scn, clim, ftype, run, year, land.chg) %>% 
      summarise(area=sum(area), volbark=sum(volbark), carbon=sum(carbon)) 
  }
  rm(aux)
  
  ## means
  dta <- group_by(dta.all, scn, clim, year, land.chg) %>% summarise(area=mean(area), volbark=mean(volbark), carbon=mean(carbon))
  dta.ft <- group_by(dta.all.ft, scn, clim, ftype, year, land.chg) %>% summarise(area=mean(area), volbark=mean(volbark), carbon=mean(carbon))
  
  
  #####
  ## It does not make sense to try to plot a smooth band around the mean, because the variability
  ## is so small that the reguib gets overlapped
  #####
  ## Evol plot - AGB
  p1 <- ggplot(filter(dta, scn %in% c("01.NULL", "04.WF", "05.WF_FS", "06.FM_WF", "07.FM_WF_FS")), 
               aes(x=year, y=volbark/10^6, col=scn, linetype=clim)) + 
        geom_line(aes(colour=scn, linetype=clim), size=1.5) +
        # scale_color_viridis_d(option="magma") + 
    scale_color_manual(values=c("black", "red3", "darkslateblue", "orange2", "darkturquoise")) +
    theme_classic() + ggtitle("Aboveground biomass")
  p1
  p2 <- ggplot(filter(dta, scn %in% c("01.NULL", "04.WF", "05.WF_FS", "06.FM_WF", "07.FM_WF_FS")), 
               aes(x=year, y=volbark/area, col=scn, linetype=clim)) + 
    geom_line(aes(colour=scn, linetype=clim), size=1.5) +
    scale_color_manual(values=c("black", "red3", "darkslateblue", "orange2", "darkturquoise")) +
    theme_classic() + ggtitle("Aboveground biomass / area")
  p2
  gridExtra::grid.arrange(p1,p2, nrow=1)
  ## Evol plot - AGB / forest type  
  p3 <- ggplot(filter(dta.ft, scn %in% c("01.NULL", "04.WF", "05.WF_FS", "06.FM_WF", "07.FM_WF_FS")), 
               aes(x=year, y=volbark/10^6, col=scn, linetype=clim)) + 
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_wrap(~ftype, scales="free") + #,  scales = "free") + 
    scale_color_manual(values=c("black", "red3", "darkslateblue", "orange2", "darkturquoise")) +
      theme_classic() + ggtitle("Aboveground biomass") + theme(legend.position = "none")
  p3
  p4 <- ggplot(filter(dta.ft, scn %in% c("01.NULL", "04.WF", "05.WF_FS", "06.FM_WF", "07.FM_WF_FS")), 
               aes(x=year, y=volbark/area, col=scn, linetype=clim)) + 
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_wrap(~ftype) +
    scale_color_manual(values=c("black", "red3", "darkslateblue", "orange2", "darkturquoise")) +
    theme_classic() + ggtitle("Aboveground biomass / area")
  p4
  gridExtra::grid.arrange(p3,p4, nrow=1)
  
  set.name <- "14567"
  tiff(paste0("rscripts/outs/10.AGB-ftype.all_", set.name, ".tiff"), width=1000, height=500)
  gridExtra::grid.arrange(p1,p3, nrow=1)
  dev.off()
  
  
  ## Difference at the end of the period 
  ## change the name of the scenarios with LC for plotting purposes
  dta.all$scn.lc <- ifelse(dta.all$scn=="02.LC", "01.NULL",
                      ifelse(dta.all$scn=="08.LC_FM", "03.FM",
                        ifelse(dta.all$scn=="09.LC_WF", "04.WF",
                          ifelse(dta.all$scn=="10.LC_WF_FS", "05.WF_FS",       
                            ifelse(dta.all$scn=="11.LC_FM_WF", "06.FM_WF",       
                              ifelse(dta.all$scn=="12.LC_FM_WF_FS", "07.FM_WF_FS", dta.all$scn))))))
  p5 <- ggplot(filter(dta.all, year==2099), aes(x=scn.lc, y=volbark/10^6, color=scn.lc)) + 
    geom_boxplot() + facet_grid(land.chg~clim) + ggtitle("AGB in  2100") +
    scale_color_manual(values=c("black", "chartreuse3", "red3", "darkslateblue", "orange2", "darkturquoise")) +
    theme(axis.text.x = element_blank())
  p5
  p6 <- ggplot(filter(dta.all, year==2099), aes(x=scn.lc, y=volbark/area, color=scn.lc)) + 
    geom_boxplot(notch=F) + facet_grid(land.chg~clim) +  ggtitle("AGB/Area in 2100") +
    scale_color_manual(values=c("black", "chartreuse3", "red3", "darkslateblue", "orange2", "darkturquoise")) +
    theme(axis.text.x = element_blank())
  p6
  p7 <- ggplot(filter(dta.all, year==2099), aes(x=land.chg, y=volbark/10^6, color=scn.lc)) + 
    geom_boxplot(position="dodge") + facet_wrap(~clim,  scales = "free") + ggtitle("AGB in 2100") +
    scale_color_manual(values=c("black", "chartreuse3", "red3", "darkslateblue", "orange2", "darkturquoise")) +
    theme_classic()
  p7
  p8 <- ggplot(filter(dta.all, year==2099), aes(x=land.chg, y=volbark/area, color=scn.lc)) + 
    geom_boxplot(position="dodge") + facet_wrap(~clim) + ggtitle("AGB/Area in  2100") +
    scale_color_manual(values=c("black", "chartreuse3", "red3", "darkslateblue", "orange2", "darkturquoise")) +
    theme_classic()
  p8
  tiff("rscripts/outs/10.AGBarea2100.tiff", width=800, height=500)
  gridExtra::grid.arrange(p8, nrow=1)
  dev.off()
  
  
  ## spider radar
  kk <- filter(dta.all, year==2099) %>% group_by(scn, clim) %>% 
    summarise(area=mean(area), volbark=mean(volbark), vol.area=mean(volbark/area)) %>% 
    select(-area, -volbark) %>% 
    pivot_wider(names_from = scn, values_from = vol.area)
  mx <- filter(dta.all, year==2099) %>% group_by(scn) %>% summarise(mx=mean(volbark/area)) %>% 
    summarise(mx=max(mx))
  aux <- matrix(c(rep(mx$mx,6), rep(350,6)), nrow=2, byrow=T)
  aux <- as.data.frame(rbind(aux, as.matrix(kk[,-1])))
  names(aux) <- c("NULL", "LC", "WF", "LC_WF", "WF_FS", "LC_WF_FS")
  
  tiff(paste0("rscripts/outs/11.RadarAGB2100_", set.name, ".tiff"), width=500, height=400)
  radarchart(aux, maxmin=T, axistype=0, seg=4, pty=16,  vlcex=1,       
             pcol= c("forestgreen", "gold3"), plty=1, plwd=3,  
             cglcol="grey30", axislabcol="black", centerzero=T, na.itp=F, cex.main=1.4)
  legend(1.2,0.5,legend=c("current","rcp8.5"),
         col=c("forestgreen", "gold3"), lwd=3, horiz=F, cex=1.3, seg.len=1, bty="n", x.intersp=0.1)
  dev.off()

}


taxo.rich <- function(list.scn){
  
  options(warn=-1)
  rm(dta.all)
  for(scn in list.scn){
    ## Existences
    dta.land <- read.table(paste0("outputs/Scn_", scn, "/Land.txt"), header=T)
    order.scn <- which(scn==list.scn)
    order.scn <- ifelse(order.scn<=6, order.scn, order.scn-6)
    dta.land$scn <- paste0(order.scn, ".", ifelse(scn=="CC", "NULL", sub("CC_", "", scn)))
    dta.land <- filter(dta.land, year>10)
    dta.land$year <- dta.land$year+2010
    dta.land$clim <- ifelse(length(grep("CC", scn))==0, "current", "rcp8.5")
    dta.land$ftype <- ifelse(dta.land$spp<=7, "conif", ifelse(dta.land$spp<=13, "decid", NA))
    
    ## mean (Total / year)
    if(exists("dta.all")){
      aux <- filter(dta.land, spp<=14) %>% 
        group_by(scn, clim, run, year, spp) %>% summarise(vol=sum(vol), area=sum(area)) 
      dta.all <- rbind(dta.all, aux)
    }
    else
      dta.all <- filter(dta.land, spp<=14) %>% group_by(scn, clim, run, year, spp) %>% 
      summarise(vol=sum(vol), area=sum(area)) 
    
  }
  rm(aux)
  
  ## shannon index for all species based on area
  dta <- filter(dta.all, spp<=13) %>% select(-vol) %>% 
          pivot_wider(names_from=spp, values_from=area, names_prefix="spp")
  ## shannon index for all species based on area
  dta <- filter(dta.all, spp<=13) %>% select(-vol) %>% 
    mutate(ftype=ifelse(spp<=1, "pine.med", ifelse(spp<=7, "conif", ifelse(spp<=11, "oak", "decid")))) %>% 
    group_by(scn, clim, run, year, ftype) %>% summarise(area=sum(area)) %>% 
    pivot_wider(names_from=ftype, values_from=area)
  ## shannon index for all species based on vol
  dta.vol <- filter(dta.all, spp<=13) %>% select(-area) %>% 
    pivot_wider(names_from=spp, values_from=vol, names_prefix="spp")
  ## shannon index for 5 functional groups  based on vol
  dta.ft <- filter(dta.all, spp<=13) %>% 
         mutate(ftype=ifelse(spp %in% c(1,2,3,5), "pine.med", ifelse(spp %in% c(4,6,7), "mountain.conif", 
                 ifelse(spp==8, "alzina",  ifelse(spp %in% c(9,10,11), "oak", "decid"))))) %>% 
         group_by(scn, clim, run, year, ftype) %>% summarise(vol=sum(vol)) %>% 
         pivot_wider(names_from=ftype, values_from=vol)
  
  ## compute shannon index and average it
  dta.vol$shannon <- exp(vegan::diversity(dta.vol[,-(1:4)], "shannon"))
  dta.ft$shannon <- exp(vegan::diversity(dta.ft[,-(1:4)], "shannon"))
  # dta$simpson <- exp(vegan::diversity(dta[,-(1:4)], "simpson"))
  dta.avg.vol <- group_by(dta.vol, scn, clim, year) %>% summarise(shannon=mean(shannon))
  dta.avg.ft <- group_by(dta.ft, scn, clim, year) %>% summarise(shannon=mean(shannon))
  # dta.avg.si <- group_by(dta, scn, clim, year) %>% summarise(simpson=mean(simpson))
  
  ## plot evolution shannon                   
  p1 <- ggplot(dta.avg.vol, aes(x=year, y=shannon, col=scn, linetype=clim)) + 
    geom_line(aes(colour=scn, linetype=clim), size=1) + scale_color_viridis_d(option="plasma")  +  
    theme_classic() + ggtitle("Shannon all species") +
    geom_vline(xintercept=2040, color="grey70")+ geom_vline(xintercept=2060, color="grey70")+
    geom_vline(xintercept=2080, color="grey70")
  p2 <- ggplot(dta.avg.ft, aes(x=year, y=shannon, col=scn, linetype=clim)) + 
    geom_line(aes(colour=scn, linetype=clim), size=1) + scale_color_viridis_d(option="plasma")  + 
    theme_classic() + ggtitle("Shannon 5FT")   +
    geom_vline(xintercept=2040, color="grey70")+ geom_vline(xintercept=2060, color="grey70")+
    geom_vline(xintercept=2080, color="grey70")
  
  ### shannon per regions bioclimatiques seria interessant¿¿¿¿???
  set.name <- "all"
  tiff(paste0("rscripts/outs/12.SppRich_", set.name, ".tiff"), width=1000, height=500)
  gridExtra::grid.arrange(p1,p2, nrow=1)
  dev.off()
  
  # RADAR
  radar <- filter(dta.avg.ft, year==2100) %>% group_by(scn, clim) %>% 
    summarise(shannon=mean(shannon)) %>% 
    pivot_wider(names_from = scn, values_from = shannon)
  max(radar[,-1]); min(radar[,-1]);
  aux <- matrix(c(rep(4.25,6), rep(4,6)), nrow=2, byrow=T)
  aux <- as.data.frame(rbind(aux, as.matrix(radar[,-1])))
  names(aux) <- c("NULL", "LC", "WF", "LC_WF", "WF_FS", "LC_WF_FS")

  tiff(paste0("rscripts/outs/13.RadarFTypeRich2100_", set.name, ".tiff"), width=500, height=400)
  radarchart(aux, maxmin=T, axistype=0, seg=4, pty=16,  vlcex=1,       
             pcol= c("forestgreen", "gold3"), plty=1, plwd=3,  
             cglcol="grey30", axislabcol="black", centerzero=T, na.itp=F, cex.main=1.4)
  legend(1.2,0.5,legend=c("current","rcp8.5"),
         col=c("forestgreen", "gold3"), lwd=3, horiz=F, cex=1.3, seg.len=1, bty="n", x.intersp=0.1)
  dev.off()  
  
}


plot.age <- function(list.scn){
  
  rm(dta.all); rm(dta.tot)
  for(scn in list.scn){
    ## Existences
    dta.land <- read.table(paste0("outputs/Scn_", scn, "/Land.txt"), header=T)
    order.scn <- which(scn==list.scn)
    order.scn <- ifelse(order.scn<=12, order.scn, order.scn-12)
    order.scn <- ifelse(order.scn<10, paste0("0", order.scn), order.scn)
    dta.land$scn <- paste0(order.scn, ".", ifelse(scn=="CC", "NULL", sub("CC_", "", scn)))
    print(unique(dta.land$scn))
    dta.land$year <- dta.land$year+2009
    dta.land$clim <- ifelse(length(grep("CC", scn))==0, "current", "rcp8.5")
    dta.land$land.chg <- ifelse(length(grep("LC", scn))==0, "NO", "YES")
    # dta.land$ftype <- ifelse(dta.land$spp<=7, "conif", ifelse(dta.land$spp<=13, "decid", NA))
    ## from 2020 to 2099
    dta.land <- filter(dta.land, year>=2020) %>% filter(spp<13)
    ## mean (Total / year)
    if(exists("dta.all")){
      aux <- filter(dta.land, spp<=13) %>% group_by(scn, clim, run, year, age.class, land.chg) %>% 
        summarise(area=sum(area), volbark=sum(volbark)) 
      dta.all <- rbind(dta.all, aux)
      aux <- filter(dta.land, spp<=13) %>% group_by(scn, clim, run, year, land.chg) %>% 
        summarise(area=sum(area), volbark=sum(volbark)) 
      dta.tot <- rbind(dta.tot, aux)
    }
    else{
      dta.all <- filter(dta.land, spp<=13) %>% group_by(scn, clim, run, year, age.class, land.chg) %>%   # spp>7 &
        summarise(area=sum(area), volbark=sum(volbark)) 
      dta.tot<- filter(dta.land, spp<=13) %>% group_by(scn, clim, run, year, land.chg) %>% 
        summarise(area=sum(area), volbark=sum(volbark)) 
    }
  }
  rm(aux); rm(order.scn); rm(dta.land)
  
  ## Pct each age class
  names(dta.tot)[6:7] <- paste0(names(dta.tot)[6:7], ".tot")
  dta <- left_join(dta.all, dta.tot, by=c("scn", "clim", "run", "year", "land.chg")) %>% 
    mutate(pct.area=area/area.tot, pct.volbark=volbark/volbark.tot) %>% 
    group_by(scn, clim, year, age.class, land.chg) %>% summarise(area=mean(area), volbark=mean(volbark),
    pct.area=mean(pct.area), pct.volbark=mean(pct.volbark)) 
    
  dta$age.class= ifelse(dta$age.class=="young", paste0("1.", dta$age.class),
                        ifelse(dta$age.class=="mature", paste0("2.", dta$age.class), 
                               paste0("3.", dta$age.class)))
  
  ## chart plot
  ggplot(dta, aes(x=year, y=pct.area, fill=age.class)) + 
    geom_area(alpha=1 , size=.5, colour="grey70") + scale_fill_viridis(discrete = T) +
    facet_grid(clim~scn) + theme_classic() + theme(legend.position="bottom") + ggtitle("all species - area")

  ## flower plot - 2100
  ggplot(filter(dta, year==2099, land.chg=="NO"), aes(x=age.class, y=pct.area, fill=age.class)) + 
    geom_col() + facet_grid(clim~scn) + coord_polar()+ theme_classic() + 
    scale_fill_viridis(discrete = T) +  theme(legend.position="bottom") 
    # scale_fill_manual(values=c("forestgreen", "gold2", "saddlebrown", "darksalmon"))
  
  
}


plot.carbon.burnt <- function(list.scn){
  
  options(warn=-1); rm(dta.all)
  eq.ba.carbon <- read.table("inputfiles/EqBasalAreaCarbon.txt", header=T)
  
  for(scn in list.scn){
    ## Burnt per species
    dta.burnt <- read.table(paste0("outputs/Scn_", scn, "/BurntSpp.txt"), header=T)
    if(nrow(dta.burnt)>0){
      order.scn <- which(scn==list.scn)
      order.scn <- ifelse(order.scn<=6, order.scn, order.scn-6)
      dta.burnt$scn <- paste0(order.scn, ".", ifelse(scn=="CC", "NULL", sub("CC_", "", scn)))
      dta.burnt$year <- dta.burnt$year+2009
      dta.burnt$clim <- ifelse(length(grep("CC", scn))==0, "current", "rcp8.5")
      ## Compute forest carbon burnt  
      ## mean (Total / year)
      if(exists("dta.all")){
        aux <- filter(dta.burnt, spp<=13) %>% left_join(eq.ba.carbon, by="spp") %>% 
          mutate(carbon=(bburnt/aburnt)*c) %>% group_by(run, year, scn, clim) %>% 
          summarise(carbon=sum(carbon))
        dta.all <- rbind(dta.all, aux)
      }
      else{
        dta.all <- filter(dta.burnt, spp<=13) %>% left_join(eq.ba.carbon, by="spp") %>% 
        mutate(carbon=(bburnt/aburnt)*c) %>% group_by(run, year, scn, clim) %>% 
        summarise(carbon=sum(carbon))
      }
      rm(aux)
    }
  } # list.scn

  
  ## mean per year
  dta <- group_by(dta.all, scn, clim, year) %>% summarise(carbon=mean(carbon)) 
  ## evolution carbon burnt over time per scenario
  ggplot(dta, aes(x=year, y=carbon/10^6, col=scn, linetype=clim))  +
    geom_line(aes(colour=scn, linetype=clim), size=1) + 
    geom_smooth(formula=y~x, method="loess", size=1.5) + #geom_smooth(method="lm", se=F) +
    scale_color_viridis(option="plasma", discrete = T) + theme_classic() + ggtitle("Carbon burnt")
  ## total carbon burnt during all the period (boxplot)
  dta.period <- group_by(dta.all, scn, clim, run) %>% summarise(carbon=sum(carbon)/10^6)
  ggplot(dta.period, aes(x=scn, y=carbon, fill=scn)) + geom_boxplot(notch=T) + facet_wrap(~clim) + 
    theme_classic() + scale_fill_viridis(option="plasma", discrete=T) +  ggtitle("Carbon burnt") #geom_violin()
  
}


plot.spp.burnt <- function(list.scn){
  
  rm(dta.all)
  for(scn in list.scn){
    ## Burnt per species
    dta.burnt <- read.table(paste0("outputs/Scn_", scn, "/BurntSpp.txt"), header=T)
    if(nrow(dta.burnt)>0){
      order.scn <- which(scn==list.scn)
      order.scn <- ifelse(order.scn<=12, order.scn, order.scn-12)
      order.scn <- ifelse(order.scn<10, paste0("0", order.scn), order.scn)
      dta.burnt$scn <- paste0(order.scn, ".", ifelse(scn=="CC", "NULL", sub("CC_", "", scn)))
      print(unique(dta.burnt$scn))
      dta.burnt$year <- dta.burnt$year+2009
      dta.burnt$clim <- ifelse(length(grep("CC", scn))==0, "current", "rcp8.5")
      dta.burnt$land.chg <- ifelse(length(grep("LC", scn))==0, "NO", "YES")
      dta.burnt$supp <- ifelse(length(grep("FS", scn))==0, "NO", "YES")
      dta.burnt$mgmt <- ifelse(length(grep("FM", scn))==0, "NO", "YES")
      if(exists("dta.all")){
        aux <- filter(dta.burnt, spp<=17) %>% mutate(cover=ifelse(spp<=7, "conif",
               ifelse(spp<=13, "decid", ifelse(spp==14, "shrub", ifelse(spp %in% c(16,17), "crop", "grass" ))))) %>% 
                group_by(run, year, scn, clim, cover, land.chg, supp, mgmt) %>% summarise(aburnt=sum(aburnt))
        dta.all <- rbind(dta.all, aux)
      }
      else{
        dta.all <- filter(dta.burnt, spp<=17) %>% mutate(cover=ifelse(spp<=7, "conif",
                   ifelse(spp<=13, "decid", ifelse(spp==14, "shrub", ifelse(spp %in% c(16,17), "crop", "grass"))))) %>%
                   group_by(run, year, scn, clim, cover, land.chg, supp, mgmt) %>% summarise(aburnt=sum(aburnt))
      }
      rm(aux)
    }
  } # list.scn
  
  ## mean per year
  dta <- group_by(dta.all, scn, clim, year, cover, mgmt, supp, land.chg) %>% 
         summarise(aburnt=mean(aburnt)) %>% filter(cover!="grass")
  ## evolution area burnt over time per scenario
  ggplot(dta, aes(x=year, y=aburnt/100, col=scn, linetype=clim))  +
    geom_line(aes(colour=scn, linetype=clim), size=1) + 
    geom_smooth(formula=y~x, method="loess", size=1.5) + facet_wrap(~cover, scales="free_y") +
    scale_color_viridis(option="plasma", discrete = T) + theme_classic() + ggtitle("Area burnt / cover")
  ## total area burnt during all the period (boxplot)
  dta.period <- group_by(dta.all, scn, clim, run, cover, land.chg, mgmt, supp) %>% summarise(aburnt=sum(aburnt)) %>% filter(cover!="grass") 
  ggplot(dta.period, aes(x=scn, y=aburnt/100, fill=scn)) + geom_boxplot(notch=F) + facet_grid(cover~clim, scales="free_y") + 
    theme_classic() + scale_fill_viridis(option="plasma", discrete=T) +  ggtitle("LC burnt") 
  ## mean total area burnt during all the period (flowerplot)
  dta.period.mean <- group_by(dta.period, scn, clim, cover, mgmt, land.chg, supp) %>% 
    summarise(aburnt=mean(aburnt))  
  ggplot(filter(dta.period.mean, supp=="NO"), aes(x=cover, y=aburnt/10^6, fill=cover)) + 
    geom_col() + facet_grid(clim~scn) + coord_polar()+ theme_classic() + 
    scale_fill_manual(values=c("forestgreen", "gold2", "saddlebrown", "darksalmon")) 
  
}


plot.ab.at <- function(list.scn){

  rm(dta.all)
  for(scn in list.scn){
    ## Burnt 
    dta.burnt <- read.table(paste0("outputs/Scn_", scn, "/Fires.txt"), header=T)
    if(nrow(dta.burnt)>0){
      order.scn <- which(scn==list.scn)
      order.scn <- ifelse(order.scn<=12, order.scn, order.scn-12)
      order.scn <- ifelse(order.scn<10, paste0("0", order.scn), order.scn)
      dta.burnt$scn <- paste0(order.scn, ".", ifelse(scn=="CC", "NULL", sub("CC_", "", scn)))
      print(unique(dta.burnt$scn))
      dta.burnt$year <- dta.burnt$year+2009
      dta.burnt$clim <- ifelse(length(grep("CC", scn))==0, "current", "rcp8.5")
      dta.burnt$land.chg <- ifelse(length(grep("LC", scn))==0, "NO", "YES")
      dta.burnt$supp <- ifelse(length(grep("FS", scn))==0, "NO", "YES")
      dta.burnt$mgmt <- ifelse(length(grep("FM", scn))==0, "NO", "YES")
      
      if(exists("dta.all")){
        aux <- group_by(dta.burnt, run, year, scn, clim, land.chg, supp, mgmt) %>% 
               summarise(sever=max(clim.sever), aburnt=sum(aburnt.highintens+aburnt.lowintens), 
                         aburnt.high=sum(aburnt.highintens), 
                         asupp=sum(asupp.fuel+asupp.sprd), atarget=sum(atarget))
        dta.all <- rbind(dta.all, aux)
      }
      else{
        dta.all <- group_by(dta.burnt, run, year, scn, clim, land.chg, supp, mgmt) %>% 
                   summarise(sever=max(clim.sever), aburnt=sum(aburnt.highintens+aburnt.lowintens), 
                             aburnt.high=sum(aburnt.highintens),
                             asupp=sum(asupp.fuel+asupp.sprd), atarget=sum(atarget))
      }
    }
  } # list.scn
  
  ## Pctg burnt and suppress per year
  dta.all$pct.ab <- dta.all$aburnt/dta.all$atarget
  dta.all$pct.as <- dta.all$asupp/dta.all$atarget
  dta.all$pct.abh <- dta.all$aburnt.high/dta.all$aburnt
  
  
  ## Number of sever years
  dta.all %>% group_by(run, scn, clim, land.chg, supp, mgmt) %>% summarise(nsever=sum(sever)) %>% 
    group_by(scn, clim, land.chg, supp, mgmt) %>% summarise(nsever=round(mean(nsever)))
  
  
  ## Pctg burnt and suppress in the period
  load("inputlyrs/rdata/mask.rdata")
  dta.period <- group_by(dta.all, scn, clim, run, land.chg, supp, mgmt) %>% 
                summarise(aburnt=sum(aburnt), aburnt.high=sum(aburnt.high),
                          asupp=sum(asupp), atarget=sum(atarget)) %>% 
                mutate(pct.ab=aburnt/atarget, pct.as=asupp/atarget, pct.abh=aburnt.high/aburnt,
                       fri=ncell(MASK)*80/aburnt)
  
  # mod <- glm(fri~clim+mgmt+land.chg+supp, dta.period, family = "gaussian")
  # summary(mod)
  # MuMIn::r.squaredGLMM(mod, null, enviar = parent.frame(), pj2014 = FALSE)
  mod <- lm(fri~clim+mgmt+land.chg+supp, dta.period)
  summary(mod)
  mod <- lm(pct.abh~clim+mgmt+land.chg+supp, dta.period)
  summary(mod)
  
  
  ## Mean fire return interval
  dta.period %>% group_by(scn, clim, land.chg) %>% summarise(aburnt=mean(aburnt)) %>% 
    mutate(mfri=ncell(MASK)*80/aburnt) %>% dplyr::select(-aburnt) %>%  
    pivot_wider(names_from = clim, values_from=mfri)
  
  ## Area burnt
  dta.period %>% group_by(scn, clim, land.chg) %>% summarise(aburnt=mean(aburnt)/10^6) %>% 
    pivot_wider(names_from = clim, values_from=aburnt) %>% mutate(dif=rcp8.5-current)
  
  
  ## evolution percentage area burnt over time per scenario
  # ggplot(filter(dta.all, land.chg=="NO"), aes(x=year, y=pct.abh, col=scn, linetype=clim))  +
  #   geom_line(aes(colour=scn, linetype=clim), size=1) + geom_smooth(formula=y~x, method="loess", size=1.5) + 
  #   scale_color_viridis(option="plasma", discrete = T) + theme_classic() + ggtitle("Pct burnt area")
  
  
  ## total area burnt during all the period (boxplot)
  p1 <- ggplot(filter(dta.period, land.chg=="NO"), aes(x=scn, y=aburnt/10^6, fill=scn)) + 
    geom_boxplot(notch=F) + facet_grid(land.chg~clim) + theme_classic() + 
    theme(axis.text.x = element_blank()) +
    scale_fill_manual(values=c( "red3", "darkslateblue", "orange2", "darkturquoise")) +  ggtitle("Burnt area") 
  p1
  p2 <- ggplot(filter(dta.period, land.chg=="YES"), aes(x=scn, y=aburnt/10^6, fill=scn)) + 
    geom_boxplot(notch=F) + facet_grid(land.chg~clim) + theme_classic() + 
    theme(axis.text.x = element_blank()) +
    scale_fill_manual(values=c( "red3", "darkslateblue", "orange2", "darkturquoise")) +  ggtitle("Burnt area") 
  p2
  gridExtra::grid.arrange(p1,p2, nrow=1)
  
  
  dta.period$scn.lc <- ifelse(dta.period$scn=="09.LC_WF", "04.WF",
                        ifelse(dta.period$scn=="10.LC_WF_FS", "05.WF_FS",       
                          ifelse(dta.period$scn=="11.LC_FM_WF", "06.FM_WF",       
                            ifelse(dta.period$scn=="12.LC_FM_WF_FS", "07.FM_WF_FS", dta.period$scn))))
  ggplot(dta.period, aes(x=scn.lc, y=aburnt/10^6, fill=scn.lc)) + 
    geom_boxplot(notch=F) + facet_wrap(land.chg~clim) + theme_classic() + 
    theme(axis.text.x = element_blank()) +  ggtitle("Burnt area") +
    scale_fill_manual(values=rep(c("red3", "darkslateblue", "orange2", "darkturquoise"),2)) 
  
  
  ## % burnt in high intensity
  ggplot(dta.period, aes(x=scn.lc, y=pct.abh, fill=scn.lc)) + 
    geom_boxplot(notch=F) + facet_wrap(land.chg~clim) + theme_classic() + 
    theme(axis.text.x = element_blank()) +  ggtitle("% high intensity") +
    scale_fill_manual(values=rep(c("red3", "darkslateblue", "orange2", "darkturquoise"),2)) 
  
  ## % Area burnt in HIGH INTESNITY
  dta.period %>% group_by(scn, clim, land.chg) %>% summarise(aburnt=mean(pct.abh)) %>% 
    pivot_wider(names_from = clim, values_from=aburnt) %>% mutate(dif=rcp8.5-current)
  mod <- lm(pct.abh~clim+mgmt+land.chg+supp, dta.period)
  summary(mod)
  
  
}



plot.kkruta <- function(){
  rm(list = ls())
  load("rscripts/ins/species.rdata")  
  scn.name <- "CC_noSPIN"
  dta.land.spp <- read.table(paste0("outputs/Scn_", scn.name, "/LandSQI.txt"), header=T) %>% 
    mutate(year=year+2010, sqi=ifelse(sqi==1, "1.low", ifelse(sqi==2, "2.high", "3.optimal"))) %>% 
    left_join(select(species, spp, name), by="spp") %>% mutate(scn="CC")
  scn.name <- "NULL_noSPIN"
  aux <- read.table(paste0("outputs/Scn_", scn.name, "/LandSQI.txt"), header=T) %>% 
    mutate(year=year+2010, sqi=ifelse(sqi==1, "1.low", ifelse(sqi==2, "2.high", "3.optimal"))) %>% 
    left_join(select(species, spp, name), by="spp") %>% mutate(scn="NULL")
  dta.land.spp <- rbind(dta.land.spp, aux)
  
  
  dta.land <- dta.land.spp %>% group_by(scn, run, year, sqi) %>% 
    summarise(area=sum(area), vol=sum(vol), volbark=sum(volbark)) %>% 
    group_by(scn, year, sqi) %>% 
    summarise(area=mean(area), vol=mean(vol), volbark=mean(volbark))
  
  dta.year <- dta.land %>%  group_by(scn, year) %>% 
    summarise(area=sum(area), vol=sum(vol), volbark=sum(volbark))
  
  p1 <- ggplot(dta.year, aes(x=year, y=area/10^3, group=scn)) + 
    geom_line(aes(color=scn), size=2) + theme_classic()
  p2 <- ggplot(dta.year, aes(x=year, y=volbark/10^6, group=scn)) + 
    geom_line(aes(color=scn), size=2) + theme_classic()
  gridExtra::grid.arrange(p1,p2, nrow=1)
  
  
}