play <- function(){
  rm(list=ls())  
  library(raster)
  library(fmsb)
  library(tidyverse)
  library(viridis)
  source("rscripts/10.reporting.bytime.r")
  list.scn <- c("NULL", "LC", "WF", "LC_WF", "WF_FS", "LC_WF_FS",
                "CC", "CC_LC", "CC_WF",  "CC_LC_WF",  "CC_WF_FS","CC_LC_WF_FS")
  # plot outputs
  plot.agb(list.scn)
  taxo.rich(list.scn)
  plot.age(list.scn)
  plot.carbon.burnt()
  plot.spp.burnt()
  plot.ab.at()
}


plot.agb <- function(list.scn){
  
  options(warn=-1)
  rm(dta.all); rm(dta.all.ft)
  for(scn in list.scn){
    ## Existences
    dta.land <- read.table(paste0("outputs/Scn_", scn, "/Land.txt"), header=T)
    order.scn <- which(scn==list.scn)
    order.scn <- ifelse(order.scn<=6, order.scn, order.scn-6)
    dta.land$scn <- paste0(order.scn, ".", ifelse(scn=="CC", "NULL", sub("CC_", "", scn)))
    dta.land$year <- dta.land$year+2009
    dta.land$clim <- ifelse(length(grep("CC", scn))==0, "current", "rcp8.5")
    dta.land$ftype <- ifelse(dta.land$spp<=7, "conif", ifelse(dta.land$spp<=13, "decid", NA))
    ## from 2020 to 2099
    dta.land <- filter(dta.land, year>=2020)
    ## mean (Total / year)
    if(exists("dta.all")){
      aux <- filter(dta.land, spp<=13) %>% group_by(scn, clim, run, year) %>% 
        summarise(area=sum(area), volbark=sum(volbark), carbon=sum(carbon)) 
      dta.all <- rbind(dta.all, aux)
    }
    else
      dta.all <- filter(dta.land, spp<=13) %>% group_by(scn, clim, run, year) %>% 
                 summarise(area=sum(area), volbark=sum(volbark), carbon=sum(carbon)) 
    ## mean (Total / forest type / year)
    if(exists("dta.all.ft")){
      aux <- filter(dta.land, !is.na(ftype)) %>% group_by(scn, clim, ftype, run, year) %>% 
        summarise(area=sum(area), volbark=sum(volbark), carbon=sum(carbon)) 
      dta.all.ft <- rbind(dta.all.ft, aux)
    }
    else
      dta.all.ft <- filter(dta.land, !is.na(ftype)) %>% group_by(scn, clim, ftype, run, year) %>% 
      summarise(area=sum(area), volbark=sum(volbark), carbon=sum(carbon)) 
  }
  rm(aux)
  
  ## means
  dta <- group_by(dta.all, scn, clim, year) %>% summarise(area=mean(area), volbark=mean(volbark), carbon=mean(carbon))
  dta.ft <- group_by(dta.all.ft, scn, clim, ftype, year) %>% summarise(area=mean(area), volbark=mean(volbark), carbon=mean(carbon))
  
  
  #####
  ## It does not make sense to try to plot a smooth band around the mean, because the variability
  ## is so small that the reguib gets overlapped
  #####
  ## Evol plot - AGB
  p1 <- ggplot(filter(dta, scn %in% c("1.NULL", "3.WF", "5.WF_FS")), aes(x=year, y=volbark, col=scn, linetype=clim)) + 
        geom_line(aes(colour=scn, linetype=clim), size=1.5) +
        scale_color_viridis_d(option="plasma") + theme_classic() + ggtitle("Aboveground biomass")
  p2 <- ggplot(filter(dta, scn %in% c("1.NULL", "3.WF", "5.WF_FS")), aes(x=year, y=volbark/area, col=scn, linetype=clim)) + 
    geom_line(aes(colour=scn, linetype=clim), size=1.5) +
    scale_color_viridis_d(option="plasma") + theme_classic() + ggtitle("Aboveground biomass / area")
  gridExtra::grid.arrange(p1,p2, nrow=1)
  ## Evol plot - AGB / forest type  
  p3 <- ggplot(filter(dta.ft, scn %in% c("1.NULL", "3.WF", "5.WF_FS")), 
               aes(x=year, y=volbark, col=scn, linetype=clim)) + 
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_wrap(~ftype) +
    scale_color_viridis_d(option="plasma") + theme_classic() + ggtitle("Aboveground biomass")
  p4 <- ggplot(filter(dta.ft, scn %in% c("1.NULL", "3.WF", "5.WF_FS")), 
               aes(x=year, y=volbark/area, col=scn, linetype=clim)) + 
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_wrap(~ftype) +
    scale_color_viridis_d(option="plasma") + theme_classic() + ggtitle("Aboveground biomass / area")
  gridExtra::grid.arrange(p3,p4, nrow=1)
  
  set.name <- "135"
  tiff(paste0("rscripts/outs/09.AGB-ftype_", set.name, ".tiff"), width=800, height=700)
  gridExtra::grid.arrange(p1,p2,p3,p4, nrow=2)
  dev.off()
  
  
  ## Difference at the end of the period 
  p5 <- ggplot(filter(dta.all, year==2099), aes(x=scn, y=volbark/10^6, fill=scn)) + 
    geom_boxplot(notch=F) + facet_grid(~clim) + 
    theme_classic() + scale_fill_viridis(option="plasma", discrete=T) +  ggtitle("AGB 2100") #geom_violin()
  p6 <- ggplot(filter(dta.all, year==2099), aes(x=scn, y=volbark/area, fill=scn)) + 
    geom_boxplot(notch=F) + facet_grid(~clim) + 
    theme_classic() + scale_fill_viridis(option="plasma", discrete=T) +  ggtitle("AGB/area 2100") #geom_violin()
  # ggplot(filter(dta.all.ft, year==2099), aes(x=scn, y=volbark, fill=scn)) + 
  #   geom_boxplot(notch=F) + facet_grid(ftype~clim) + 
  #   theme_classic() + scale_fill_viridis(option="plasma", discrete=T) +  ggtitle("AGB 2100") #geom_violin()
    set.name <- "135"
  tiff(paste0("rscripts/outs/10.AGB2100_", set.name, ".tiff"), width=800, height=400)
  gridExtra::grid.arrange(p5,p6, nrow=1)
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
  
  options(warn=-1)
  rm(dta.all); rm(dta.tot)
  for(scn in list.scn){
    ## Existences
    dta.land <- read.table(paste0("outputs/spatial/Scn_", scn, "/Land.txt"), header=T)
    order.scn <- which(scn==list.scn)
    order.scn <- ifelse(order.scn<=6, order.scn, order.scn-6)
    dta.land$scn <- paste0(order.scn, ".", ifelse(scn=="CC", "NULL", sub("CC_", "", scn)))
    dta.land$year <- dta.land$year+2009
    dta.land$clim <- ifelse(length(grep("CC", scn))==0, "current", "rcp8.5")
    # dta.land$ftype <- ifelse(dta.land$spp<=7, "conif", ifelse(dta.land$spp<=13, "decid", NA))
    ## from 2020 to 2099
    dta.land <- filter(dta.land, year>=2020)
    ## mean (Total / year)
    if(exists("dta.all")){
      aux <- filter(dta.land, spp<=13) %>% group_by(scn, clim, run, year, age.class) %>% 
        summarise(area=sum(area), volbark=sum(volbark)) 
      dta.all <- rbind(dta.all, aux)
      aux <- filter(dta.land, spp<=13) %>% group_by(scn, clim, run, year) %>% 
        summarise(area=sum(area), volbark=sum(volbark)) 
      dta.tot <- rbind(dta.tot, aux)
    }
    else{
      dta.all <- filter(dta.land, spp<=13) %>% group_by(scn, clim, run, year, age.class) %>%   # spp>7 &
        summarise(area=sum(area), volbark=sum(volbark)) 
      dta.tot<- filter(dta.land, spp<=13) %>% group_by(scn, clim, run, year) %>% 
        summarise(area=sum(area), volbark=sum(volbark)) 
    }
  }
  rm(aux); rm(order.scn); rm(dta.land)
  
  ## Pct each age class
  names(dta.tot)[5:6] <- paste0(names(dta.tot)[5:6], ".tot")
  dta <- left_join(dta.all, dta.tot, by=c("scn", "clim", "run", "year")) %>% 
    mutate(pct.area=area/area.tot, pct.volbark=volbark/volbark.tot) %>% 
    group_by(scn, clim, year, age.class) %>% summarise(area=mean(area), volbark=mean(volbark),
    pct.area=mean(pct.area), pct.volbark=mean(pct.volbark))
  
  ## chart plot
  ggplot(dta, aes(x=year, y=pct.area, fill=age.class)) + 
    geom_area(alpha=1 , size=.5, colour="grey70") + scale_fill_viridis(discrete = T) +
    facet_grid(clim~scn) + theme_classic() + theme(legend.position="bottom") + ggtitle("all species - area")

  ## flower plot - 2100
  ggplot(filter(dta, year==2099), aes(x=age.class, y=pct.area, fill=age.class)) + 
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
  
  options(warn=-1); rm(dta.all)
  for(scn in list.scn){
    ## Burnt per species
    dta.burnt <- read.table(paste0("outputs/Scn_", scn, "/BurntSpp.txt"), header=T)
    if(nrow(dta.burnt)>0){
      order.scn <- which(scn==list.scn)
      order.scn <- ifelse(order.scn<=6, order.scn, order.scn-6)
      dta.burnt$scn <- paste0(order.scn, ".", ifelse(scn=="CC", "NULL", sub("CC_", "", scn)))
      dta.burnt$year <- dta.burnt$year+2009
      dta.burnt$clim <- ifelse(length(grep("CC", scn))==0, "current", "rcp8.5")
      if(exists("dta.all")){
        aux <- filter(dta.burnt, spp<=17) %>% mutate(cover=ifelse(spp<=7, "conif",
               ifelse(spp<=13, "decid", ifelse(spp==14, "shrub", ifelse(spp %in% c(16,17), "crop", "grass" ))))) %>% 
                group_by(run, year, scn, clim, cover) %>% summarise(aburnt=sum(aburnt))
        dta.all <- rbind(dta.all, aux)
      }
      else{
        dta.all <- filter(dta.burnt, spp<=17) %>% mutate(cover=ifelse(spp<=7, "conif",
                   ifelse(spp<=13, "decid", ifelse(spp==14, "shrub", ifelse(spp %in% c(16,17), "crop", "grass"))))) %>%
                   group_by(run, year, scn, clim, cover) %>% summarise(aburnt=sum(aburnt))
      }
      rm(aux)
    }
  } # list.scn
  
  ## mean per year
  dta <- group_by(dta.all, scn, clim, year, clim, cover) %>% summarise(aburnt=mean(aburnt)) %>% 
        filter(cover!="grass")
  ## evolution area burnt over time per scenario
  ggplot(dta, aes(x=year, y=aburnt/100, col=scn, linetype=clim))  +
    geom_line(aes(colour=scn, linetype=clim), size=1) + 
    geom_smooth(formula=y~x, method="loess", size=1.5) + facet_wrap(~cover, scales="free_y") +
    scale_color_viridis(option="plasma", discrete = T) + theme_classic() + ggtitle("Area burnt / cover")
  ## total area burnt during all the period (boxplot)
  dta.period <- group_by(dta.all, scn, clim, run, cover) %>% summarise(aburnt=sum(aburnt)) %>% filter(cover!="grass") 
  ggplot(dta.period, aes(x=scn, y=aburnt/100, fill=scn)) + geom_boxplot(notch=T) + facet_grid(cover~clim, scales="free_y") + 
    theme_classic() + scale_fill_viridis(option="plasma", discrete=T) +  ggtitle("LC burnt") 
  ## mean total area burnt during all the period (flowerplot)
  dta.period.mean <- group_by(dta.period, scn, clim, cover) %>% summarise(aburnt=mean(aburnt))  
  ggplot(dta.period.mean, aes(x=cover, y=aburnt/10^6, fill=cover)) + 
    geom_col() + facet_grid(clim~scn) + coord_polar()+ theme_classic() + 
    scale_fill_manual(values=c("forestgreen", "gold2", "saddlebrown", "darksalmon")) 
  
}


plot.ab.at <- function(list.scn){

  options(warn=-1); rm(dta.all)
  for(scn in list.scn){
    ## Burnt per species
    dta.burnt <- read.table(paste0("outputs/Scn_", scn, "/Fires.txt"), header=T)
    if(nrow(dta.burnt)>0){
      order.scn <- which(scn==list.scn)
      order.scn <- ifelse(order.scn<=6, order.scn, order.scn-6)
      dta.burnt$scn <- paste0(order.scn, ".", ifelse(scn=="CC", "NULL", sub("CC_", "", scn)))
      dta.burnt$year <- dta.burnt$year+2009
      dta.burnt$clim <- ifelse(length(grep("CC", scn))==0, "current", "rcp8.5")
      
      if(exists("dta.all")){
        aux <- group_by(dta.burnt, run, year, scn, clim) %>% summarise(aburnt=sum(aburnt.highintens+aburnt.lowintens), 
                 aburnt.high=sum(aburnt.highintens), asupp=sum(asupp.fuel+asupp.sprd), atarget=sum(atarget))
        dta.all <- rbind(dta.all, aux)
      }
      else{
        dta.all <- group_by(dta.burnt, run, year, scn, clim) %>% summarise(aburnt=sum(aburnt.highintens+aburnt.lowintens), 
                    aburnt.high=sum(aburnt.highintens), asupp=sum(asupp.fuel+asupp.sprd), atarget=sum(atarget))
      }
      rm(aux)
    }
  } # list.scn
  
  ## Pctg burnt and suppress per year
  dta.all$pct.ab <- dta.all$aburnt/dta.all$atarget
  dta.all$pct.as <- dta.all$asupp/dta.all$atarget
  dta.all$pct.abh <- dta.all$aburnt.high/dta.all$atarget
  
  ## Pctg burnt and suppress in the period
  dta.period <- group_by(dta.all, scn, clim, run) %>% summarise(aburnt=sum(aburnt), aburnt.high=sum(aburnt.high),
                    asupp=sum(asupp), atarget=sum(atarget)) %>% mutate(pct.ab=aburnt/atarget, pct.as=asupp/atarget,
                    pct.abh=aburnt.high/atarget)
                                                                
  
  
  ## evolution percentage area burnt over time per scenario
  ggplot(dta.all, aes(x=year, y=pct.abh, col=scn, linetype=clim))  +
    geom_line(aes(colour=scn, linetype=clim), size=1) + geom_smooth(formula=y~x, method="loess", size=1.5) + 
    scale_color_viridis(option="plasma", discrete = T) + theme_classic() + ggtitle("Pct burnt area")
  ## total area burnt during all the period (boxplot)
  ggplot(dta.period, aes(x=scn, y=pct.abh, fill=scn)) + geom_boxplot(notch=F) + facet_grid(~clim) + 
    theme_classic() + scale_fill_viridis(option="plasma", discrete=T) +  ggtitle("% high-intensity burnt") 
  

  ggplot(dta.period, aes(x=scn, y=aburnt/10^6, fill=scn)) + geom_boxplot(notch=F) + facet_grid(~clim) + 
    theme_classic() + scale_fill_viridis(option="plasma", discrete=T) +  ggtitle("Burnt area") 
  
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