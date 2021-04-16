play <- function(){
  rm(list=ls())  
  library(fmsb)
  library(tidyverse)
  library(viridis)
  source("rscripts/10.reporting.bytime.r")
  list.scn <- c("NULL", "LC", "WF", "LC_WF", "WF_FS", "LC_WF_FS",
                "CC", "CC_LC", "CC_WF",  "CC_LC_WF",  "CC_WF_FS","CC_LC_WF_FS")
  
  plot.agb(list.scn)
  
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


plot.carbon.burnt <- function(list.scn){
  
  options(warn=-1); rm(dta.all)
  eq.ba.carbon <- read.table("inputfiles/EqBasalAreaCarbon.txt", header=T)
  
  for(scn in list.scn){
    ## Burnt per species
    dta.burnt <- read.table(paste0("outputs/Scn_", scn, "/BurntSpp.txt"), header=T)
    if(nrow(dta.burnt)>0){
      dta.burnt$scn <- ifelse(scn=="CC", "NULL_noage", sub("CC_", "", scn))
      dta.burnt$year <- dta.burnt$year+2010
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
 
  ggplot(dta, aes(x=year, y=carbon/10^6, col=scn, linetype=clim)) + 
    geom_line(aes(colour=scn, linetype=clim), size=1.5) +
    scale_color_brewer(palette="Set1") + theme_classic() + ggtitle("Carbon burnt")
  
  ggplot(dta, aes(x=year, y=carbon/10^6, col=scn, linetype=clim))  +
    geom_line(aes(colour=scn, linetype=clim), size=1) + geom_smooth(method="loess", size=1.5) + #geom_smooth(method="lm", se=F) +
    scale_color_viridis(option="plasma", discrete = T) + theme_classic() + ggtitle("Carbon burnt")
  
  ## total period
  dta.period <- group_by(dta.all, scn, clim, run) %>% summarise(carbon=sum(carbon)/10^6)
  ggplot(dta.period, aes(x=scn, y=carbon, fill=scn)) + geom_boxplot(notch=T) + facet_wrap(~clim) + 
    theme_classic() + scale_fill_viridis(option="plasma", discrete=T) +  ggtitle("Carbon burnt") #geom_violin()
  
}


taxo.rich <- function(list.scn){
 
  options(warn=-1)
  rm(dta.all)
  for(scn in list.scn){
    ## Existences
    dta.land <- read.table(paste0("outputs/Scn_", scn, "/Land.txt"), header=T)
    dta.land$scn <- ifelse(scn=="CC", "NULL_noage", sub("CC_", "", scn))
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
  dta <- filter(dta.all, spp<=13) %>% select(-area) %>% 
    pivot_wider(names_from=spp, values_from=vol, names_prefix="spp")
  
  ## compute shannon index and average it
  dta$shannon <- exp(vegan::diversity(dta[,-(1:4)], "shannon"))
  dta.avg <- group_by(dta, scn, clim, year) %>% summarise(shannon=mean(shannon))
  
  ## plot evolution shannon                   
  ggplot(dta.avg, aes(x=year, y=shannon, col=scn, linetype=clim)) + 
    geom_line(aes(colour=scn, linetype=clim), size=1) + 
    scale_color_brewer(palette="Accent") + theme_classic() + ggtitle("Shannon")                    
  
  ### shannon per regions bioclimatiques seria interessant
  
}