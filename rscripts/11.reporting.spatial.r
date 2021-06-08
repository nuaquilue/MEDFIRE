play <- function(){
  rm(list=ls())  
  library(raster)
  library(fmsb)
  library(landscapemetrics)
  library(viridis)
  library(rasterVis)
  library(tidyverse)
  source("rscripts/11.reporting.spatial.r")

  ## MFRI
  list.scn <- c("WF", "LC_WF", "WF_FS", "LC_WF_FS", "CC_WF",  "CC_LC_WF",  "CC_WF_FS", "CC_LC_WF_FS")
  a <- plot.mfri(list.scn)
  a$clim <- c(rep("current",4*5), rep("rcp8.5",4*5))
  ggplot(a, aes(x=scn, y=mfri, fill=scn)) + geom_boxplot(notch=F) + facet_wrap(~clim) + 
    theme_classic() + scale_fill_viridis(option="plasma", discrete=T) +  ggtitle("MFRI") 
  group_by(a, scn) %>% summarise(mfri=mean(mfri))
  
  ## LANDSCAPE METRICS
  list.scn <- c("NULL", "LC", "WF", "LC_WF", "WF_FS", "LC_WF_FS",
                "CC", "CC_LC", "CC_WF",  "CC_LC_WF",  "CC_WF_FS","CC_LC_WF_FS")
  dta <- land.metrics(list.scn)
  save(dta, file="rscripts/outs/15.landmetrics.rdata")
  
  
}


plot.mfri <- function(list.scn){
  
  ## data.frame
  load("inputlyrs/rdata/mask.rdata")
  COMARCA <- raster("c:/work/MEDMOD/spatialmodelsr/MEDFIRE/inputlyrs/asc/County_100m_31N-ETRS89.asc")
  
  # id - area comarca
  comarca <- data.frame(id=COMARCA[]) %>% filter(!is.na(id)) %>% group_by(id) %>% summarise(area=length(id))
  # to save mfri
  id <- data.frame(id=COMARCA[])
  dta.cat <- data.frame(scn=NA, run=NA, mfri=NA)
  
  for(scn in list.scn){
    rm(dta.all)
    for(i in 1:5){
      TB <- raster(paste0("outputs/spatial/Scn_", scn, "_run0", i, "/lyr/TimesBurnt_r1.tif"))
      if(exists("dta.all")){
        aux <- data.frame(id, run=i, tb=TB[]) %>% filter(!is.na(id) & !is.na(tb)) %>% 
          group_by(id, run) %>% summarise(aburnt=sum(tb)) %>% left_join(comarca, by="id") %>% 
          mutate(fri=area*80/aburnt)
        dta.all <- rbind(dta.all, aux)
      }
      else
        dta.all <- data.frame(id, run=i, tb=TB[]) %>% filter(!is.na(id) & !is.na(tb)) %>% 
          group_by(id, run) %>% summarise(aburnt=sum(tb)) %>% left_join(comarca, by="id") %>% 
          mutate(fri=area*80/aburnt)
    }
    # class mfri
    dta <- group_by(dta.all, id) %>% summarise(mfri=mean(fri)) %>% 
           mutate(class=ifelse(mfri<=300, 1, ifelse(mfri<=600, 2, ifelse(mfri<=1000, 3,
        ifelse(mfri<=5000, 4, 5))))) 
    table(dta$class)
    
    ## mfri at CAT level
    n.scn <- ifelse(scn %in% c("WF", "CC_WF"),"3.WF", ifelse(scn %in% c("LC_WF", "CC_LC_WF"), "4.LC_WF",
              ifelse(scn %in% c("WF_FS", "CC_WF_FS"), "5.WF_FS", "6.LC_WF_FS")))
    dta.cat <- rbind(dta.cat, data.frame(scn=n.scn, group_by(dta.all, run) %>% 
      summarise(aburnt=sum(aburnt), area=sum(area)) %>% mutate(mfri=area*80/aburnt) %>% select(run,mfri)) )
      
    # ## transfer mfri to raster
    # map <- left_join(id, dta, by="id")
    # MAP <- COMARCA; MAP[] <- map$class
    # MAP@extent@xmin = MAP@extent@xmin / 1000
    # MAP@extent@xmax = MAP@extent@xmax / 1000
    # MAP@extent@ymin = MAP@extent@ymin / 1000
    # MAP@extent@ymax = MAP@extent@ymax / 1000
    # tiff(paste0("rscripts/outs/14.MFRI_", scn, ".tif"), width=500, heigh=500)
    # plot(MAP, col=viridis(5), legend=T)
    # text(x=450, y=4730, scn, cex=2)
    # dev.off()  
    
  }
  
  return(dta.cat[-1,])
}


land.metrics <- function(list.scn){
  
  list_lsm(level="landscape",  type="diversity metric")
  list_lsm(level="class",  type="aggregation metric")
  list_lsm(level="class", type="core area metric")
  
  # to save metrics
  dta <- data.frame(scn=NA, clim=NA, run=NA, year=NA, level=NA, class=NA, id=NA, metric=NA, value=NA)
  for(scn in list.scn){
    order.scn <- which(scn==list.scn)
    order.scn <- ifelse(order.scn<=6, order.scn, order.scn-6)
    for(run in 1:5){
      for(year in seq(10,90,10)){
        cat(scn, "- run", run, "- year", year, "\n")
        SPP <- raster(paste0("outputs/spatial/Scn_", scn, "_run0", run, "/lyr/Spp_r1t", year, ".tif"))  
        AGE <- raster(paste0("outputs/spatial/Scn_", scn, "_run0", run, "/lyr/Age_r1t", year, ".tif"))  # or biomass ?
        ## diversity metrics at the landscape level: 5 covers
        COVER <- SPP; cover <- COVER[]
        cover <- ifelse(cover<=7, 1, ifelse(cover<=13, 2, ifelse(cover<=15, 3, ifelse(cover<=17, 4, 5))))
        COVER[] <- cover  
        aux <- calculate_lsm(COVER, what=c("lsm_l_prd", "lsm_l_shdi", "lsm_l_shei", "lsm_l_sidi", "lsm_l_siei"))
        dta <- bind_rows(dta, data.frame(scn=paste0(order.scn, ".", ifelse(scn=="CC", "NULL", sub("CC_", "", scn))),
                              clim=ifelse(length(grep("CC", scn))==0, "current", "rcp8.5"), run, year, aux[,-1]))
        ## aggregation metrics at the class level: 2 covers, that is, forests and shrubs+grass
        COVER <- SPP; cover <- COVER[]
        cover <- ifelse(cover<=13, 1, ifelse(cover<=15, 2, NA))
        COVER[] <- cover  
        aux <- calculate_lsm(COVER, what=c("lsm_c_ai", "lsm_c_clumpy",  "lsm_c_division", 
                                            "lsm_c_mesh", "lsm_c_np", "lsm_c_pd", "lsm_c_split"))
        dta <- bind_rows(dta, data.frame(scn=paste0(order.scn, ".", ifelse(scn=="CC", "NULL", sub("CC_", "", scn))),
                              clim=ifelse(length(grep("CC", scn))==0, "current", "rcp8.5"), run, year, aux[,-1]))
        ## core metrics at the class level: young forest vs. mature/old forests
        COVER <- SPP; cover <- COVER[]; age <- AGE[]
        cover <- ifelse(cover<=13 & age<=15, 1, ifelse(cover<=13 & age>15, 2, NA))
                 # ifelse(cover>7 & cover<=13 & age<=15, 3, ifelse(cover<7 & cover>=13 & age >15, 4, NA))))
        COVER[] <- cover  
        aux <- calculate_lsm(COVER, what=c("lsm_c_cai_cv", "lsm_c_cai_mn",  "lsm_c_core_cv", "lsm_c_core_mn",
                                           "lsm_c_cpland", "lsm_c_dcore_cv", "lsm_c_dcore_mn", "lsm_c_ndca", "lsm_c_tca"))
        dta <- bind_rows(dta, data.frame(scn=paste0(order.scn, ".", ifelse(scn=="CC", "NULL", sub("CC_", "", scn))),
                              clim=ifelse(length(grep("CC", scn))==0, "current", "rcp8.5"), run, year, aux[,-1]))
      }
    }
  }
  return(dta[-1,])
}


plot.metrics <- function(){
  
  load("rscripts/outs/15.landmetrics.rdata")
  
  ## diversity metrics at the landscape level: 5 covers
  l <- "landscape"; m <- "prd"
  aux <- filter(dta, level==l, metric==m) %>% group_by(scn, clim, year) %>% summarise(value=mean(value))
  p1 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + 
    geom_line(aes(colour=scn, linetype=clim), size=1.5) +
    scale_color_viridis_d(option="plasma") + theme_classic() + ggtitle(paste(l,"-",m))
  l <- "landscape"; m <- "shdi"
  aux <- filter(dta, level==l, metric==m) %>% group_by(scn, clim, year) %>% summarise(value=mean(value))
  p2 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + 
    geom_line(aes(colour=scn, linetype=clim), size=1.5) +
    scale_color_viridis_d(option="plasma") + theme_classic() + ggtitle(paste(l,"-",m))
  l <- "landscape"; m <- "shei"
  aux <- filter(dta, level==l, metric==m) %>% group_by(scn, clim, year) %>% summarise(value=mean(value))
  p3 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + 
    geom_line(aes(colour=scn, linetype=clim), size=1.5) +
    scale_color_viridis_d(option="plasma") + theme_classic() + ggtitle(paste(l,"-",m))
  l <- "landscape"; m <- "sidi"
  aux <- filter(dta, level==l, metric==m) %>% group_by(scn, clim, year) %>% summarise(value=mean(value))
  p4 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + 
    geom_line(aes(colour=scn, linetype=clim), size=1.5) +
    scale_color_viridis_d(option="plasma") + theme_classic() + ggtitle(paste(l,"-",m))
  l <- "landscape"; m <- "siei"
  aux <- filter(dta, level==l, metric==m) %>% group_by(scn, clim, year) %>% summarise(value=mean(value))
  p5 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + 
    geom_line(aes(colour=scn, linetype=clim), size=1.5) +
    scale_color_viridis_d(option="plasma") + theme_classic() + ggtitle(paste(l,"-",m))
  
  gridExtra::grid.arrange(p1,p2,p3,p4,p5, nrow=2)
  p3
  
  
  ## aggregation metrics at the class level: 2 covers, that is, forests and shrubs+grass
  l <- "class"; m <- "ai"
  aux <- filter(dta, level==l, metric==m, !is.na(class)) %>% 
    mutate(class=ifelse(class==1, "forest", "shrub+grass")) %>% 
    group_by(scn, clim, year, class) %>% summarise(value=mean(value))
  p6 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + theme_classic() +
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_grid(~class) +
    scale_color_viridis_d(option="plasma") + theme(legend.position ="none")+ ggtitle(paste(l,"-",m))
  l <- "class"; m <- "clumpy"
  aux <- filter(dta, level==l, metric==m, !is.na(class)) %>% 
    mutate(class=ifelse(class==1, "forest", "shrub+grass")) %>% 
    group_by(scn, clim, year, class) %>% summarise(value=mean(value))
  p7 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + theme_classic() +
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_grid(~class) +
    scale_color_viridis_d(option="plasma") + theme(legend.position ="none")+ ggtitle(paste(l,"-",m))
  l <- "class"; m <- "division"
  aux <- filter(dta, level==l, metric==m, !is.na(class)) %>% 
    mutate(class=ifelse(class==1, "forest", "shrub+grass")) %>% 
    group_by(scn, clim, year, class) %>% summarise(value=mean(value))
  p8 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + theme_classic() +
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_grid(~class) +
    scale_color_viridis_d(option="plasma") + theme(legend.position ="none")+ ggtitle(paste(l,"-",m))
  l <- "class"; m <- "mesh"
  aux <- filter(dta, level==l, metric==m, !is.na(class)) %>% 
    mutate(class=ifelse(class==1, "forest", "shrub+grass")) %>% 
    group_by(scn, clim, year, class) %>% summarise(value=mean(value))
  p9 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + theme_classic() +
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_grid(~class) +
    scale_color_viridis_d(option="plasma") + theme(legend.position ="none")+ ggtitle(paste(l,"-",m))
  l <- "class"; m <- "np"
  aux <- filter(dta, level==l, metric==m, !is.na(class)) %>% 
    mutate(class=ifelse(class==1, "forest", "shrub+grass")) %>% 
    group_by(scn, clim, year, class) %>% summarise(value=mean(value))
  p10 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + theme_classic() +
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_grid(~class) +
    scale_color_viridis_d(option="plasma") + theme(legend.position ="none")+ ggtitle(paste(l,"-",m))
  l <- "class"; m <- "pd"
  aux <- filter(dta, level==l, metric==m, !is.na(class)) %>% 
    mutate(class=ifelse(class==1, "forest", "shrub+grass")) %>% 
    group_by(scn, clim, year, class) %>% summarise(value=mean(value))
  p11 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + theme_classic() +
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_grid(~class) +
    scale_color_viridis_d(option="plasma") + theme(legend.position ="none")+ ggtitle(paste(l,"-",m))
  l <- "class"; m <- "split"
  aux <- filter(dta, level==l, metric==m, !is.na(class)) %>% 
    mutate(class=ifelse(class==1, "forest", "shrub+grass")) %>% 
    group_by(scn, clim, year, class) %>% summarise(value=mean(value))
  p12 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + theme_classic() +
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_grid(~class) +
    scale_color_viridis_d(option="plasma") + theme(legend.position ="none")+ ggtitle(paste(l,"-",m))
  
  gridExtra::grid.arrange(p6,p7,p8,p9,p10,p11,p12, nrow=2)
  gridExtra::grid.arrange(p6,p7,p8,p11, nrow=2)
  
  
  ## core metrics at the class level: young forest vs. mature/old forests
  
  l <- "class"; m <- "cai_cv"
  aux <- filter(dta, level==l, metric==m, !is.na(class)) %>% 
    mutate(class=ifelse(class==1, "young forest", "mature/old forest")) %>% 
    group_by(scn, clim, year, class) %>% summarise(value=mean(value))
  p13 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + theme_classic() +
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_wrap(~class, scales="free_y") +
    scale_color_viridis_d(option="plasma") + theme(legend.position ="none")+ ggtitle(paste(l,"-",m))
  l <- "class"; m <- "cai_mn"
  aux <- filter(dta, level==l, metric==m, !is.na(class)) %>% 
    mutate(class=ifelse(class==1, "young forest", "mature/old forest")) %>% 
    group_by(scn, clim, year, class) %>% summarise(value=mean(value))
  p14 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + theme_classic() +
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_wrap(~class, scales="free_y") +
    scale_color_viridis_d(option="plasma") + theme(legend.position ="none")+ ggtitle(paste(l,"-",m))
  l <- "class"; m <- "core_cv"
  aux <- filter(dta, level==l, metric==m, !is.na(class)) %>% 
    mutate(class=ifelse(class==1, "young forest", "mature/old forest")) %>% 
    group_by(scn, clim, year, class) %>% summarise(value=mean(value))
  p15 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + theme_classic() +
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_wrap(~class, scales="free_y") +
    scale_color_viridis_d(option="plasma") + theme(legend.position ="none")+ ggtitle(paste(l,"-",m))
  l <- "class"; m <- "core_mn"
  aux <- filter(dta, level==l, metric==m, !is.na(class)) %>% 
    mutate(class=ifelse(class==1, "young forest", "mature/old forest")) %>% 
    group_by(scn, clim, year, class) %>% summarise(value=mean(value))
  p16 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + theme_classic() +
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_wrap(~class, scales="free_y") +
    scale_color_viridis_d(option="plasma") + theme(legend.position ="none")+ ggtitle(paste(l,"-",m))
  l <- "class"; m <- "cpland"
  aux <- filter(dta, level==l, metric==m, !is.na(class)) %>% 
    mutate(class=ifelse(class==1, "young forest", "mature/old forest")) %>% 
    group_by(scn, clim, year, class) %>% summarise(value=mean(value))
  p17 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + theme_classic() +
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_wrap(~class, scales="free_y") +
    scale_color_viridis_d(option="plasma") + theme(legend.position ="none")+ ggtitle(paste(l,"-",m))
  l <- "class"; m <- "dcore_cv"
  aux <- filter(dta, level==l, metric==m, !is.na(class)) %>% 
    mutate(class=ifelse(class==1, "young forest", "mature/old forest")) %>% 
    group_by(scn, clim, year, class) %>% summarise(value=mean(value))
  p18 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + theme_classic() +
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_wrap(~class, scales="free_y") +
    scale_color_viridis_d(option="plasma") + theme(legend.position ="none")+ ggtitle(paste(l,"-",m))
  l <- "class"; m <- "dcore_mn"
  aux <- filter(dta, level==l, metric==m, !is.na(class)) %>% 
    mutate(class=ifelse(class==1, "young forest", "mature/old forest")) %>% 
    group_by(scn, clim, year, class) %>% summarise(value=mean(value))
  p19 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + theme_classic() +
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_wrap(~class, scales="free_y") +
    scale_color_viridis_d(option="plasma") + theme(legend.position ="none")+ ggtitle(paste(l,"-",m))
  l <- "class"; m <- "ndca"
  aux <- filter(dta, level==l, metric==m, !is.na(class)) %>% 
    mutate(class=ifelse(class==1, "young forest", "mature/old forest")) %>% 
    group_by(scn, clim, year, class) %>% summarise(value=mean(value))
  p20 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + theme_classic() +
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_wrap(~class, scales="free_y") +
    scale_color_viridis_d(option="plasma") + theme(legend.position ="none")+ ggtitle(paste(l,"-",m))
  l <- "class"; m <- "tca"
  aux <- filter(dta, level==l, metric==m, !is.na(class)) %>% 
    mutate(class=ifelse(class==1, "young forest", "mature/old forest")) %>% 
    group_by(scn, clim, year, class) %>% summarise(value=mean(value))
  p21 <- ggplot(aux, aes(x=year, y=value, col=scn, linetype=clim)) + theme_classic() +
    geom_line(aes(colour=scn, linetype=clim), size=1.5) + facet_wrap(~class, scales="free_y") +
    scale_color_viridis_d(option="plasma") + theme(legend.position ="none")+ ggtitle(paste(l,"-",m))
  
  
  gridExtra::grid.arrange(p13,p14,p15,p16,p17,p18,p19,p20,p21, nrow=3) 
  gridExtra::grid.arrange(p17,p20,p16,p15, nrow=2) 
  
}

