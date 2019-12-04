# setwd("c:/work/MEDMOD/SpatialModelsR/MEDFIRE")  #NúLaptop
setwd("d:/MEDMOD/SpatialModelsR/MEDFIRE")   #CTFC
library(raster)
library(tidyverse)


################## Plot maps of accumulated sdm per species ##################
rm(list=ls())
## Mask of the study area
load("inputlyrs/rdata/mask.rdata")
load("inputlyrs/rdata/land.rdata")
## climatic scn and Th
p <- 5; clim.scn <- "rcp45"
## Species
species <- c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster", "puncinata",
             "aalba", "qilex", "qsuber", "qfaginea", "qhumilis", "fsylvatica", "other")
## Build sdm per decade
load(paste0("inputlyrs/rdata/sdm_", p, "p_", clim.scn, "_10.rdata"))
sdm[,-1] <- ifelse(sdm[,-1]==0,NA,10); sdm10 <- sdm
load(paste0("inputlyrs/rdata/sdm_", p, "p_", clim.scn, "_20.rdata"))
sdm[,-1] <- ifelse(sdm[,-1]==0,NA,20); sdm20 <- sdm
load(paste0("inputlyrs/rdata/sdm_", p, "p_", clim.scn, "_30.rdata"))
sdm[,-1] <- ifelse(sdm[,-1]==0,NA,30); sdm30 <- sdm
load(paste0("inputlyrs/rdata/sdm_", p, "p_", clim.scn, "_40.rdata"))
sdm[,-1] <- ifelse(sdm[,-1]==0,NA,40); sdm40 <- sdm
load(paste0("inputlyrs/rdata/sdm_", p, "p_", clim.scn, "_50.rdata"))
sdm[,-1] <- ifelse(sdm[,-1]==0,NA,50); sdm50 <- sdm
load(paste0("inputlyrs/rdata/sdm_", p, "p_", clim.scn, "_60.rdata"))
sdm[,-1] <- ifelse(sdm[,-1]==0,NA,60); sdm60 <- sdm
load(paste0("inputlyrs/rdata/sdm_", p, "p_", clim.scn, "_70.rdata"))
sdm[,-1] <- ifelse(sdm[,-1]==0,NA,70); sdm70 <- sdm
load(paste0("inputlyrs/rdata/sdm_", p, "p_", clim.scn, "_80.rdata"))
sdm[,-1] <- ifelse(sdm[,-1]==0,NA,80); sdm80 <- sdm
load(paste0("inputlyrs/rdata/sdm_", p, "p_", clim.scn, "_90.rdata"))
sdm[,-1] <- ifelse(sdm[,-1]==0,NA,90); sdm90 <- sdm
sdm.accum.pmin <- pmin(sdm10,sdm20,sdm30,sdm40,sdm50,sdm60,sdm70,sdm80,sdm90, na.rm=T)
sdm.accum.pmax <- pmax(sdm10,sdm20,sdm30,sdm40,sdm50,sdm60,sdm70,sdm80,sdm90, na.rm=T)
rm(sdm)
## Plot accumulated sdm per species
for(spp in species){
  tiff(paste0("rscripts/outs/sdm.accum_", scn, "_", spp, ".tiff"), width = 1600, height=800)
  par(mfrow=c(1,2))
  SDM <- MASK; SDM[!is.na(SDM[])] <- sdm.accum.pmin[,which(spp == species)+1]
  plot(SDM, col=rainbow(9)[9:1], main=paste("Gain SDM", spp))
  SDM <- MASK; SDM[!is.na(SDM[])] <- sdm.accum.pmax[,which(spp == species)+1]
  plot(SDM, col=rainbow(9)[9:1], main=paste("Lost SDM", spp)); dev.off()
}


################## Compute continuous SDM ##################
rm(list=ls()); gc()
extCat <- extent(c(250000, 540000, 4480000, 4760000)) ## Default extent of raster maps of Catalonia  
coeff <- read.table("inputfiles/SDMmdl.txt", header=T)
for(scn in c("rcp45", "rcp85")){
  for(decade in seq(10,90,10)){
    MNAN <- raster(paste0("inputlyrs/asc/", scn, "/", decade, "/mnan.asc"))
    MXAN <- raster(paste0("inputlyrs/asc/", scn, "/", decade, "/mxan.asc"))
    PLAN <- raster(paste0("inputlyrs/asc/", scn, "/", decade, "/plan.asc"))
    RAD <- raster(paste0("inputlyrs/asc/", scn, "/", decade, "/Radan.asc"))
    for(i in 1:nrow(coeff)){  
      print(paste("Scenario", scn, "- Decade", decade, "- Spp", i))
      P <- coeff$c[i] + coeff$c_mnan[i]*MNAN + coeff$c2_mnan[i]*MNAN*MNAN +
            coeff$c_mxan[i]*MXAN + coeff$c2_mxan[i]*MXAN*MXAN + 
            coeff$c_plan[i]*PLAN + coeff$c2_plan[i]*PLAN*PLAN +
            coeff$c_rad[i]*RAD + coeff$c2_rad[i]*RAD*RAD
      Z <- 1/(1+exp(-P))
      Z <- disaggregate(Z, fact=c(10,10))
      Z <- extend(Z, extCat)
      if(decade==10 & i==1)
        sdm <- data.frame(spp=i, cell.id=1:ncell(Z), x10=Z[])
      if(decade==10 & i!=1)
        sdm <- rbind(sdm, data.frame(spp=i, cell.id=1:ncell(Z), x10=Z[]))
      sdm <- filter(sdm, !is.na(x10))
      if(decade!=10 & i==1){
        x <- Z[]; x <- x[!is.na(x)]
      }
      if(decade!=10 & i>1){
        aux <- Z[]; aux <- aux[!is.na(aux)]; x <- c(x,aux)
      }
    }
    ## Add that decade in the df (last colum) and name accordingly
    if(decade>10){
      sdm$x <- x
      names(sdm)[ncol(sdm)] <- paste0("x", decade)  
    }
    
  }
  ## Condensate sdm of other trees
  aux <- filter(sdm, spp>=13) %>% group_by(cell.id) %>% 
         summarize(spp=13, x10=max(x10), x20=max(x20), x30=max(x30), x40=max(x40), x50=max(x50),
                   x60=max(x60), x70=max(x70), x80=max(x80), x90=max(x90))
  sdm <- rbind(as.data.frame(filter(sdm, spp<13)), as.data.frame(aux))
  save(sdm, file=paste0("inputlyrs/SDMcont_", scn, "_100m.rdata"))
}


################### Plot SDM distribution for each scn and decade  ##################
################### with current SPP distribution ####################################
rm(list=ls()); gc()
species <- c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster", "puncinata",
             "aalba", "qilex", "qsuber", "qfaginea", "qhumilis", "fsylvatica", "other")
for(scn in c("rcp45", "rcp85")){
  load("inputlyrs/rdata/land.rdata")
  load(paste0("inputlyrs/SDMcont_", scn, "_100m.rdata"))
  names(sdm)[3:11] <- paste0("Decade", seq(10,90,10))
  land <- left_join(land, sdm, by = c("cell.id", "spp")) %>% 
          select(-cell.id, -biom, -age, -tsdist, -distype) %>%
          gather(key, value, -spp)
  for(i in 1:13){
    sdm.spp <- filter(land, spp==i) 
    aux <- filter(sdm.spp, key=="Decade10")
    q1 <- quantile(aux$value, p=0.01)
    q5 <- quantile(aux$value, p=0.05)
    q10 <- quantile(aux$value, p=0.1)
    th1 <- filter(aux, value<=q1) %>% group_by(spp) %>% summarise(x=mean(value))
    th5 <- filter(aux, value<=q5) %>% group_by(spp) %>% summarise(x=mean(value))
    th10 <- filter(aux, value<=q10) %>% group_by(spp) %>% summarise(x=mean(value))
    p <- ggplot(sdm.spp, aes(x=value)) + geom_density(color="black", fill="grey80") + theme_bw() + 
      geom_vline(aes(xintercept=th1$x), color="darkblue", linetype="dashed", size=1) +
      geom_vline(aes(xintercept=th5$x), color="red", linetype="dashed", size=1) +
      geom_vline(aes(xintercept=th10$x), color="violet", linetype="dashed", size=1) +
      facet_wrap(.~key)
    tiff(paste0("rscripts/outs/sdm.dist_", scn, "_", species[i], ".tiff"), width = 800, height=800)
    p
    dev.off()
  }
}




# ## SPP overlapped to SDM ... hard to see anything
# SPP <- MASK; SPP[!is.na(SPP[])] <- land$spp
# tiff("rscripts/outs/sdm.accum_phalepensis.tiff", width = 800, height=800)
# SDM <- MASK; SDM[!is.na(SDM[])] <- sdm.accum$sdm.phalepensis
# spp <- rasterToPoints(SPP, fun=function(x){x==1}, spatial = T)
# plot(SDM, col=rainbow(9)[9:1], main="Accumulated SDM P.halepensis")
# plot(spp, add=TRUE, col="grey30", pch=19, cex=0.05, alpha=0.8)
# dev.off()