 setwd("c:/work/MEDMOD/SpatialModelsR/MEDFIRE")  #NúLaptop
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
sdm.5p.rcp45 <- list()
sdm.5p.rcp85 <- list()
sdm.10p.rcp45 <- list()
sdm.10p.rcp85 <- list()
for(decade in seq(10,90,10)){
  load(paste0("inputlyrs/rdata/sdm_5p_rcp45_", decade,".rdata"))
  sdm[,-1] <- ifelse(sdm[,-1]==0, NA, decade)
  sdm.5p.rcp45[[decade/10]] <- sdm
  load(paste0("inputlyrs/rdata/sdm_5p_rcp85_", decade,".rdata"))
  sdm[,-1] <- ifelse(sdm[,-1]==0, NA, decade)
  sdm.5p.rcp85[[decade/10]] <- sdm
  load(paste0("inputlyrs/rdata/sdm_10p_rcp45_", decade,".rdata"))
  sdm[,-1] <- ifelse(sdm[,-1]==0, NA, decade)
  sdm.10p.rcp45[[decade/10]] <- sdm
  load(paste0("inputlyrs/rdata/sdm_10p_rcp85_", decade,".rdata"))
  sdm[,-1] <- ifelse(sdm[,-1]==0, NA, decade)
  sdm.10p.rcp85[[decade/10]] <- sdm
}
sdm.5p.rcp45.pmin <- pmin(sdm.5p.rcp45[[1]],sdm.5p.rcp45[[2]],sdm.5p.rcp45[[3]],sdm.5p.rcp45[[4]], sdm.5p.rcp45[[5]],
                          sdm.5p.rcp45[[6]],sdm.5p.rcp45[[7]],sdm.5p.rcp45[[8]],sdm.5p.rcp45[[9]], na.rm=T)
sdm.5p.rcp45.pmax <- pmax(sdm.5p.rcp45[[1]],sdm.5p.rcp45[[2]],sdm.5p.rcp45[[3]],sdm.5p.rcp45[[4]], sdm.5p.rcp45[[5]],
                          sdm.5p.rcp45[[6]],sdm.5p.rcp45[[7]],sdm.5p.rcp45[[8]],sdm.5p.rcp45[[9]], na.rm=T)
sdm.5p.rcp85.pmin <- pmin(sdm.5p.rcp85[[1]],sdm.5p.rcp85[[2]],sdm.5p.rcp85[[3]],sdm.5p.rcp85[[4]], sdm.5p.rcp85[[5]],
                          sdm.5p.rcp85[[6]],sdm.5p.rcp85[[7]],sdm.5p.rcp85[[8]],sdm.5p.rcp85[[9]], na.rm=T)
sdm.5p.rcp85.pmax <- pmax(sdm.5p.rcp85[[1]],sdm.5p.rcp85[[2]],sdm.5p.rcp85[[3]],sdm.5p.rcp85[[4]], sdm.5p.rcp85[[5]],
                          sdm.5p.rcp85[[6]],sdm.5p.rcp85[[7]],sdm.5p.rcp85[[8]],sdm.5p.rcp85[[9]], na.rm=T)
sdm.10p.rcp45.pmin <- pmin(sdm.10p.rcp45[[1]],sdm.10p.rcp45[[2]],sdm.10p.rcp45[[3]],sdm.10p.rcp45[[4]], sdm.10p.rcp45[[5]],
                          sdm.10p.rcp45[[6]],sdm.10p.rcp45[[7]],sdm.10p.rcp45[[8]],sdm.10p.rcp45[[9]], na.rm=T)
sdm.10p.rcp45.pmax <- pmax(sdm.10p.rcp45[[1]],sdm.10p.rcp45[[2]],sdm.10p.rcp45[[3]],sdm.10p.rcp45[[4]], sdm.10p.rcp45[[5]],
                          sdm.10p.rcp45[[6]],sdm.10p.rcp45[[7]],sdm.10p.rcp45[[8]],sdm.10p.rcp45[[9]], na.rm=T)
sdm.10p.rcp85.pmin <- pmin(sdm.10p.rcp85[[1]],sdm.10p.rcp85[[2]],sdm.10p.rcp85[[3]],sdm.10p.rcp85[[4]], sdm.10p.rcp85[[5]],
                          sdm.10p.rcp85[[6]],sdm.10p.rcp85[[7]],sdm.10p.rcp85[[8]],sdm.10p.rcp85[[9]], na.rm=T)
sdm.10p.rcp85.pmax <- pmax(sdm.10p.rcp85[[1]],sdm.10p.rcp85[[2]],sdm.10p.rcp85[[3]],sdm.10p.rcp85[[4]], sdm.10p.rcp85[[5]],
                          sdm.10p.rcp85[[6]],sdm.10p.rcp85[[7]],sdm.10p.rcp85[[8]],sdm.10p.rcp85[[9]], na.rm=T)
rm(sdm.5p.rcp45); rm(sdm.5p.rcp85); rm(sdm.10p.rcp45); rm(sdm.10p.rcp85)
## Plot accumulated sdm per species
for(spp in species){
  tiff(paste0("rscripts/outs/sdm.accum_", spp, ".tiff"), width = 800, height=800)
  par(mfrow=c(2,2))
  SDM <- MASK; SDM[!is.na(SDM[])] <- sdm.5p.rcp45.pmin[,which(spp == species)+1]
  plot(SDM, col=rainbow(9)[9:1], main=paste("Gain SDM", spp, "RCP45 th5%"))
  SDM <- MASK; SDM[!is.na(SDM[])] <- sdm.5p.rcp45.pmax[,which(spp == species)+1]
  plot(SDM, col=rainbow(9)[9:1], main=paste("Lost SDM", spp, "RCP45 th5%"))
  SDM <- MASK; SDM[!is.na(SDM[])] <- sdm.5p.rcp85.pmin[,which(spp == species)+1]
  plot(SDM, col=rainbow(9)[9:1], main=paste("Gain SDM", spp, "RCP85 th5%"))
  SDM <- MASK; SDM[!is.na(SDM[])] <- sdm.5p.rcp85.pmax[,which(spp == species)+1]
  plot(SDM, col=rainbow(9)[9:1], main=paste("Lost SDM", spp, "RCP85 th5%")); dev.off()
}



################## Compute continuous SDM ##################
rm(list=ls()); gc()
extCat <- extent(c(250000, 540000, 4480000, 4760000)) ## Default extent of raster maps of Catalonia  
coeff <- read.table("inputfiles/SDMmdl.txt", header=T)
for(clim.scn in c("rcp45", "rcp85")){
  for(decade in seq(10,90,10)){
    MNAN <- raster(paste0("inputlyrs/asc/", clim.scn, "/", decade, "/mnan.asc"))
    MXAN <- raster(paste0("inputlyrs/asc/", clim.scn, "/", decade, "/mxan.asc"))
    PLAN <- raster(paste0("inputlyrs/asc/", clim.scn, "/", decade, "/plan.asc"))
    RAD <- raster(paste0("inputlyrs/asc/", clim.scn, "/", decade, "/Radan.asc"))
    for(i in 1:nrow(coeff)){  
      print(paste("Scenario", clim.scn, "- Decade", decade, "- Spp", i))
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
  save(sdm, file=paste0("inputlyrs/SDMcont_", clim.scn, "_100m.rdata"))
}


######################################## Compute SDM THRESHOLDS ########################################
rm(list=ls()); gc()
extCat <- extent(c(250000, 540000, 4480000, 4760000)) ## Default extent of raster maps of Catalonia  
coeff <- read.table("inputfiles/SDMmdl.txt", header=T)
MNAN <- raster("inputlyrs/asc/actual/mnan.asc")
MXAN <- raster("inputlyrs/asc/actual/mxan.asc")
PLAN <- raster("inputlyrs/asc/actual/plan.asc")
RAD <- raster("inputlyrs/asc/actual/radan.asc")
load("inputlyrs/rdata/land.rdata")
load("inputlyrs/rdata/mask.rdata")
thresh <- vector()
for(i in 1:nrow(coeff)){  
  P <- coeff$c[i] + coeff$c_mnan[i]*MNAN + coeff$c2_mnan[i]*MNAN*MNAN +
    coeff$c_mxan[i]*MXAN + coeff$c2_mxan[i]*MXAN*MXAN + 
    coeff$c_plan[i]*PLAN + coeff$c2_plan[i]*PLAN*PLAN +
    coeff$c_rad[i]*RAD + coeff$c2_rad[i]*RAD*RAD
  Z <- 1/(1+exp(-P))
  Z <- disaggregate(Z, fact=c(10,10))
  Z <- extend(Z, extCat)
  LAND <- MASK
  LAND[!is.na(LAND[])] <- land$spp
  LAND[LAND[]!=i] <- NA
  SDM <- Z * (LAND==i)
  sdm <- SDM[]
  q1 <- quantile(sdm, na.rm=T, p=0.01)
  q5 <- quantile(sdm, na.rm=T, p=0.05)
  q10 <- quantile(sdm, na.rm=T, p=0.1)
  thresh <- rbind(thresh, data.frame(spp=i,  th1=mean(sdm[!is.na(sdm) & sdm<=q1]),
             th5=mean(sdm[!is.na(sdm) & sdm<=q5]), th10=mean(sdm[!is.na(sdm) & sdm<=q10]) ))
}
write.table(thresh, "rscripts/outs/SDMth.txt", quote=F, sep="\t", row.names = F)



######################################## Build IN-OUT SDMs ########################################
rm(list=ls()); gc()
species <- c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster", "puncinata",
             "aalba", "qilex", "qsuber", "qfaginea", "qhumilis", "fsylvatica", "other")
thresh <- read.table("rscripts/outs/SDMth.txt", header=T)
load("inputlyrs/rdata/mask.rdata")
for(clim.scn in c("rcp45", "rcp85")){
  load(paste0("inputlyrs/SDMcont_", clim.scn, "_100m.rdata"))
  sdm <- left_join(sdm, thresh, by="spp")
  for(th in 12:14){
    sdm.inout <- sdm[,1:11]; sdm.inout[,3:11] <- sdm.inout[,3:11] >= sdm[,th]
    p <- ifelse(th==12, 1, ifelse(th==13, 5, 10))
    for(decade in seq(10,90,10)){
      sdm.proj <- list()
      for(i in 1:15){
        print(paste("Scenario", clim.scn, "- Threshold", p, "- Decade", decade, "- Spp", i))
        SDM <- MASK
        cell <- data.frame(cell.id=1:ncell(SDM))      
        aux <- filter(sdm.inout, spp==i) %>% select(cell.id, paste0("x",decade))
        cell <- left_join(cell, aux, by="cell.id")
        names(cell)[2] <- "sdm"
        SDM[] <- cell$sdm
        sdm.proj[[i]] <- SDM
      }
      ## Condensate sdm of other trees
      sdm.proj[[13]] <- sdm.proj[[13]] + sdm.proj[[14]] + sdm.proj[[15]]
      sdm.proj[[13]] <- subs(sdm.proj[[13]], data.frame(from=c(3,2,1,0),to=c(1,1,1,0)))
      sdm.proj <- sdm.proj[1:13]
      save(sdm.proj, file=paste0("inputlyrs/asc/SDM_", p, "p_", clim.scn, "_", decade, "_100m.rdata"))
    }
  }
}



################### Plot SDM distribution for each scn and decade  ##################
################### with current SPP distribution ####################################
rm(list=ls()); gc()
species <- c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster", "puncinata",
             "aalba", "qilex", "qsuber", "qfaginea", "qhumilis", "fsylvatica", "other")
thresh <- read.table("rscripts/outs/SDMth.txt", header=T)
for(scn in c("rcp45", "rcp85")){
  load("inputlyrs/rdata/land.rdata")
  load(paste0("inputlyrs/SDMcont_", scn, "_100m.rdata"))
  names(sdm)[3:11] <- paste0("Decade", seq(10,90,10))
  land <- left_join(land, sdm, by = c("cell.id", "spp")) %>% 
    select(-cell.id, -biom, -age, -tsdist, -distype) %>%
    gather(key, value, -spp)
  for(i in 1:13){
    sdm.spp <- filter(land, spp==i) 
    th <- filter(thresh, spp==i)
    # plot the distribution of continous SDM and mark the 3 thresholds
    p <- ggplot(sdm.spp, aes(x=value)) + geom_density(color="black", fill="grey80") + theme_bw() +
      geom_vline(aes(xintercept=th$th1), color="darkblue", linetype="dashed", size=1) +
      geom_vline(aes(xintercept=th$th5), color="red", linetype="dashed", size=1) +
      geom_vline(aes(xintercept=th$th10), color="violet", linetype="dashed", size=1) +
      facet_wrap(.~key)
    tiff(paste0("rscripts/outs/sdm.dist_", scn, "_", species[i], ".tiff"), width=800, height=800)
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