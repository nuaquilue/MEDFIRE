rm(list=ls())
# setwd("c:/work/MEDMOD/SpatialModelsR/MEDFIRE")  #NúLaptop
setwd("d:/MEDMOD/SpatialModelsR/MEDFIRE")   #CTFC

library(raster)
library(tidyverse)

## Mask of the study area
load("inputlyrs/rdata/mask.rdata")
load("inputlyrs/rdata/land.rdata")

## climatic scn and Th
p <- 5
clim.scn <- "rcp45"

## Fo
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
sdm.accum <- pmin(sdm10,sdm20,sdm30,sdm40,sdm50,sdm60,sdm70,sdm80,sdm90, na.rm=T)

## Plot accumulated sdm per species
# 01
load("inputlyrs/rdata/mask.rdata")
mask=MASK[]; mask[!is.na(mask)] <- sdm.accum$sdm.phalepensis; MASK[] <- mask; 
tiff("rscripts/outs/sdm.accum_phalepensis.tiff", width = 800, height=800)
plot(MASK, col=rainbow(9)[9:1], main="Accumulated SDM P.halepensis"); dev.off()
# 02
load("inputlyrs/rdata/mask.rdata")
mask=MASK[]; mask[!is.na(mask)] <- sdm.accum$sdm.pnigra; MASK[] <- mask; 
tiff("rscripts/outs/sdm.accum_pnigra.tiff", width = 800, height=800)
plot(MASK, col=rainbow(9)[9:1], main="Accumulated SDM P.nigra"); dev.off()
# 03
load("inputlyrs/rdata/mask.rdata")
mask=MASK[]; mask[!is.na(mask)] <- sdm.accum$sdm.ppinea; MASK[] <- mask; 
tiff("rscripts/outs/sdm.accum_ppinea.tiff", width = 800, height=800)
plot(MASK, col=rainbow(9)[9:1], main="Accumulated SDM P.pinea"); dev.off()
# 04
load("inputlyrs/rdata/mask.rdata")
mask=MASK[]; mask[!is.na(mask)] <- sdm.accum$sdm.psylvestris; MASK[] <- mask; 
tiff("rscripts/outs/sdm.accum_psylvestris.tiff", width = 800, height=800)
plot(MASK, col=rainbow(9)[9:1], main="Accumulated SDM P.sylvestris"); dev.off()
# 05
load("inputlyrs/rdata/mask.rdata")
mask=MASK[]; mask[!is.na(mask)] <- sdm.accum$sdm.ppinaster; MASK[] <- mask; 
tiff("rscripts/outs/sdm.accum_ppinaster.tiff", width = 800, height=800)
plot(MASK, col=rainbow(9)[9:1], main="Accumulated SDM P.pinaster"); dev.off
# 06
load("inputlyrs/rdata/mask.rdata")
mask=MASK[]; mask[!is.na(mask)] <- sdm.accum$sdm.puncinata; MASK[] <- mask
tiff("rscripts/outs/sdm.accum_puncinata.tiff", width = 800, height=800)
plot(MASK, col=rainbow(9)[9:1], main="Accumulated SDM P.uncinata"); dev.off()
# 07
load("inputlyrs/rdata/mask.rdata")
mask=MASK[]; mask[!is.na(mask)] <- sdm.accum$sdm.aalba; MASK[] <- mask; 
tiff("rscripts/outs/sdm.accum_aalba.tiff", width = 800, height=800)
plot(MASK, col=rainbow(9)[9:1], main="Accumulated SDM A.alba"); dev.off()
# 08
load("inputlyrs/rdata/mask.rdata")
mask=MASK[]; mask[!is.na(mask)] <- sdm.accum$sdm.qilex; MASK[] <- mask; 
tiff("rscripts/outs/sdm.accum_qilex.tiff", width = 800, height=800)
plot(MASK, col=rainbow(9)[9:1], main="Accumulated SDM Q.ilex"); dev.off()
# 09
load("inputlyrs/rdata/mask.rdata")
mask=MASK[]; mask[!is.na(mask)] <- sdm.accum$sdm.qsuber; MASK[] <- mask; 
tiff("rscripts/outs/sdm.accum_qsuber.tiff", width = 800, height=800)
plot(MASK, col=rainbow(9)[9:1], main="Accumulated SDM Q.suber"); dev.off()
# 10
load("inputlyrs/rdata/mask.rdata")
mask=MASK[]; mask[!is.na(mask)] <- sdm.accum$sdm.qfaginea; MASK[] <- mask; 
tiff("rscripts/outs/sdm.accum_qfaginea.tiff", width = 800, height=800)
plot(MASK, col=rainbow(9)[9:1], main="Accumulated SDM Q.faginea"); dev.off()
# 11
load("inputlyrs/rdata/mask.rdata")
mask=MASK[]; mask[!is.na(mask)] <- sdm.accum$sdm.qhumilis; MASK[] <- mask; 
tiff("rscripts/outs/sdm.accum_qhumilis.tiff", width = 800, height=800)
plot(MASK, col=rainbow(9)[9:1], main="Accumulated SDM Q.humilis"); dev.off()
# 12
load("inputlyrs/rdata/mask.rdata")
mask=MASK[]; mask[!is.na(mask)] <- sdm.accum$sdm.fsylvatica; MASK[] <- mask; 
tiff("rscripts/outs/sdm.accum_fsylvatica.tiff", width = 800, height=800)
plot(MASK, col=rainbow(9)[9:1], main="Accumulated SDM F.sylvatica"); dev.off()
# 13
load("inputlyrs/rdata/mask.rdata")
mask=MASK[]; mask[!is.na(mask)] <- sdm.accum$sdm.other; MASK[] <- mask; 
tiff("rscripts/outs/sdm.accum_other.tiff", width = 800, height=800)
plot(MASK, col=rainbow(9)[9:1], main="Accumulated SDM Other"); dev.off()