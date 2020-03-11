########################################################################
## Build from scratch input layers of MEDFIRE from original data sources
## Cartographic projection: UTM31N - ETRS89
## Extent:
## xmin       : 250000 
## xmax       : 540000 
## ymin       : 4480000 
## ymax       : 4760000
######################################################################## 

rm(list=ls())
library(rgdal)
library(raster)
library(tidyverse)
 # setwd("c:/work/MEDMOD/SpatialModelsR/MEDFIRE")  #N?Laptop
setwd("d:/MEDMOD/SpatialModelsR/MEDFIRE")   #CTFC

load("inputlyrs/rdata/mask.rdata")

## Default extent of raster maps of Catalonia  
extCat <- extent(c(250000, 540000, 4480000, 4760000))


################################## LAND COVER FOREST SPECIES MAP ##################################
## Mask target values to phagocyte (100 - burnt, 200 - abandoned) with potential values
RECLASS <- raster("D:/MEDMOD/InputLayers_MEDFIRE_II/LCFspp/Reclass_MCSC_v4_Cat_100m_31N-ETRS89.asc")
MASK.BURNT <- RECLASS
MASK.BURNT[] <- ifelse(MASK.BURNT[]==18, NA,
                       ifelse(MASK.BURNT[]==19, NA,
                              ifelse(MASK.BURNT[]==20, NA,
                                     ifelse(MASK.BURNT[]==200, NA, MASK.BURNT[]))))
sum(table(MASK.BURNT[]))
writeRaster(MASK.BURNT, "D:/MEDMOD/InputLayers_MEDFIRE_II/LCFspp/ToPhagoBurnt0_100m_31N-ETRS89.asc",
            format="ascii", NAflag=255, overwrite=T)
MASK.ABAND <- RECLASS
MASK.ABAND[] <- ifelse(MASK.ABAND[]==14, NA, 
                       ifelse(MASK.ABAND[]==15, NA,
                              ifelse(MASK.ABAND[]==16, NA,
                                     ifelse(MASK.ABAND[]==17, NA, 
                                            ifelse(MASK.ABAND[]==18, NA, 
                                                   ifelse(MASK.ABAND[]==19, NA, 
                                                          ifelse(MASK.ABAND[]==20, NA, 
                                                                  ifelse(MASK.ABAND[]==100, NA, MASK.ABAND[]))))))))
table(MASK.ABAND[])
sum(table(MASK.ABAND[]))
writeRaster(MASK.ABAND, "D:/MEDMOD/InputLayers_MEDFIRE_II/LCFspp/ToPhagoAband0_100m_31N-ETRS89.asc",
            format="ascii", NAflag=255, overwrite=T)

## Phagocyte in Miramon, then combine
PHAGO.BURNT <- raster("D:/MEDMOD/InputLayers_MEDFIRE_II/LCFspp/ToPhagoBurnt1_100m_31N-ETRS89.asc")
PHAGO.ABAND <- raster("D:/MEDMOD/InputLayers_MEDFIRE_II/LCFspp/ToPhagoAband7_100m_31N-ETRS89.asc")
LCFM <- RECLASS
LCFM[LCFM[]==100] <- PHAGO.BURNT[LCFM[]==100]
LCFM[LCFM[]==200] <- PHAGO.ABAND[LCFM[]==200]
sum(table(LCFM[]))  # = 3.210.899

## Convert grasslands when dem<1500m to shrub
DEM <- raster("D:/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/DEM_100m_31N-ETRS89.asc")
LCFM[LCFM[]==15 & DEM[]<1500] <- 14

## Land-cover Forest species map
table(LCFM[])
writeRaster(LCFM, "D:/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/LCFspp_100m_31N-ETRS89.asc",
            format="ascii", NAflag=255, overwrite=T)


## IDEM AT 1KM
RECLASS <- raster("D:/MEDMOD/InputLayers_MEDFIRE_II/LCFspp/Reclass_MCSC_v4_Cat_1km_31N-ETRS89.asc")
MASK.BURNT <- RECLASS
MASK.BURNT[] <- ifelse(MASK.BURNT[]==18, NA,
                       ifelse(MASK.BURNT[]==19, NA,
                              ifelse(MASK.BURNT[]==20, NA,
                                     ifelse(MASK.BURNT[]==200, NA, MASK.BURNT[]))))
sum(table(MASK.BURNT[]))
writeRaster(MASK.BURNT, "D:/MEDMOD/InputLayers_MEDFIRE_II/LCFspp/ToPhagoBurnt0_1km_31N-ETRS89.asc",
            format="ascii", NAflag=255, overwrite=T)
PHAGO.BURNT <- raster("D:/MEDMOD/InputLayers_MEDFIRE_II/LCFspp/ToPhagoBurnt1_1km_31N-ETRS89.asc")
LCFM <- RECLASS
LCFM[LCFM[]==100] <- PHAGO.BURNT[LCFM[]==100]
DEM <- raster("D:/MEDMOD/InputLayers_MEDFIRE_II/ToOriol_1km/DEM_1km_31N-ETRS89.asc")
LCFM[LCFM[]==15 & DEM[]<1500] <- 14

## Land-cover Forest species map
table(LCFM[])
writeRaster(LCFM, "D:/MEDMOD/InputLayers_MEDFIRE_II/ToOriol_1km/LCFspp_1km_31N-ETRS89.asc",
            format="ascii", NAflag=255, overwrite=T)



################################## TIME SINCE DISTURBANCE ##################################
load("inputlyrs/rdata/mask.rdata")
TSF <- MASK
TSF[MASK[]==1] <- 400
for(y in 1986:2009){
  fire <- rgdal::readOGR(paste0("D:/MEDMOD/InputLayers_MEDFIRE_II/Fires/incendis", y, ".shp"))
  r <- rasterize(fire, MASK, 'GRID_CODE')
  r[!is.na(r[])] <- 2010-y
  TSF <- min(TSF, r, na.rm=T)
}
LCFM <- raster("D:/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/LCFspp_100m_31N-ETRS89.asc")
plot(TSF)
TSF[LCFM[]>17] <- NA
plot(TSF)
writeRaster(TSF, "D:/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/TSDisturb_100m_31N-ETRS89.asc",
            format="ascii", NAflag=-1, overwrite=T)
TSF.1k <- aggregate(TSF, fact=10, fun=median, na.rm=T)
plot(TSF.1k)
writeRaster(TSF, "D:/MEDMOD/Inputlayers_medfire_ii/tooriol_1km/TSDisturb_1km_31N-ETRS89.asc",
            format="ascii", NAflag=-1, overwrite=T)



################################ INITIAL SDM AND SQI ################################
######### SQI shurb to calculate initial biomass as function of TSF ##################
source("mdl/update.clim.r")
load("inputlyrs/rdata/mask.rdata")
load("inputlyrs/rdata/orography.rdata")
load("inputlyrs/rdata/land.rdata")
clim <- update.clim(MASK, land, orography, decade=0, "rcp85", 1)
SQI <- MASK
SQI[MASK[]==1] <- clim$sqi
LAND <- MASK
LAND[MASK[]==1] <- land$spp
table(LAND[], SQI[])
# rcp45
# 14 149473  71957 250095
# rcp85
# 14 149541  67502 254482
writeRaster(SQI, "inputlyrs/asc/SQI_100m_31N-ETRS89.asc", format="ascii", overwrite=T, NAflag=0)
# writeRaster(SDM, "inputlyrs/asc/SDM_100m_31N-ETRS89.asc", format="ascii", overwrite=T, NAflag=-1)



############################# BIOPHYSIC VARIABLES: BASAL AREA --> BIOMASS ################################
all.files <- list.files("D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/LidarData")
files.select <- all.files[seq(1,230,8)]
BA <- raster(paste0("D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/LidarData/", files.select[1]))
BAall <- extend(BA, extent(c(250000, 540000, 4480000, 4766800)))
for(sh in files.select[-1]){
  BAsheet <- raster(paste0("D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/LidarData/", sh))
  aux <- extend(BAsheet, extent(c(250000, 540000, 4480000, 4766800)))
  BAall <- merge(BAall, aux)
}
crs(BAall) <- CRS("+init=epsg:25831")
BAall[BAall[]<0] <- 0
BAall[] <- BAall[]*10
plot(BAall)
## Change resolution from 20m to 100m
BAcat <- crop(BAall, extCat)
BA100m <- resample(BAcat, MASK, fun="bilinear", expand=T)
## Now, match BA and LCFspp, phagocite in MiraMon
LCFM <- raster("D:/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/LCFspp_100m_31N-ETRS89.asc")
BA <- BA100m
BA[LCFM[]>13] <- NA
BA[LCFM[]<=13 & is.na(BA[])] <- 0
writeRaster(BA, "D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/ToPhagoBA0_100m_31N-ETRS89.asc")
## Overlap Biomass of Shurbs: kg (or tones) as function of Time Since Fire
BA <- raster("D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/ToPhagoBA10_100m_31N-ETRS89.asc")
# dta <- data.frame(lcfm=LCFM[], ba=BA[])
# a <- filter(dta, !is.na(lcfm) & lcfm<=13)
writeRaster(BA, "D:/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/Biomass_100m_31N-ETRS89.asc",
            format="ascii", NAflag=-1, overwrite=T)


############################# BIOPHYSIC VARIABLES: HEIGHT --> AGE ################################
all.files <- list.files("D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/LidarData")
files.select <- all.files[seq(7,235,8)]
HM <- raster(paste0("D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/LidarData/", files.select[1]))
HMall <- extend(HM, extent(c(250000, 540000, 4480000, 4766800)))
for(sh in files.select[-1]){
  HMsheet <- raster(paste0("D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/LidarData/", sh))
  aux <- extend(HMsheet, extent(c(250000, 540000, 4480000, 4766800)))
  HMall <- merge(HMall, aux)
}
crs(HMall) <- CRS("+init=epsg:25831")
HMall[HMall[]<0] <- 0
plot(HMall)
## Change resolution from 20m to 100m
HMcat <- crop(HMall, extCat)
HM100m <- resample(HMcat, MASK, fun="bilinear", expand=T)
## Now, match MeanHeight with LCFspp, phagocite in MiraMon
LCFM <- raster("D:/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/LCFspp_100m_31N-ETRS89.asc")
HM <- HM100m
HM[LCFM[]>13] <- NA
HM[LCFM[]<=13 & is.na(HM[])] <- 0
writeRaster(HM, "D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/ToPhagoHeight0_100m_31N-ETRS89.asc",
            format="ascii", NAflag=-1, overwrite=T)
## Height has to be transformed to age!!
HM <- raster("D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/ToPhagoHeight10_100m_31N-ETRS89.asc")
source("D:/MEDMOD/SpatialModelsR/medfire/rscripts/05.forest.age.r")
AGE <- forest.age(LCFM, HM)
## Age of Shrubs is TSF!
TSF <- raster("D:/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/TSDisturb_100m_31N-ETRS89.asc")
AGE[LCFM[]==14] <- TSF[LCFM[]==14] 
AGE[LCFM[]>14] <- NA
plot(AGE)
writeRaster(AGE, "D:/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/ForestAge_100m_31N-ETRS89.asc",
            format="ascii", NAflag=-1, overwrite=T)

####### Height --> To Age at 1 km 
LCFM.1k <- raster("D:/MEDMOD/InputLayers_MEDFIRE_II/ToOriol_1km/LCFspp_1km_31N-ETRS89.asc")
HM.1k <- raster("D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/ToPhagoHeight10_1km_31N-ETRS89.asc")
dta <- data.frame(lcf=LCFM.1k[], h=HM.1k[]) %>% filter(lcf<=13)
source("D:/MEDMOD/SpatialModelsR/medfire/rscripts/05.forest.age.r")
AGE.1k <- forest.age(LCFM.1k, HM.1k)
plot(AGE.1k)
writeRaster(AGE.1k, "D:/MEDMOD/InputLayers_MEDFIRE_II/ToOriol_1km/ForestAge_1km_31N-ETRS89.asc",
            format="ascii", NAflag=-1, overwrite=T)



###################################### DEM 15x15 ######################################
sheets <- list.files("c:/work/MEDMOD/InputLayers_MEDFIRE_II/DEM15x15", pattern="*.txt")
DEM <- raster(paste0("c:/work/MEDMOD/InputLayers_MEDFIRE_II/DEM15x15/", sheets[1]))
for(sh in sheets){
  DEM15 <- raster(paste0("c:/work/MEDMOD/InputLayers_MEDFIRE_II/DEM15x15/", sh))
  DEM <- merge(DEM, DEM15)
}
writeRaster(DEM, "c:/work/MEDMOD/InputLayers_MEDFIRE_II/DEM15x15/DEM.tif", 
            format="GTiff", overwrite=T, NAflag=-100)

## Clean DEM 15x15
DEM.15m <- raster("c:/work/MEDMOD/InputLayers_MEDFIRE_II/DEM15x15/DEM.tif" )
crs(DEM.15m) <- CRS("+init=epsg:25831")
DEM.15m[DEM.15m[] < -100] <- NA
plot(DEM.15m)

## Resample to a 100m and 1000m DEM
DEM.100m <- resample(DEM.15m, MASK, method="bilinear", 
                    filename="c:/work/MEDMOD/InputLayers_MEDFIRE_II/DEM15x15/DEM_100m_31N-ETRS89.asc",
                    format="ascii", overwrite=T, NAflag=-100)
crs(DEM.100m) <- CRS("+init=epsg:25831")
MASK.1km <- aggregate(MASK, fact=10, fun=mean)
DEM.1km <- resample(DEM.15m, MASK.1km, method="bilinear", 
                     filename="c:/work/MEDMOD/InputLayers_MEDFIRE_II/DEM15x15/DEM_1km_31N-ETRS89.asc",
                     format="ascii", overwrite=T, NAflag=-100)
crs(DEM.1km) <- CRS("+init=epsg:25831")


## SLOPE (º)
SLOPE <- terrain(DEM.100m, opt='slope', unit='degrees', neighbors=8)
writeRaster(SLOPE, "c:/work/MEDMOD/InputLayers_MEDFIRE_II/DEM15x15/SlopeDegree_100m_31N-ETRS89.asc",
            format="ascii", overwrite=T, NAflag=-100)
SLOPE <- terrain(DEM.1km, opt='slope', unit='degrees', neighbors=8)
writeRaster(SLOPE, "c:/work/MEDMOD/InputLayers_MEDFIRE_II/DEM15x15/SlopeDegree_1km_31N-ETRS89.asc",
            format="ascii", overwrite=T, NAflag=-100)



## ASPECT: from º to 4 categories
ASPECT <- terrain(DEM.100m, opt='aspect', unit='degrees', neighbors=8)
ASPECT[] <- ifelse(ASPECT[]<=45, 1,
                   ifelse(ASPECT[]<=115, 2,
                          ifelse(ASPECT[]<=225, 3,
                                 ifelse(ASPECT[]<=315, 4, 1))))
writeRaster(ASPECT, "c:/work/MEDMOD/InputLayers_MEDFIRE_II/DEM15x15/Aspect_100m_31N-ETRS89.asc",
            format="ascii", overwrite=T, NAflag=-100)
ASPECT <- terrain(DEM.1km, opt='aspect', unit='degrees', neighbors=8)
ASPECT[] <- ifelse(ASPECT[]<=45, 1,
                   ifelse(ASPECT[]<=115, 2,
                          ifelse(ASPECT[]<=225, 3,
                                 ifelse(ASPECT[]<=315, 4, 1))))
writeRaster(ASPECT, "c:/work/MEDMOD/InputLayers_MEDFIRE_II/DEM15x15/Aspect_1km_31N-ETRS89.asc",
            format="ascii", overwrite=T, NAflag=-100)


##############################################################################################################




## Test inputlyrs
ggplot(filter(land, spp<=13, age<=10), aes(x=as.factor(age), y=biom)) +
  geom_violin(width=1.2, color="black", fill="grey") +
  geom_boxplot(width=0.1) +  theme_bw() +   theme(legend.position="none") + 
  facet_wrap(.~as.factor(spp)) +
  stat_summary(fun.y=median, geom="point", size=2, color="black", shape="square") 

ggplot(filter(land, spp<=13, age>10 & age<=20), aes(x=as.factor(age), y=biom)) +
  geom_violin(width=1.2, color="black", fill="grey") +
  geom_boxplot(width=0.1) +  theme_bw() +   theme(legend.position="none") + 
  facet_wrap(.~as.factor(spp)) +
  stat_summary(fun.y=median, geom="point", size=2, color="black", shape="square") 



