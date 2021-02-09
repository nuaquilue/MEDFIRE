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
setwd("c:/work/MEDMOD/SpatialModelsR/MEDFIRE")  #NúLaptop
# setwd("d:/MEDMOD/SpatialModelsR/MEDFIRE")   #CTFC

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
dta <- data.frame(m=MASK[], lcf=LCFM[]) %>% filter(!is.na(m))
any(is.na(dta$lcf))
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
writeRaster(TSF.1k, "D:/MEDMOD/Inputlayers_medfire_ii/tooriol_1km/TSDisturb_1km_31N-ETRS89.asc",
            format="ascii", NAflag=-1, overwrite=T)




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
LCFM <- raster("c:/work/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/LCFspp_100m_31N-ETRS89.asc")
BA <- BA100m
BA[LCFM[]>13] <- NA
BA[LCFM[]<=13 & is.na(BA[])] <- 0
writeRaster(BA, "D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/ToPhagoBA0_100m_31N-ETRS89.asc")

## Overlap Biomass of Shurbs: kg (or tones) as function of Time Since Fire
LCFM <- raster("c:/work/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/LCFspp_100m_31N-ETRS89.asc")
TSF <- raster("c:/work/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/TSDisturb_100m_31N-ETRS89.asc")
BA <- raster("c:/work/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/ToPhagoBA10_100m_31N-ETRS89.asc")
load("inputlyrs/rdata/land.rdata")
load("inputlyrs/rdata/orography.rdata")
source("mdl/update.clim.r")
clim <- hist.clim(land, orography, "SMHI-RCA4_MOHC-HadGEM2-ES")
w <- read.table("C:/WORK/MEDMOD/SpatialModelsR/MEDFIRE/inputfiles/InitialBiomassShrub.txt", header=T)
# Shrub Biomassa: W=exp(a0+a1*ln(hm*x)+a2*ln(fcc*x)) where x=TSF and W=Biomassa (Tones/hectarea)
dta.shrub <- data.frame(cell.id=1:ncell(LCFM), lcfm=LCFM[], tsf=TSF[]) %>% filter(!is.na(lcfm)) %>%
             filter(lcfm==14) %>%  left_join(select(clim, cell.id, sqi), by="cell.id") %>% 
             left_join(w, by="sqi") %>% 
             mutate(ba=exp(a0+a1*log(hm*pmin(tsf, age_max))+a2*log(fcc*pmin(tsf,age_max))))
summary(dta.shrub$ba[dta.shrub$lcfm==14])
BA[LCFM[]==14] <- dta.shrub$ba
# final test
dta  <- data.frame(cell.id=1:ncell(LCFM), lcfm=LCFM[], ba=BA[]) %>% filter(!is.na(lcfm)) %>%
        filter(lcfm<=14)
any(is.na(dta$ba))
# only 1 phalepensis cell
dta$ba[is.na(dta$ba)] <- mean(dta$ba[dta$lcfm==1], na.rm=T)
BA[LCFM[]<=14] <- dta$ba
writeRaster(BA, "c:/work/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/Biomass_100m_31N-ETRS89.asc",
            format="ascii", NAflag=-1, overwrite=T)  ## Biomass forest is m2/ha * 10 and Biomass shrubs is tones/ha

load("inputlyrs/rdata/land.rdata")
land$biom[land$spp==14] <- dta.shrub$ba
land[land$spp<=14 & is.na(land$ba), ]
save(land, file="c:/work/MEDMOD/SpatialModelsR/medfire/inputlyrs/rdata/land.rdata")

# final check
dta <- data.frame(cell.id=1:ncell(LCFM), lcf=LCFM[], ba=BA[]) %>% filter(lcf<=13)
any(is.na(dta$ba))
dta[which(is.na(dta$ba)),]
dta$ba[which(is.na(dta$ba))] <- mean(dta$ba[dta$lcf==1], na.rm=T)


####### Biomass, from 100m to 1 km 
BA <- raster("c:/work/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/Biomass_100m_31N-ETRS89.asc")
BA.1K <- aggregate(BA, fact=10, fun=mean)
BA.1K
writeRaster(BA.1K, "c:/work/MEDMOD/InputLayers_MEDFIRE_II/ToOriol_1km/Biomass_1km_31N-ETRS89.asc",
            format="ascii", NAflag=-1, overwrite=T)


############################# BIOPHYSIC VARIABLES: HEIGHT --> AGE ################################
source("rscripts/05.forest.age.r")
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
LCFM <- raster("c:/work/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/LCFspp_100m_31N-ETRS89.asc")
HM <- HM100m
HM[LCFM[]>13] <- NA
HM[LCFM[]<=13 & is.na(HM[])] <- 0
writeRaster(HM, "c:/work/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/ToPhagoHeight0_100m_31N-ETRS89.asc",
            format="ascii", NAflag=-1, overwrite=T)
## Height has to be transformed to age!!
HM <- raster("c:/work/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/ToPhagoHeight10_100m_31N-ETRS89.asc")
AGE <- forest.age(LCFM, HM)
## Age of Shrubs is TSF!
TSF <- raster("c:/work/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/TSDisturb_100m_31N-ETRS89.asc")
AGE[LCFM[]==14] <- TSF[LCFM[]==14] 
AGE[LCFM[]>14] <- NA
# check
dta <- data.frame(lcf=LCFM[], age=AGE[])
filter(dta, lcf<=14, is.na(age))
writeRaster(AGE, "c:/work/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/ForestAge_100m_31N-ETRS89.asc",
            format="ascii", NAflag=-1, overwrite=T)

####### Height --> To Age at 1 km 
LCFM.1k <- raster("c:/work/MEDMOD/InputLayers_MEDFIRE_II/ToOriol_1km/LCFspp_1km_31N-ETRS89.asc")
HM.1k <- raster("c:/work/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/ToPhagoHeight10_1km_31N-ETRS89.asc")
dta <- data.frame(lcf=LCFM.1k[], h=HM.1k[]) %>% filter(lcf<=13)
AGE.1k <- forest.age(LCFM.1k, HM.1k)
plot(AGE.1k)
writeRaster(AGE.1k, "c:/work/MEDMOD/InputLayers_MEDFIRE_II/ToOriol_1km/ForestAge_1km_31N-ETRS89.asc",
            format="ascii", NAflag=-1, overwrite=T)



###################################### DEM, SLOPE, ASPECT ######################################
rm(list=ls())
library(RANN)
library(sp)
library(raster)
library(tidyverse)
load("inputlyrs/rdata/mask.rdata")

############### DEM 15 x 15 --> DEM 100m and DEM 1km
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
summary(DEM.15m[]); plot(DEM.15m)

## Resample to a 100m DEM
DEM.100m <- resample(DEM.15m, MASK, method="bilinear", 
                    filename="c:/work/MEDMOD/InputLayers_MEDFIRE_II/DEM15x15/DEM_100m_31N-ETRS89.asc",
                    format="ascii", overwrite=T, NAflag=-100)
## Are there NAs in the DEM within CAT?
DEM.100m <- raster("c:/work/MEDMOD/InputLayers_MEDFIRE_II/DEM15x15/DEM_100m_31N-ETRS89.asc")
dta <- data.frame(cell.id=1:ncell(DEM.100m), m=MASK[], coordinates(DEM.100m), z=DEM.100m[]) %>%
        filter(!is.na(m))
na.var <- filter(dta, is.na(z))
for(id in na.var$cell.id){
  neighs <- nn2(select(dta, x, y), filter(na.var, cell.id==id)%>%select(x,y), 
                searchtype="priority", k=9)
  phago <- mean(dta$z[neighs$nn.idx], na.rm=T)
  if(!is.na(phago))
    na.var$z[na.var$cell.id==id] <- phago
}
na.var2 <- filter(na.var, is.na(z))
for(id in na.var2$cell.id){
  neighs <- nn2(select(dta, x, y), filter(na.var, cell.id==id)%>%select(x,y), 
                searchtype="priority", k=25)
  phago <- mean(dta$z[neighs$nn.idx], na.rm=T)
  if(!is.na(phago))
    na.var$z[na.var$cell.id==id] <- phago
}
na.var3 <- filter(na.var, is.na(z))
dta$z[is.na(dta$z)] <- na.var$z
DEM <- MASK
DEM[!is.na(MASK[])] <- dta$z
writeRaster(DEM, "c:/work/MEDMOD/spatialmodelsr/MEDFIRE/inputlyrs/asc/DEM_100m_31N-ETRS89.asc", 
            format="ascii", overwrite=T, NAflag=-100)


## Resample to a 1000m DEM
MASK.1km <- aggregate(MASK, fact=10, fun=mean)
DEM.1km <- resample(DEM, MASK.1km, method="bilinear", 
                    filename="c:/work/MEDMOD/InputLayers_MEDFIRE_II/DEM15x15/DEM_1km_31N-ETRS89.asc",
                    format="ascii", overwrite=T, NAflag=-100)
## Are there NAs in the DEM within CAT?
DEM.1km <- raster("c:/work/MEDMOD/InputLayers_MEDFIRE_II/DEM15x15/DEM_1km_31N-ETRS89.asc")
dta <- data.frame(cell.id=1:ncell(DEM.1km), m=MASK.1km[], coordinates(DEM.1km), z=DEM.1km[]) %>%
       filter(!is.na(m))
na.var <- filter(dta, is.na(z))
for(id in na.var$cell.id){
  neighs <- nn2(select(dta, x, y), filter(na.var, cell.id==id)%>% select(x,y), 
                searchtype="priority", k=9)
  phago <- mean(dta$z[neighs$nn.idx], na.rm=T)
  if(!is.na(phago))
    na.var$z[na.var$cell.id==id] <- phago
}
na.var2 <- filter(na.var, is.na(z))
dta$z[is.na(dta$z)] <- na.var$z
DEM <- MASK.1km
DEM[!is.na(MASK.1km[])] <- dta$z
writeRaster(DEM, "c:/work/MEDMOD/inputlayers_MEDFIRE_II/ToOriol_1km/DEM_1km_31N-ETRS89.asc", 
            format="ascii", overwrite=T, NAflag=-100)



################################## SLOPE (º) at 100m
SLOPE.100m <- raster("c:/work/MEDMOD/inputlayers_MEDFIRE_II/Orography/SlopeDegree_100m_31N-ETRS89.asc")
## Are there NAs in the SLOPE within CAT?
dta <- data.frame(cell.id=1:ncell(DEM.100m), m=MASK[], coordinates(DEM.100m), z=SLOPE.100m[]) %>%
        filter(!is.na(m))
na.var <- filter(dta, is.na(z))
for(id in na.var$cell.id){
  neighs <- nn2(select(dta, x, y), filter(na.var, cell.id==id)%>% select(x,y), 
                searchtype="priority", k=9)
  phago <- mean(dta$z[neighs$nn.idx], na.rm=T)
  if(!is.na(phago))
    na.var$z[na.var$cell.id==id] <- phago
  print(which(na.var$cell.id==id))
}
na.var2 <- filter(na.var, is.na(z))
for(id in na.var2$cell.id){
  neighs <- nn2(select(dta, x, y), filter(na.var, cell.id==id)%>% select(x,y), 
                searchtype="priority", k=25)
  phago <- mean(dta$z[neighs$nn.idx], na.rm=T)
  if(!is.na(phago))
    na.var$z[na.var$cell.id==id] <- phago
  print(which(na.var$cell.id==id))
}
na.var3 <- filter(na.var, is.na(z))
dta$z[is.na(dta$z)] <- na.var$z
SLOPE <- MASK
SLOPE[!is.na(MASK[])] <- dta$z
writeRaster(SLOPE, "c:/work/MEDMOD/spatialmodelsr/MEDFIRE/inputlyrs/asc/SlopeDegree_100m_31N-ETRS89.asc", 
            format="ascii", overwrite=T, NAflag=-100)



################################## SLOPE (º) at 1km
DEM.1km <- raster("c:/work/MEDMOD/inputlayers_MEDFIRE_II/ToOriol_1km/DEM_1km_31N-ETRS89.asc")
crs(DEM.1km) <- CRS("+init=epsg:25831")
SLOPE <- terrain(DEM.1km, opt='slope', unit='degrees', neighbors=8)
## Are there NAs in the SLOPE within CAT?
dta <- data.frame(cell.id=1:ncell(DEM.1km), m=MASK.1km[], coordinates(DEM.1km), z=SLOPE[]) %>%
       filter(!is.na(m))
na.var <- filter(dta, is.na(z))
for(id in na.var$cell.id){
  neighs <- nn2(select(dta, x, y), filter(na.var, cell.id==id)%>% select(x,y), 
                searchtype="priority", k=9)
  phago <- mean(dta$z[neighs$nn.idx], na.rm=T)
  if(!is.na(phago))
    na.var$z[na.var$cell.id==id] <- phago
}
na.var2 <- filter(na.var, is.na(z))
for(id in na.var2$cell.id){
  neighs <- nn2(select(dta, x, y), filter(na.var, cell.id==id)%>%select(x,y), 
                searchtype="priority", k=25)
  phago <- mean(dta$z[neighs$nn.idx], na.rm=T)
  if(!is.na(phago))
    na.var$z[na.var$cell.id==id] <- phago
}
na.var3 <- filter(na.var, is.na(z))
for(id in na.var3$cell.id){
  neighs <- nn2(select(dta, x, y), filter(na.var, cell.id==id)%>%select(x,y), 
                searchtype="priority", k=49)
  phago <- mean(dta$z[neighs$nn.idx], na.rm=T)
  if(!is.na(phago))
    na.var$z[na.var$cell.id==id] <- phago
}
na.var4 <- filter(na.var, is.na(z))
dta$z[is.na(dta$z)] <- na.var$z
SLOPE <- MASK.1km
SLOPE[!is.na(MASK.1km[])] <- dta$z
writeRaster(SLOPE, "c:/work/MEDMOD/inputlayers_MEDFIRE_II/ToOriol_1km/SlopeDegree_1km_31N-ETRS89.asc",
            format="ascii", overwrite=T, NAflag=-100)



#################################### ASPECT: 4 categories at 100m
ASPECT.100m <- raster("c:/work/MEDMOD/inputlayers_MEDFIRE_II/Orography/Aspect_100m_31N-ETRS89.asc")
## Are there NAs in the ASPECT within CAT?
dta <- data.frame(cell.id=1:ncell(ASPECT.100m), m=MASK[], coordinates(ASPECT.100m), z=ASPECT.100m[]) %>%
      filter(!is.na(m))
na.var <- filter(dta, is.na(z))
for(id in na.var$cell.id){
  neighs <- nn2(select(dta, x, y), filter(na.var, cell.id==id)%>% select(x,y), 
                searchtype="priority", k=9)
  phago <- median(dta$z[neighs$nn.idx], na.rm=T)
  if(!is.na(phago))
    na.var$z[na.var$cell.id==id] <- phago
  print(which(na.var$cell.id==id))
}
na.var2 <- filter(na.var, is.na(z))
for(id in na.var2$cell.id){
  neighs <- nn2(select(dta, x, y), filter(na.var, cell.id==id)%>%select(x,y), 
                searchtype="priority", k=25)
  phago <- median(dta$z[neighs$nn.idx], na.rm=T)
  if(!is.na(phago))
    na.var$z[na.var$cell.id==id] <- phago
}
na.var3 <- filter(na.var, is.na(z))
dta$z[is.na(dta$z)] <- na.var$z
ASPECT <- MASK
ASPECT[!is.na(MASK[])] <- dta$z
writeRaster(ASPECT, "c:/work/MEDMOD/spatialmodelsr/MEDFIRE/inputlyrs/asc/Aspect_100m_31N-ETRS89.asc", 
            format="ascii", overwrite=T, NAflag=-100)



#################################### ASPECT: from º to 4 categories at 1km
DEM.1km <- raster("c:/work/MEDMOD/inputlayers_MEDFIRE_II/ToOriol_1km/DEM_1km_31N-ETRS89.asc")
crs(DEM.1km) <- CRS("+init=epsg:25831")
ASPECT <- terrain(DEM.1km, opt='aspect', unit='degrees', neighbors=8)
## Are there NAs in the SLOPE within CAT?
dta <- data.frame(cell.id=1:ncell(DEM.1km), m=MASK.1km[], coordinates(DEM.1km), z=ASPECT[]) %>%
        filter(!is.na(m))
na.var <- filter(dta, is.na(z))
for(id in na.var$cell.id){
  neighs <- nn2(select(dta, x, y), filter(na.var, cell.id==id)%>% select(x,y), 
                searchtype="priority", k=9)
  phago <- mean(dta$z[neighs$nn.idx], na.rm=T)
  if(!is.na(phago))
    na.var$z[na.var$cell.id==id] <- phago
}
na.var2 <- filter(na.var, is.na(z))
for(id in na.var2$cell.id){
  neighs <- nn2(select(dta, x, y), filter(na.var, cell.id==id)%>%select(x,y), 
                searchtype="priority", k=25)
  phago <- mean(dta$z[neighs$nn.idx], na.rm=T)
  if(!is.na(phago))
    na.var$z[na.var$cell.id==id] <- phago
}
na.var3 <- filter(na.var, is.na(z))
for(id in na.var3$cell.id){
  neighs <- nn2(select(dta, x, y), filter(na.var, cell.id==id)%>%select(x,y), 
                searchtype="priority", k=49)
  phago <- mean(dta$z[neighs$nn.idx], na.rm=T)
  if(!is.na(phago))
    na.var$z[na.var$cell.id==id] <- phago
}
na.var4 <- filter(na.var, is.na(z))
dta$z[is.na(dta$z)] <- na.var$z
dta$category <- ifelse(dta$z<=45, 1,
                   ifelse(dta$z<=115, 2,
                          ifelse(dta$z<=225, 3,
                                 ifelse(dta$z<=315, 4, 1))))
ASPECT <- MASK.1km
ASPECT[!is.na(MASK.1km[])] <- dta$category
writeRaster(ASPECT, "c:/work/MEDMOD/inputlayers_MEDFIRE_II/ToOriol_1km/Aspect_1km_31N-ETRS89.asc",
            format="ascii", overwrite=T, NAflag=-1)





###################################### FIRES 2010 to 2019 (to be burnt in the spin-in) ######################################
load("inputlyrs/rdata/mask.rdata")
aux <- data.frame(cell.id=1:ncell(MASK))
wildfires <- data.frame(cell.id=aux[!is.na(MASK[]),])
for(y in 0:9){
  PERIM <- rgdal::readOGR(paste0("c:/WORK/MEDMOD/FIRES/IncendisSHP_1986-2019_31N.ETRS89/incendis", 2010+y, ".shp"))
  FIRE <- rasterize(PERIM, MASK, 'GRID_CODE')
  FIRE[!is.na(FIRE[])] <- 1
  fire <- FIRE[]
  wildfires <- cbind(wildfires, fire[!is.na(MASK[])])
  names(wildfires)[y+2] <- paste0("y", 10+y)
}
summary(wildfires)
apply(wildfires, 2, sum, na.rm=T)
save(wildfires, file="inputlyrs/rdata/wildfires.rdata")

## Land-cover types burnt in 2010-2019 wildfires
load("inputlyrs/rdata/land.rdata")
load("inputlyrs/rdata/wildfires.rdata")
lct.burnt <- data.frame(lct=NA, n=NA, y=NA)
for(y in 0:8){
  aux <- as.data.frame(land$spp[!is.na(wildfires[,y+2])])
  names(aux) <- "lct"
  aux <- group_by(aux,lct) %>% summarise(n=length(lct)) %>% mutate(y=2010+y)
  lct.burnt <- rbind(lct.burnt, aux)
}
lct.burnt <- lct.burnt[-1,]
lct.burnt <- filter(lct.burnt, lct<=17)  ## només burnable land-cover types
tot <- sum(lct.burnt$n)
group_by(lct.burnt, lct) %>% summarise(area=sum(n), pct=round(100*area/tot,1))



############################ Test inputlyrs ############################

rm(list=ls())
library(sp)
library(raster)
library(tidyverse)

setwd("c:/work/MEDMOD/SpatialModelsR/MEDFIRE")  #NúLaptop

load("inputlyrs/rdata/mask.rdata")
LCFM <- raster("inputlyrs/asc/LCFspp_100m_31N-ETRS89.asc")
DEM <- raster("inputlyrs/asc/DEM_100m_31N-ETRS89.asc")
ASPECT <- raster("inputlyrs/asc/Aspect_100m_31N-ETRS89.asc")
SLOPE <- raster("inputlyrs/asc/SlopeDegree_100m_31N-ETRS89.asc")

BIOM  <- raster("inputlyrs/asc/Biomass_100m_31N-ETRS89.asc")
AGE  <- raster("inputlyrs/asc/ForestAge_100m_31N-ETRS89.asc")
TSDIST <- raster("inputlyrs/asc/TSDisturb_100m_31N-ETRS89.asc")
dta.orogra <- data.frame(m=MASK[], lcf=LCFM[], dem=DEM[], asp=ASPECT[], slope=SLOPE[]) %>% filter(!is.na(m))
dta <- data.frame(m=MASK[], lcf=LCFM[], biom=BIOM[], age=AGE[], tsd=TSDIST[]) %>% filter(!is.na(m))

# no NA in LCFM --> OK
any(is.na(dta$lcf))

# NA in DEM
any(is.na(dta$dem))
na.var <- filter(dta, is.na(dem))
table(na.var$lcf)

# NA in ASPECT
any(is.na(dta$asp))
na.aspect <- filter(dta, is.na(asp))
table(na.aspect$lcf)

# NA in SLOPE
any(is.na(dta$slope))
na.slope <- filter(dta, is.na(slope))
table(na.slope$lcf)


# only NA in TSD if LCFM is BareLand, Water or Urban --> OK
na.tsd <- filter(dta, is.na(tsd))
table(na.tsd$lcf)
    # 18     19     20 
    # 70660  22765 178914


# NA in BIOM
na.biom <- filter(dta, is.na(biom))
table(na.biom$lcf)
  # 15     16     17     18     19     20 
  # 79864 644652 355993  70660  22765 178914 

# NA in AGE
na.age <- filter(dta, is.na(age))
table(na.age$lcf)
    # 15     16     17     18     19     20 
    # 79864 644652 355993  70660  22765 178914 


############################# Compare MCSC v3 - 2009 to  MCSC v4 - 2018 ###################################
## Read MCSC 2018, and reclassify the 41-legend to the LCFM categories (use thesaurus)
MCSC <- raster("C:/WORK/CARTO-DATA/MCSC2018/MCSC18.tif")
DEM <- raster("C:/work/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/DEM_100m_31N-ETRS89.asc")
thesaurus <- read.table("C:/WORK/CARTO-DATA/MCSC2018/MCSC18_thesaurus.txt", sep="\t", header=T)
mcsc <- data.frame(cell.id=1:ncell(MCSC), x=MCSC[], dem=DEM[]) %>% filter(!is.na(x))
mcsc <- left_join(mcsc, thesaurus, by="x") %>% select(-x)
names(mcsc)[3] <- "lct18"
mcsc$lct18[mcsc$lct18==15 & mcsc$dem<=1500] <- 14
mcsc$lct18[mcsc$lct18==17] <- 16  #one type of agrico
count18 <- table(mcsc$lct18); count18
pct18 <- count18/nrow(mcsc); round(100*pct18,1)

## Read MCSC 2010
LCF <- raster("C:/WORK/MEDMOD/SpatialModelsR/MEDFIRE/inputlyrs/tiff/LCFspp_100m_31N-ETRS89.tif")
aux <- data.frame(cell.id=1:ncell(LCF), lct10=LCF[]) %>% filter(!is.na(lct10))
aux$lct10[aux$lct10<=13] <- 1  # forest, one single cover
aux$lct10[aux$lct10==17] <- 16  # one type of agrio
count10 <- table(aux$lct10); count10
pct10 <- count10/nrow(aux); round(100*pct10,1)
## Join both
lcf <- left_join(aux, mcsc, by="cell.id") %>% select(-dem)
lcf$lct18[is.na(lcf$lct18)] <- lcf$lct10[is.na(lcf$lct18)]  # 93 cel·les perdudes
table(lcf$lct10, lcf$lct18)
newcount18 <- table(lcf$lct18)
newcount18-count10
lcf$code <- paste0(lcf$lct10,lcf$lct18)
## Map with changes
MAP <- LCF
MAP[!is.na(MAP[])] <- lcf$code
writeRaster(MAP, "C:/WORK/MEDMOD/SpatialModelsR/MEDFIRE/rscripts/outs/LandChg1018.tif", 
            overwrite=T, format="GTiff")
land.cover.changes <- data.frame(cell.id=1:ncell(MAP), code=MAP[]) %>% filter(!is.na(code))
save(land.cover.changes, file="C:/WORK/MEDMOD/SpatialModelsR/MEDFIRE/inputlyrs/rdata/land.cover.changes.rdata")

## Read MCSC 1993
MCSC93 <- raster("C:/WORK/CARTO-DATA/MCSC/Versio1_MCSC93/MCSC93reclass_100m_31N-ETRS89.tif")
aux <- data.frame(cell.id=1:ncell(MCSC93), lct93=MCSC93[]) %>% filter(lct93!=255)
count93 <- table(aux$lct93); count93
## Join 2018 and 1993
lcf <- left_join(aux, mcsc, by="cell.id") %>% select(-dem)
lcf$lct18[is.na(lcf$lct18)] <- lcf$lct93[is.na(lcf$lct18)]  # 305 cel·les perdudes
table(lcf$lct93, lcf$lct18)
newcount18 <- table(lcf$lct18)
newcount18-count93



################################################# PLOT inputs #################################################
## Biomass
BIOMASS <- raster("C:/WORK/MEDMOD/SpatialModelsR/MEDFIRE/inputlyrs/asc/Biomass_100m_31N-ETRS89.asc")
BIOMASS@extent@xmin = BIOMASS@extent@xmin / 1000
BIOMASS@extent@xmax = BIOMASS@extent@xmax / 1000
BIOMASS@extent@ymin = BIOMASS@extent@ymin / 1000
BIOMASS@extent@ymax = BIOMASS@extent@ymax / 1000
levelplot(BIOMASS, margin=FALSE, colorkey=list(space="bottom"), par.settings=viridisTheme())
## Age
AGE <- raster("C:/WORK/MEDMOD/SpatialModelsR/MEDFIRE/inputlyrs/asc/ForestAge_100m_31N-ETRS89.asc")
AGE@extent@xmin = AGE@extent@xmin / 1000
AGE@extent@xmax = AGE@extent@xmax / 1000
AGE@extent@ymin = AGE@extent@ymin / 1000
AGE@extent@ymax = AGE@extent@ymax / 1000
levelplot(AGE, margin=FALSE, colorkey=list(space="bottom"), par.settings=magmaTheme())

## Distribution biomass in young stands
ggplot(filter(land, spp<=13, age<=10), aes(x=as.factor(age), y=biom)) +
  geom_violin(width=1.2, color="black", fill="grey") +
  geom_boxplot(width=0.1) +  theme_bw() +   theme(legend.position="none") +
  facet_wrap(.~as.factor(spp)) +
  stat_summary(fun.y=median, geom="point", size=2, color="black", shape="square")
## Distribution biomass in older stands
ggplot(filter(land, spp<=13, age>10 & age<=20), aes(x=as.factor(age), y=biom)) +
  geom_violin(width=1.2, color="black", fill="grey") +
  geom_boxplot(width=0.1) +  theme_bw() +   theme(legend.position="none") +
  facet_wrap(.~as.factor(spp)) +
  stat_summary(fun.y=median, geom="point", size=2, color="black", shape="square")
# 