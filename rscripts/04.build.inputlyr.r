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
setwd("d:/MEDMOD/SpatialModelsR/MEDFIRE")   #CTFC
load("inputlyrs/rdata/mask.rdata")

## Default extent of raster maps of Catalonia  
extCat <- extent(c(250000, 540000, 4480000, 4760000))


## BIOPHYSIC VARIABLES: BASAL AREA
all.files <- list.files("D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic")
files.select <- all.files[seq(1,230,8)]
BA <- raster(paste0("D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/", files.select[1]))
BAall <- extend(BA, extent(c(250000, 540000, 4480000, 4766800)))
for(sh in files.select[-1]){
  BAsheet <- raster(paste0("D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/", sh))
  aux <- extend(BAsheet, extent(c(250000, 540000, 4480000, 4766800)))
  BAall <- merge(BAall, aux)
}
crs(BAall) <- CRS("+init=epsg:25831")
BAall[BAall[]<0] <- 0
BAall[] <- BAall[]*10
plot(BAall)
## Change resolution from 20m to 100m
BAcat <- crop(BAall, extCat)
BA100m <- aggregate(BAcat, fact=5, mean, expand=T)
BA100m
writeRaster(BA100m, "inputlyrs/asc/Biomass2010x10_100m_31N-ETRS89.asc", 
            format="ascii", NAflag=-1, overwrite=T)


## BIOPHYSIC VARIABLES: HEIGHT
all.files <- list.files("D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic")
files.select <- all.files[seq(7,230,8)]
HM <- raster(paste0("D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/", files.select[1]))
HMall <- extend(HM, extent(c(250000, 540000, 4480000, 4766800)))
for(sh in files.select[-1]){
  HMsheet <- raster(paste0("D:/MEDMOD/InputLayers_MEDFIRE_II/VarsBiophysic/", sh))
  aux <- extend(HMsheet, extent(c(250000, 540000, 4480000, 4766800)))
  HMall <- merge(HMall, aux)
}
crs(HMall) <- CRS("+init=epsg:25831")
HMcat[HMall[]<0] <- 0
plot(HMall)
## Change resolution from 20m to 100m
HMcat <- crop(HMall, extCat)
HM100m <- aggregate(HMcat, fact=5, mean, expand=T)
writeRaster(HM100m, "inputlyrs/asc/MeanHeight2010_100m_30NETRS89.asc", 
            format="ascii", NAflag=-1, overwrite=T)
## Height has to be transformed to age!!


## DEM 15x15 
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



