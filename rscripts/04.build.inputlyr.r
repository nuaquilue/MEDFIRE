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
# setwd("c:/work/MEDMOD/SpatialModelsR/MEDFIRE")  #NúLaptop
setwd("d:/MEDMOD/SpatialModelsR/MEDFIRE")   #CTFC
  # load("inputlyrs/rdata/land.rdata")
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
BAcat[BAall[]<0] <- 0
plot(BAall)
## Change resolution from 20m to 100m
BAcat <- crop(BAall, extCat)
BA100m <- aggregate(BAcat, fact=5, mean, expand=T)
writeRaster(BA100m, "inputlyrs/asc/BasalArea_100m_30NETRS89.asc", format="ascii", NAflag=-1, overwrite=T)


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
writeRaster(HM100m, "inputlyrs/asc/MeanHeight_100m_30NETRS89.asc", format="ascii", NAflag=-1, overwrite=T)


## DEM 5x5 --> DEM 100m
sheets <- list.files("D:/MEDMOD/InputLayers_MEDFIRE_II/DEM5x5", pattern="*.txt")
DEM <- raster(paste0("D:/MEDMOD/InputLayers_MEDFIRE_II/DEM5x5/", sheets[1]))
for(sh in sheets[-1]){
  DEM5 <- raster(paste0("D:/MEDMOD/InputLayers_MEDFIRE_II/DEM5x5/", sh))
  DEM <- merge(DEM, DEM5)
}
crs(DEM) <- CRS("+init=epsg:25831")
plot(DEM)
## CROP!!!

## Change resolution from 5m to 100m
DEM100m <- aggregate(DEM, fact=20, mean, expand=T)
DEM100m <- extend(DEM100m, extCat)
writeRaster(DEM100m, "inputlyrs/asc/DEM_100m_30NETRS89.asc", format="ascii", NAflag=-1, overwrite=T)

## SLOPE (º)
SLOPE <- terrain(DEM, opt='slope', unit='degrees', neighbors=8)
writeRaster(SLOPE, "D:/MEDMOD/DataCLIM/DataSp/SlopeDegree_CAT1K.asc")


## ASPECT: from º to 4 categories
ASPECT <- terrain(DEM, opt='aspect', unit='degrees', neighbors=8)
ASPECT[] <- ifelse(ASPECT[]<=45, 1,
                   ifelse(ASPECT[]<=115, 2,
                          ifelse(ASPECT[]<=225, 3,
                                 ifelse(ASPECT[]<=315, 4, 1))))
writeRaster(ASPECT, "D:/MEDMOD/DataCLIM/DataSp/AspectDegree_CAT1K.asc")



## UTM
UTM <- readOGR("D:/MEDMOD/InputLayers_MEDFIRE_II/Malla1k_Catalunya_ETRS89UTM/Malla1k_CatalunyaReduc_ETRS89UTM.shp")
crs(UTM) <- CRS("+init=epsg:25831")
plot(UTM)
UTMrast <- rasterize(UTM, MASK, field="IDCEL1KM")  ## change by a MASK in 31N-ETRS89
## crop!!
writeRaster(UTMrast, "D:/MEDMOD/SpatialModelsR/medfire/inputlyrs/asc/utm.asc", 
            format="ascii", NAflag=-1, overwrite=T)



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



