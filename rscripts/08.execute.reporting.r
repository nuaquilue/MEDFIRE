rm(list=ls())
library(sp)
library(raster)
library(fmsb)
library(tidyverse)
library(viridis)
library(cowplot)
options(dplyr.summarise.inform=F)
source("rscripts/09.reporting.one.scn.r")
source("rscripts/10.reporting.bytime.r")

list.scn <- c("NULL", "LC", "FM", "WF", "WF_FS", "LC_FM", "LC_WF", "LC_WF_FS", 
              "FM_WF", "FM_WF_FS", "LC_FM_WF", "LC_FM_WF_FS",
              "CC", "CC_LC", "CC_FM", "CC_WF", "CC_WF_FS", "CC_LC_FM", "CC_LC_WF", "CC_LC_WF_FS", 
              "CC_FM_WF", "CC_FM_WF_FS", "CC_LC_FM_WF", "CC_LC_FM_WF_FS")

scn.name <- "CC_LC_FM_WF_FS"

species <- data.frame(spp=1:17, 
                      name=c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster",
                             "puncinata", "aalba", "qilex", "qsuber", "qfaginea", "qhumilis",
                             "fsylvatica", "other", "shrub", "grass", "crop", "orchard"),
                      namefull = c("Pinus halepensis", "Pinus nigra", "Pinus pinea", "Pinus sylvestris", "Pinus pinaster",
                                   "Pinus uncinata", "Abies alba", "Quercus ilex", "Quercus suber", "Quercus faginea", 
                                   "Quercus humilis", "Fagus sylvatica", "Other trees", "Shrubland", "Grassland", 
                                   "Cropland", "Orchards"),
                      type=c("conifer", "conifer", "conifer", "conifer", "conifer",
                             "conifer", "conifer", "deciduous", "deciduous", "deciduous", "deciduous",
                             "deciduous", "deciduous", "shrub-grass", "shrub-grass", "crop", "crop"))

species.sort = data.frame(spp=c(7, 12, 13,  1,  2,  5,  3,  4,  6, 10, 11,  8,  9, 14, 15, 16, 17),
                          name=c("aalba", "fsylvatica", "other", "phalepensis", "pnigra", "ppinaster",
                                 "ppinea", "psylvestris", "puncinata", "qfaginea", "qhumilis", "qilex", 
                                 "qsuber", "shrub", "grass", "crop", "orchard"),
                          color=c("royalblue4", "red4", "purple3", "chartreuse3", "darkolivegreen1",
                                  "darkslategrey", "seagreen1", "forestgreen", "grey10", 
                                  "darkorange2", "palegoldenrod", "gold", "saddlebrown",  
                                  "grey70", "deeppink", "azure2", "azure2")) #cornsilk3


## 09.reporting.one.scn.r
for(scn.name in list.scn){
  cat(paste(scn.name, "\n"))
  plot.drought(scn.name, species, species.sort)     # > 01.DroughtKilled
  plot.cohort(scn.name, species, species.sort)      # > 02.CohortReplace
  plot.abundance(scn.name, species, species.sort)   # > 03.Abundance, 04.AbundancePctg, 05.AbundanceRel, 06.AbundanceShrubGrass
  plot.afforest(scn.name, species, species.sort)    # > 07.Afforestation
  plot.land.covers(scn.name)                        # > 08.AbundanceRelLandCover
  plot.sqi(scn.name, species)                       # > 09.SQI, 10.SQIsppArea, 11.SQIsppVol
  plot.age.class(scn.name, species)                 # > 12.AgeClass
  plot.burnt(scn.name, species)                     # > 13.SppBurnt
  plot.harvest(scn.name, species)                   # > 14.HarvestType
}


## 10.reporting.bytime.r
plot.volume(list.scn)
taxo.rich(list.scn)
plot.age(list.scn)
plot.carbon.burnt()
plot.spp.burnt()
plot.ab.at()

## 11.reporting.compare.r
plot.rel.lc(list.scn)



##  t     |     year    |   decade
##  0     |     2009
##  1     |     2010    |   2010
##  ...
##  10    |     2019    |   2010
##  11    |     2020    |   2020
##  ...
##  20    |     2029    |   2020
##  ...
##  81    |     2090    |   2090
##  ...
##  90    |     2099    |   2090