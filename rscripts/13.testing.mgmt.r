############################################ RUN A SCN ##################################################
rm(list=ls())
source("mdl/define.scenario.r"); source("mdl/land.dyn.mdl.r")  
# Define scenario
scn.name <- "harvesting20_0102"; define.scenario(scn.name)
# Change target parameters
nrun <- 1
time.horizon <- 20
spin.up <- F
write.maps <- F
write.freq <- 1
is.climate.change <- F
clim.scn <- NA
file.pctg.hot.days <- "PctgHotDays_noCC"
file.clim.severity <- "ClimaticSeverity_noCC"
is.land.cover.change <- F
is.harvest <- T
is.wildfire <- F
is.postfire <- F
file.fire.suppression <- "FireSuppress"
dump(c("nrun", "time.horizon", "spin.up", "write.maps", "write.freq", "is.climate.change", "clim.scn", "file.pctg.hot.days", "file.clim.severity",
       "is.land.cover.change", "is.harvest", "is.wildfire", "is.postfire", "file.fire.suppression"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))


## Load required packages and functions 
library(raster)  
library(RANN)  
library(Rcpp)
library(tidyverse)
source("mdl/afforestation.r")
source("mdl/auxiliars.r")
source("mdl/cohort.establish.r")
source("mdl/drought.r")
source("mdl/fire.regime.r")
source("mdl/sustainable.mgmt.r")
source("mdl/forest.areas.r")
source("mdl/growth.r")
source("mdl/land.cover.change.r")
source("mdl/post.fire.r")
source("mdl/prob.igni.r")
source("mdl/update.clim.r")
source("mdl/update.interface.r")
sourceCpp("mdl/is.in.cpp")
`%notin%` <- Negate(`%in%`)

## Load scenario definition (global variables and scenario parameters)
## and customized scenario parameters
source(paste0("outputs/", scn.name, "/scn.def.r"))
if(file.exists(paste0("outputs/", scn.name, "/scn.custom.def.r")))
  source(paste0("outputs/", scn.name, "/scn.custom.def.r"))


## Load:
## 1. Mask of the study area (raster)
## 2. Data frame with cell.id and coordinates x, y
## 3. Data frame of the model static variables 
## 4. Data frame with interface value
load("inputlyrs/rdata/mask.rdata")
load("inputlyrs/rdata/coordinates.rdata")
load("inputlyrs/rdata/orography.rdata")
load("inputlyrs/rdata/harvest.rdata")
load("inputlyrs/rdata/interface.rdata")
load("inputlyrs/rdata/pfst.pwind.rdata")
if(spin.up)
  load("inputlyrs/rdata/wildfires.rdata")


## Set the directory for writing spatial outputs (create it, if it does not exist yet) 
if(write.maps){      
  if(!file.exists(paste0(out.path, "/rdata")))
    dir.create(file.path(getwd(), out.path, "/rdata"), showWarnings = F) 
}


## List the name of the forest species
species <- c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster", "puncinata",
             "aalba", "qilex", "qsuber", "qfaginea", "qhumilis", "fsylvatica", "other")


## Translation equations from Basal Area to Volum, Volum with bark and Carbon
eq.ba.vol <- read.table("inputfiles/EqBasalAreaVol.txt", header=T)
eq.ba.volbark <- read.table("inputfiles/EqBasalAreaVolWithBark.txt", header=T)
eq.ba.carbon <- read.table("inputfiles/EqBasalAreaCarbon.txt", header=T)
site.quality.shrub <- read.table("inputfiles/SiteQualityShrub.txt", header=T)


## Climatic severity 
clim.severity <- read.table(paste0("inputfiles/", file.clim.severity, ".txt"), header=T)


## Build the baseline time sequence and the time sequence of the processes (shared for all runs). 
## 1. Climate change, 2. Land-cover changes, 3. Forest management
## 4. Wildfires, 5. Prescribed burns, 6. Drought, 7. Post-fire regeneration,
## 8. Cohort establihsment, 9. Afforestation, 10. Growth
time.seq <- 
  lchg.schedule <- 
  mgmt.schedule <- 
  fire.schedule <- 
  pb.schedule <- 
  drought.schedule <-
  post.fire.schedule <- 
  cohort.schedule <- 
  afforest.schedule <- 
  growth.schedule <- seq(1, time.horizon, time.step)
if(spin.up & time.horizon>10){
  lchg.schedule <- seq(11, time.horizon, time.step)
  fire.schedule <- seq(11, time.horizon, time.step)
}
if(spin.up & time.horizon<=10){
  lchg.schedule <- 
    fire.schedule <- numeric()
}
clim.schedule <- seq(1, time.horizon, time.step*10) 

## Tracking data.frames
track.harvest <- data.frame(run=NA, year=NA, spp=NA, vol.sawlog=NA, vol.wood=NA)
track.forest.areas <- data.frame(run=NA, year=NA, forest=NA, spp.harvestable=NA, non.protect=NA,
                                 national.park=NA, enpe=NA, no.park=NA, slope30.nopark=NA, slope30.nopark.distpaht1.5=NA,
                                 slope30.nopark.distpaht2.2=NA, prep.cut_conif=NA, prep.cut_decid=NA, removal.cut_conif=NA, 
                                 removal.cut_decid=NA, seed.cut_conif=NA, seed.cut_decid=NA, thinning_conif=NA, thinning_decid=NA)
track.volumes <- data.frame(run=NA, year=NA, vol.potential.extract.sawlog_conif=NA, vol.potential.extract.sawlog_decid=NA, 
                            vol.potential.extract.wood_conif=NA, vol.potential.extract.wood_decid=NA,
                            vol.extract.sawlog_conif=NA, vol.extract.sawlog_decid=NA, 
                            vol.extract.wood_conif=NA, vol.extract.wood_decid=NA,
                            pct.sawlog_conif=NA, pct.sawlog_decid=NA, pct.wood_conif=NA, pct.wood_decid=NA, 
                            pct.conif_sawlog=NA, pct.conif_wood=NA, pct.decid_sawlog=NA, pct.decid_wood=NA) 
track.fire <- data.frame(run=NA, year=NA, swc=NA, clim.sever=NA, fire.id=NA, fst=NA, wind=NA, atarget=NA, 
                         aburnt.highintens=NA, aburnt.lowintens=NA, asupp.fuel=NA, asupp.sprd=NA, rem=NA)
track.fire.spp <- data.frame(run=NA, year=NA, fire.id=NA, spp=NA, aburnt=NA, bburnt=NA)
# track.step <- data.frame(run=NA, year=NA, fire.id=NA, step=NA, nneigh=NA, nneigh.in=NA, nburn=NA, nff=NA)
# track.sr <- data.frame(run=NA, year=NA, swc=NA, clim.sever=NA, cell.id=NA, fire.id=NA, spp=NA, age=NA, fi=NA, pb=NA,
#                        nsource=NA, nsupp.sprd=NA, nsupp.fuel=NA, tosupp.sprd=NA, tosupp.fuel=NA, burn=NA)
# track.sr.source <- data.frame(run=NA, year=NA, swc=NA, clim.sever=NA, cell.id=NA, spp=NA, biom=NA, age=NA, fuel=NA,
#                               source.id=NA, position=NA, dist=NA, windir=NA, nsupp.sprd=NA, nsupp.fuel=NA,
#                               elev.x=NA, elev.y=NA, dif.elev=NA, dif.wind=NA, slope=NA, wind=NA, sr=NA, fi=NA, pb=NA)
track.pb <- data.frame(run=NA, year=NA, clim.sever=NA, fire.id=NA, 
                       wind=NA, atarget=NA, aburnt.lowintens=NA)
track.drougth <- data.frame(run=NA, year=NA, spp=NA, ha=NA)
track.cohort <- data.frame(run=NA, year=NA, spp.out=NA, spp.in=NA, ha=NA) #Var2=NA, Freq=NA)
track.post.fire <- data.frame(run=NA, year=NA, spp.out=NA, spp.in=NA, ha=NA) #Var2=NA, Freq=NA)
track.afforest <- data.frame(run=NA, year=NA, spp=NA, ha=NA) #Var1=NA, Freq=NA)
track.land <- data.frame(run=NA, year=NA, spp=NA, age.class=NA, area=NA, vol=NA, volbark=NA, carbon=NA)


## Start the simulations   
irun <- 1


## Load initial spatial dynamic state variables in a data.frame format
load("inputlyrs/rdata/land.rdata")
if(spin.up)
  load("inputlyrs/rdata/land.cover.changes.rdata")

## Land at time 0, at the initial stage
aux.forest <- filter(land, spp<=13) %>% select(spp, age, biom) %>% left_join(eq.ba.vol, by="spp") %>% 
  mutate(vol=cx*biom/10+cx2*biom*biom/100) %>% select(-cx, -cx2) %>%
  left_join(eq.ba.volbark, by="spp") %>% 
  mutate(volbark=cx*biom/10+cx2*biom*biom/100) %>% select(-cx, -cx2) %>% 
  left_join(eq.ba.carbon, by="spp") %>% 
  mutate(carbon=c*biom/10) %>% 
  mutate(age.class=ifelse(spp<=7 & age<=15, "young", ifelse(spp<=7 & age<=50, "mature",
                                                            ifelse(spp<=7 & age>50, "old", ifelse(spp>7 & spp<=13 & age<=15, "young",
                                                                                                  ifelse(spp>7 & spp<=13 & age<=50, "mature", "old")))))) %>%       
  group_by(spp, age.class) %>% select(-c) %>%
  summarise(area=length(vol), vol=sum(vol), volbark=sum(volbark), carbon=sum(carbon))  
aux.shrub <- filter(land, spp==14) %>% select(spp, biom) %>% group_by(spp) %>%
  summarise(age.class=NA, area=length(biom), vol=sum(biom), volbark=0, carbon=0)  
aux.other <- filter(land, spp>14) %>% select(spp) %>% group_by(spp) %>%
  summarise(age.class=NA, area=length(spp), vol=0, volbark=0, carbon=0)  
track.land <- rbind(track.land, data.frame(run=irun, year=0, aux.forest), data.frame(run=irun, year=0, aux.shrub),
                    data.frame(run=irun, year=0, aux.other))

## Start the discrete time sequence 
t <- 1
time.seq <- 1:1

