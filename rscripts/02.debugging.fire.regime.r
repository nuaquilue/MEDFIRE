########################## Run fire.regime() ##########################
rm(list=ls())
library(sp)
library(raster)  
library(RANN)  # for nn2()
library(Rcpp)
library(tidyverse)
select <- dplyr::select
source("mdl/fire.regime.r")
source("mdl/update.clim.r")
source("mdl/prob.igni.r")
source("mdl/growth.r")
source("mdl/post.fire.r")
source("mdl/auxiliars.r")
sourceCpp("mdl/is.in.cpp")

# Name scn and output folder
scn.name <- "TestPerformanceFire"
out.path <- paste0("outputs/", scn.name)
dir.create(file.path(getwd(), out.path), showWarnings = F) 
dir.create(file.path(getwd(), out.path, "/lyr"), showWarnings = F) 

## Input data from land.dyn.mdl.r
load("inputlyrs/rdata/mask.rdata")
load("inputlyrs/rdata/land.rdata")
load("inputlyrs/rdata/coordinates.rdata")
load("inputlyrs/rdata/orography.rdata")
load("inputlyrs/rdata/interface.rdata")

## Climate
clim.scn <- "rcp45"
clim.mdl <- "SMHI-RCA4_MOHC-HadGEM2-ES"
clim <- update.clim(MASK, land, orography, decade=10, clim.scn, clim.mdl)
load(paste0("inputlyrs/rdata/sdm_base_", clim.scn, "_", clim.mdl, "_", 10, ".rdata"))

## Probability of ignition
pigni <- prob.igni(land, orography, clim, interface)

## Climatic severity and pctg hot days tabes
clim.severity <- read.table("inputfiles/ClimaticSeverity_test.txt", header=T)

## Tracking fire
track.fire <-  data.frame(run=NA, year=NA, swc=NA, clim.sever=NA, fire.id=NA, fst=NA, 
                          wind=NA, atarget=NA, aburnt.highintens=NA, 
                          aburnt.lowintens=NA, asupp.fuel=NA, asupp.sprd=NA)
track.fire.spp <-  data.frame(run=NA, year=NA, fire.id=NA, spp=NA, aburnt=NA, bburnt=NA)
track.post.fire <- data.frame(run=NA, year=NA, spp.out=NA, Var2=NA, Freq=NA)
processes <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
temp.fire.schedule <- seq(1,91,1)
temp.growth.schedule <- seq(1,91,1)
temp.post.fire.schedule <- seq(1,91,1)
hfire <- 6
lfire <- 7
fire.id <- 4
post.fire.id <- 7
growth.id <- 10

## Neigh radius
spp.distrib.rad <- 20 	# i.e. 2 km
shrub.colon.rad <- 5 		# i.e. 500 m

## Basic arguments
t <- 1
irun <- 1
swc <- 1

## Fire regime characteristics
file.clim.severity <- "ClimaticSeverity_test"
file.pctg.hot.days <- "PctgHotDays_rcp45"
file.fire.suppression <- "FireSuppression_CurrExtrem"
file.sprd.weight <- "SprdRateWeights_E"
clim.sever <- 0
## Spread rate, burn probability parameters, prescribed burns
# rpb.sr <- 1.5
rpb <- 0.5
stochastic.spread <- 0.9 #0.75, 1-0.75=0.25 creama o no aleatoriament, independentment del spread rate i pb
pb.upper.th <- 0.75  # 0.9 - wind, 0.8 - convective, 
pb.lower.th <- 0.05  # 0.25 - wind, 0.1 - convective, 
fire.intens.th <- 0.35  # high vs. low intensity fire, SR_noAcc <= fire.intens.th
pb.target.area <- NA  # if NA, then burnt as 7*pb.convenient.area, otherwise annually burnt pb.fix.area
pb.convenient.area <- 400 ## should be 15.000 ha
accum.burnt.area <- rep(pb.convenient.area,7)
pb.mean <- 1.974
pb.sd <- 0.683
pb.fage.th <- 30 ## minimum forest age to apply prescribed burns
## Tracking vars
burnt.cells <- integer()
burnt.intens <- integer()
fintensity <- integer()
fire.ids <- integer()
id.fire <- annual.burnt <- 0


######################## FIRST PART OF FIRE.REGIME.R, LINES 9 TO 89 ########################
cat(paste0("Fires in SWC: ", ifelse(swc==1, "Wind.", ifelse(swc==2, "Heat.", 
                                                            ifelse(swc==3, "Regular.", "Prescribed.")))))

## Function to select items not in a vector
`%notin%` <- Negate(`%in%`)

## Read and load input data
load("inputlyrs/rdata/pfst.pwind.rdata")
clim.severity <- read.table(paste0("inputfiles/", file.clim.severity, ".txt"), header=T)
pctg.hot.days <- read.table(paste0("inputfiles/", file.pctg.hot.days, ".txt"), header=T)
prob.hot <- read.table("inputfiles/ProbHot.txt", header=T)
prob.conv <- read.table("inputfiles/ProbConv.txt", header=T)
aba.dist <- read.table("inputfiles/AnnualBurntAreaDist.txt", header=T)
fs.dist <- read.table("inputfiles/FireSizeDist.txt", header=T)
fire.supp <- read.table(paste0("inputfiles/", file.fire.suppression, ".txt"), header=T)
spp.flammability <- read.table("inputfiles/SppSpreadRate.txt", header=T)
fst.sprd.weight <- read.table(paste0("inputfiles/", file.sprd.weight, ".txt"), header=T)


## To be sure that non-burnable covers do not burn (water, rock, urban), nor agriculture land
## under prescribed burns
i <- land$spp<=17        # 2.938.560
subland <- land[i,]   
suborography <- orography[i,]


## Reset TrackFires data frame each run and swc
track.fire <- data.frame(year=NA, swc=NA, clim.sever=NA, fire.id=NA, fst=NA, 
                         wind=NA, atarget=NA, aburnt.highintens=NA, 
                         aburnt.lowintens=NA, asupp.fuel=NA, asupp.sprd=NA)


## Wind direction between 12 neigbours
## Wind direction is coded as 0-N, 45-NE, 90-E, 135-SE, 180-S, 225-SW, 270-W, 315-NE
default.neigh <- data.frame(x=c(-1,1,2900,-2900,2899,-2901,2901,-2899,-2,2,5800,-5800),
                            windir=c(270,90,180,0,225,315,135,45,270,90,180,0),
                            dist=c(100,100,100,100,141.421,141.421,141.421,141.421,200,200,200,200))
default.nneigh <- nrow(default.neigh)


## Find either fixed or stochastic annual target area for wildfires
if(swc<4){ 
  ## Fixed
  if(sum(clim.severity[clim.severity$year==t,2:4])>0){  #
    is.aba.fix <- T
    area.target <- clim.severity[clim.severity$year==t, swc+1]
  }
  ## Find stochastic annual burnt area
  else{ 
    is.aba.fix <- F
    if(clim.sever==1 & swc<=2){  # decide if climatic severity is extrem for 'wind' or 'heat' swc
      pctg <- pctg.hot.days[pctg.hot.days$year==t, swc+1]
      prob.extrem <- 1/(1+exp(-(prob.hot$inter[swc] + prob.hot$slope[swc]*pctg)))
      if(runif(1,0,100) <= prob.extrem) # extreme
        clim.sever <- 2
    }
    area.target <- round(min(200000, max(10, 
                                         rlnorm(1, aba.dist$meanlog[aba.dist$clim==clim.sever & aba.dist$swc==swc],
                                                aba.dist$sdlog[aba.dist$clim==clim.sever & aba.dist$swc==swc])))) 
  }  
}
cat(paste(" Annual target area:", area.target), "\n")


## Update prob.igni according to swc
pigni <- mutate(pigni, psft=p*pfst.pwind[,ifelse(swc==1,1,2)+1]) %>%
  filter(cell.id %in% subland$cell.id)


## Pre-select the coordinates of old Mediterranean vegetation, i.e.
## Pinus halepensis, Pinus nigra, and Pinus pinea of age >=30 years.
## to compute probability of being a convective fire
old.forest.coord <- filter(subland, spp<=3 & age>=30) %>% select(cell.id) %>% left_join(coord, by = "cell.id")



## Start burning until annual area target is not reached
fire.id <- 0
track.spread <- data.frame(fire.id=fire.id, cell.id=NA, step=NA, spp=NA, biom=NA, age=NA, fuel=NA,
                           slope=NA, wind=NA, flam=NA, aspc=NA, sr=NA, pb=NA, burning=NA)



######################## SECOND PART OF FIRE.REGIME.R, LINES 102 TO 167 ########################
## ID for each fire event
fire.id <- fire.id+1

## Select an ignition point, to then decide the fire spread type, the fire suppression level,
## the wind direction and the target fire size according to clim and fire spread type
igni.id <- sample(pigni$cell.id, 1, replace=F, pigni$p)

## Assign the fire spread type 
fire.spread.type <- swc
wwind <- fst.sprd.weight[1,fire.spread.type+1]
wslope <- fst.sprd.weight[2,fire.spread.type+1]
wflam <- fst.sprd.weight[3,fire.spread.type+1]
waspc <- fst.sprd.weight[4,fire.spread.type+1]

## Assign the fire suppression levels
sprd.th <- filter(fire.supp, clim==clim.sever, fst==fire.spread.type)$sprd.th
fuel.th <- filter(fire.supp, clim==clim.sever, fst==fire.spread.type)$fuel.th

## Assign the main wind direction according to the fire spread type
## Wind directions: 0-N, 45-NE, 90-E, 135-SE, 180-S, 225-SW, 270-W, 315-NE
if(fire.spread.type==1)  # N, NW or W according to map
  fire.wind <- sample(c(0,315,270), 1, replace=F, p=filter(pfst.pwind,cell.id==igni.id)[3:5])
if(fire.spread.type==2)  # S 80%, SW 10%, SE 10%
  fire.wind <- sample(c(180,225,135), 1, replace=F, p=c(80,10,10))
if(fire.spread.type==3)  # any at random
  fire.wind <- sample(seq(0,315,45), 1, replace=F)
spp.flam <- filter(spp.flammability, fst==fire.spread.type) %>% select(-fst)

## Derive target fire size from a power-law according to clima and fire.spread.type 
log.size <- seq(1.7, 5, 0.01)
log.num <- filter(fs.dist, clim==clim.sever, fst==fire.spread.type)$intercept +
           filter(fs.dist, clim==clim.sever, fst==fire.spread.type)$slope * log.size
fire.size.target <- sample(round(10^log.size), 1, replace=F, prob=10^log.num)
fire.size.target <- area.target; fire.size.target

## Initialize tracking variables
fire.front <- igni.id
aburnt.lowintens <- 0
aburnt.highintens <- 1  # ignition always burnt, and it does in high intensity
asupp.sprd <- 0
asupp.fuel <- 0
burnt.cells <- igni.id
visit.cells <- igni.id
burnt.intens <- T
fire.step <- 1
track.spread <- rbind(track.spread, data.frame(fire.id=fire.id, cell.id=igni.id, step=fire.step, 
                spp=land$spp[land$cell.id==igni.id], biom=1, age=1, fuel=1,
                slope=0, wind=0, flam=0, aspc=0, sr=1, pb=1, burning=1))
