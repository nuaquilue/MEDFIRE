rm(list=ls())
setwd("c:/work/MEDMOD/SpatialModelsR/MEDFIRE")  #Nú HP
setwd("d:/MEDMOD/SpatialModelsR/MEDFIRE")   #CTFC

# set the scenario
source("mdl/define.scenario.r")
scn.name <- "Test03_rcp45_10p"
define.scenario(scn.name)

# run the model
source("mdl/land.dyn.mdl.r")  
system.time(land.dyn.mdl(scn.name))

########################## Run fire.regime()
library(sp)
library(raster)  
library(RANN)  # for nn2()
## Arguments
t <- 1  
load("inputlyrs/rdata/mask.rdata")
load("inputlyrs/rdata/land.rdata")
load("inputlyrs/rdata/coordinates.rdata")
load("inputlyrs/rdata/orography.rdata")
load("inputlyrs/rdata/interface.rdata")
species <- c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster", "puncinata",
             "aalba", "qilex", "qsuber", "qfaginea", "qhumilis", "fsylvatica", "other")
source("mdl/update.clim.r")
clim <- update.clim(land, orography, MASK, species, decade=(1+floor(t/10))*10, "rcp45", 5)
source("mdl/prob.igni.r")
pigni <- prob.igni(land, clim, orography, interface)
swc <- 1
burnt.cells <- integer()
## Fire regime characteristics
file.clim.severity <- "ClimaticSeverity_rcp45_fixABA"
file.pctg.hot.days <- "PctgHotDays_rcp45"
file.fire.suppression <- "FireSuppression_CurrExtrem"
## Spread rate and burn probability parameters
fire.strength <- 1
rpb <- 0.9
stochastic.spread = 0.75
pb.th = 1	




## Create .Rdata with static variables of the model, only run once for all scenarios!
source("mdl/read.static.vars.r")
read.static.vars()
## Create .Rdata with initial values of variables of the model, used at each replicate of any scn.
source("mdl/read.sp.dyn.vars.r")
read.sp.dyn.vars()
## Create 2 data frames per climatic scn and decade: SDMs of all spp, and climatic variables (temp & precip) for CAT
source("mdl/read.climatic.vars.r")
read.climatic.vars()
## Save interfaces
source("mdl/update.interface.r")
load("inputlyrs/rdata/land.rdata")
interface <- update.interface(land)
save(interface, file="inputlyrs/rdata/interface.rdata")