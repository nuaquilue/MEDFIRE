########################## Run fire.regime() ##########################
rm(list=ls())
library(sp)
library(raster)  
library(RANN)  # for nn2()
library(tidyverse)
source("mdl/fire.regime.r")

# Name scn and output folder
scn.name <- "TestFire"
out.path <- paste0("outputs/", scn.name)
dir.create(file.path(getwd(), out.path), showWarnings = F) 
dir.create(file.path(getwd(), out.path, "/asc"), showWarnings = F) 

## Input data from land.dyn.mdl.r
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

track.fire <-  data.frame(run=NA, year=NA, swc=NA, clima=NA, fire.id=NA, fire.spread.type=NA, 
                          fire.wind=NA, fire.size.target=NA, aburnt.highintens=NA, 
                          aburnt.lowintens=NA, asupp.fuel=NA, asupp.sprd=NA)
hfire <- 4
lfire <- 5

## Basic arguments
irun <- 1
swc <- 1
burnt.cells <- integer()
burnt.intens <- integer()
fire.intens.th <- 0.35
## Fire regime characteristics
file.clim.severity <- "ClimaticSeverity_rcp45_fixABA"
file.pctg.hot.days <- "PctgHotDays_rcp45"
file.fire.suppression <- "FireSuppression_CurrExtrem"
## Spread rate and burn probability parameters
fire.strength <- 1
rpb <- 0.9
stochastic.spread = 0.75
pb.th = 1	

