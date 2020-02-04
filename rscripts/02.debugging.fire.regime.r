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
clim <- update.clim(MASK, land, orography, decade=(1+floor(t/10))*10, "rcp45", 5)
source("mdl/prob.igni.r")
pigni <- prob.igni(land, orography, clim, interface)
## Climatic severity and pctg hot days tabes
clim.severity <- read.table("inputfiles/ClimaticSeverity_test.txt", header=T)
## Tracking fire
track.fire <-  data.frame(run=NA, year=NA, swc=NA, clim.sever=NA, fire.id=NA, fire.spread.type=NA, 
                          fire.wind=NA, fire.size.target=NA, aburnt.highintens=NA, 
                          aburnt.lowintens=NA, asupp.fuel=NA, asupp.sprd=NA)

processes <- c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, T, FALSE, FALSE, TRUE)
temp.fire.schedule <- seq(1,91,1)
fire.id <- 4
pb.id <- 5
post.fire.id <- 7
hfire <- 4
lfire <- 5

## Basic arguments
irun <- 1
swc <- 1
## Fire regime characteristics
file.clim.severity <- "ClimaticSeverity_test"
file.pctg.hot.days <- "PctgHotDays_rcp45"
file.fire.suppression <- "FireSuppression_CurrExtrem"
clim.sever <- 0
## Spread rate, burn probability parameters, prescribed burns
fire.strength <- 1
rpb <- 2
stochastic.spread <- 1 #0.75, 1-0.75=0.25 creama o no aleatoriament, independentment del spread rate i pb
pb.th <- 0.9 #0.9 per sobre de 0.9 tot crema
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
annual.burnt <- 0

