library(readxl)
library(raster)
library(viridis)
library(tidyverse)

############################################ RUN A SCN ##################################################
rm(list=ls())
source("mdl/define.scenario.r")
source("mdl/land.dyn.mdl.r")  
# Define scenario
scn.name <- "landcover"
define.scenario(scn.name)
# Change target parameters
nrun <- 1
time.horizon <- 90
write.sp.outputs <- F
spin.up <- T
clim.scn <- "rcp85"
file.clim.severity <- "ClimaticSeverity_test.conv"
file.pctg.hot.days <- "PctgHotDays_rcp85"
is.land.cover.change <- T
is.wildfire <- T
is.postfire <- T
dump(c("clim.scn", "file.pctg.hot.days", "file.clim.severity", "spin.up", 
       "is.wildfire", "is.postfire", "is.land.cover.change",
       "nrun", "write.sp.outputs", "time.horizon"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
system.time(land.dyn.mdl(scn.name))


############################################### RUN FACTORIAL SCN ################################################
rm(list=ls())
source("mdl/define.scenario.r"); source("mdl/land.dyn.mdl.r") 
scenarios <- read_xlsx("Scenarios.xlsx", sheet="Obj1")
for(i in 2){
  scn.name <- scenarios$scn.name[i]
  define.scenario(scn.name)
  ## general
  nrun <- scenarios$nrun[i]
  write.sp.outputs <- F
  spin.up <- as.logical(scenarios$spin.up[i])
  ## processes
  is.drought <- T
  is.cohort.establish <- T
  is.afforestation <- T
  is.growth <- T
  is.climate.change <- as.logical(scenarios$is.climate.change[i])
  is.land.cover.change <- as.logical(scenarios$is.land.cover.change[i])
  is.harvest <- as.logical(scenarios$is.harvest[i])
  is.wildfire <- as.logical(scenarios$is.wildfire[i])
  is.prescribed.burn <- F
  is.postfire <- as.logical(scenarios$is.postfire[i])
  ## scenario parameters
  file.fire.suppression <- scenarios$fire.suppression[i]
  if(is.climate.change){
    clim.scn <- "rcp85"
    file.pctg.hot.days <- "PctgHotDays_rcp85"
    file.clim.severity <- "ClimaticSeverity_rcp85"
  }
  if(!is.climate.change){  
    clim.scn <- NA
    file.pctg.hot.days <- "PctgHotDays_noCC"
    file.clim.severity <- "ClimaticSeverity_noCC"
  }
  dump(c("nrun", "write.sp.outputs", "spin.up", "is.drought", "is.cohort.establish", "is.afforestation", "is.growth",
         "is.climate.change", "is.land.cover.change", "is.harvest", "is.wildfire", "is.prescribed.burn",
         "is.postfire", "file.fire.suppression", "clim.scn", "file.pctg.hot.days", "file.clim.severity"), 
       paste0("outputs/", scn.name, "/scn.custom.def.r"))
  land.dyn.mdl(scn.name)
}


###########################################################################################################
## Create .Rdata with static variables of the model, only run once for all scenarios!
work.path <- getwd()
source("mdl/read.static.vars.r")
read.static.vars(work.path)
## Create .Rdata with initial values of variables of the model, used at each replicate of any scn.
source("mdl/read.state.vars.r")
read.state.vars(work.path)
## Create 2 data frames per climatic scn and decade: SDMs of all spp, and climatic variables (temp & precip) for CAT
source("mdl/read.climatic.vars.r")
read.climatic.vars(work.path)
source("mdl/read.sdm.r")
work.path <- "C:/WORK/MEDMOD"
read.sdm(work.path, "base")
read.sdm(work.path, "plan1")
read.sdm(work.path, "planfix")
## Save interfaces
source("mdl/update.interface.r")
load("inputlyrs/rdata/land.rdata")
interface <- update.interface(land)
save(interface, file="inputlyrs/rdata/interface.rdata")
# write as raster
load("inputlyrs/rdata/mask.rdata")
INTERFACE <- MASK
INTERFACE[!is.na(MASK[])] <- interface$x
plot(INTERFACE, col=rainbow(7))
writeRaster(INTERFACE, "C:/WORK/MEDMOD/SpatialModelsR/MEDFIRE/inputlyrs/asc/Interface_100m_31N-ETRS89.asc",
            format="ascii", NAflag=-1, overwrite=T)



