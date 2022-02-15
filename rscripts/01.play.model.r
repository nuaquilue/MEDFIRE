library(sp)
library(raster)
library(tidyverse)

############################################ RUN A SCN ##################################################
rm(list=ls())
source("mdl/define.scenario.r"); source("mdl/land.dyn.mdl.r")  
# Define scenario
scn.name <- "Scn_ALL"; define.scenario(scn.name)
# Change target parameters
nrun <- 1
time.horizon <- 10
spin.up <- F
write.maps <- F
is.climate.change <- T
clim.scn <- "rcp85"
file.clim.severity <- paste0("ClimaticSeverity_", ifelse(is.na(clim.scn), "noCC", clim.scn))
file.pctg.hot.days <- paste0("PctgHotDays_", ifelse(is.na(clim.scn), "noCC", clim.scn)) 
is.land.cover.change <- T
is.harvest <- T
is.wildfire <- T
is.postfire <- T
is.drought <- T
is.cohort.establish <- T
is.afforestation <- T
is.encroachment = T
file.fire.suppression <- "FireSuppress"
dump(c("nrun", "time.horizon", "spin.up", "write.maps",  "is.climate.change", "clim.scn", "file.clim.severity", 
      "file.pctg.hot.days",  "is.land.cover.change", "is.harvest", "is.wildfire", "is.postfire", "is.drought",
      "is.cohort.establish", "is.afforestation", "file.fire.suppression"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
land.dyn.mdl(scn.name)



############################################### RUN FACTORIAL SCN ################################################
rm(list=ls())
source("mdl/define.scenario.r"); source("mdl/land.dyn.mdl.r") 
scenarios <- readxl::read_xlsx("Scenarios.xlsx", sheet="GCM5")
i=1
for(i in 1){
  scn.name <- paste0(scenarios$scn.name[i], "_1run")
  define.scenario(scn.name)
  ## general
  nrun <-1 #scenarios$nrun[i]
  write.maps <- F
  spin.up <- as.logical(scenarios$spin.up[i])
  ## processes
  is.climate.change <- as.logical(scenarios$is.climate.change[i])
  is.land.cover.change <- as.logical(scenarios$is.land.cover.change[i])
  is.harvest <- as.logical(scenarios$is.harvest[i])
  is.wildfire <- as.logical(scenarios$is.wildfire[i])
  is.postfire <- as.logical(scenarios$is.wildfire[i])
  ## scenario parameters
  file.fire.suppression <- scenarios$fire.suppression[i]
  clim.scn <- "rcp85"
  if(is.climate.change){
    file.pctg.hot.days <- "PctgHotDays_rcp85"
    file.clim.severity <- "ClimaticSeverity_rcp85"
  }
  if(!is.climate.change){  
    file.pctg.hot.days <- "PctgHotDays_noCC"
    file.clim.severity <- "ClimaticSeverity_noCC"
  }
  dump(c("nrun", "write.maps", "spin.up", "is.climate.change", "is.land.cover.change", "is.harvest", 
         "is.wildfire", "is.postfire", "file.fire.suppression", "clim.scn", "file.pctg.hot.days", "file.clim.severity"), 
       paste0("outputs/", scn.name, "/scn.custom.def.r"))
  land.dyn.mdl(scn.name)
}


 ###########################################################################################################
## Create .Rdata with static variables of the model, only run once for all scenarios!
source("mdl/read.static.vars.r")
read.static.vars()
## Create .Rdata with initial values of variables of the model, used at each replicate of any scn.
source("mdl/read.state.vars.r")
read.state.vars()
## Create a data frame with climatic variables (temp & precip) for CAT
source("mdl/read.climatic.vars.r")
read.climatic.vars()
# s per climatic scn and decade: SDMs of all spp, and
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


################### climate ###################
library(tidyverse)
source("mdl/update.clim.r")
load("inputlyrs/rdata/land.rdata")
load("inputlyrs/rdata/orography.rdata")
## Build clim df with sqi and sdm
for(clim.mdl in c("KNMI-RACMO22E_ICHEC-EC-EARTH",
                  "KNMI-RACMO22E_MOHC-HadGEM2-ES",
                  "SMHI-RCA4_CNRM-CERFACS-CNRM-CM5",
                  "SMHI-RCA4_MPI-M-MPI-ESM-LR",
                  "SMHI-RCA4_MOHC-HadGEM2-ES"))
{
  hist.clim(land, orography, clim.mdl)
}
