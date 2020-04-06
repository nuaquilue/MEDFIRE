rm(list=ls())
 # setwd("c:/work/MEDMOD/SpatialModelsR/MEDFIRE")  #Nú HP
 setwd("d:/MEDMOD/SpatialModelsR/MEDFIRE")   #CTFC

# set the scenario
source("mdl/define.scenario.r")
scn.name <- "TestSDM85fire"
define.scenario(scn.name)
# run the model
source("mdl/land.dyn.mdl.r")  
system.time(land.dyn.mdl(scn.name))


## Create .Rdata with static variables of the model, only run once for all scenarios!
work.path <- "d:/MEDMOD/spatialmodelsr/Medfire"
source("mdl/read.static.vars.r")
read.static.vars(work.path)
## Create .Rdata with initial values of variables of the model, used at each replicate of any scn.
source("mdl/read.state.vars.r")
read.state.vars(work.path)
## Create 2 data frames per climatic scn and decade: SDMs of all spp, and climatic variables (temp & precip) for CAT
source("mdl/read.climatic.vars.r")
source("mdl/read.sdm.r")
read.climatic.vars(work.path)
read.sdm(work.path, "base")
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


work.path <- "c:/work/MEDMOD/spatialmodelsr/Medfire"
clim.scn <- "SMHI-RCA4_MOHC-HadGEM2-ES"
