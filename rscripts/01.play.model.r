############################################ RUN A SCN ##################################################
rm(list=ls())
# setwd("c:/work/MEDMOD/SpatialModelsR/MEDFIRE")  #Nú HP
setwd("d:/MEDMOD/SpatialModelsR/MEDFIRE")   #CTFC
source("mdl/define.scenario.r")
source("mdl/land.dyn.mdl.r")  
# Define scenario
scn.name <- "Scn_pb"
define.scenario(scn.name)

# Change target parameters
nrun <- 1
time.horizon <- 1
write.sp.outputs <- F
fi.accelerate <- 5
clim.scn <- "rcp85"
file.clim.severity <- "ClimaticSeverity"
file.pctg.hot.days <- "PctgHotDays_rcp85"
file.sprd.weight <- "WeightSprdFactors"
processes <- c(TRUE,   # 1. Climate change
               FALSE,  # 2. Land-cover changes
               FALSE,  # 3. Forest management
               TRUE,   # 4. Wildfires
               TRUE,   # 5. Prescribed burns
               FALSE,  # 6. Drought
               TRUE,   # 7. Post-fire regeneration
               FALSE,  # 8. Cohort establihsment
               FALSE,  # 9. Afforestation
               TRUE)   # 10. Growth
dump(c("clim.scn", "file.pctg.hot.days", "file.clim.severity", "fi.accelerate", 
       "nrun", "write.sp.outputs", "time.horizon", "processes", "file.sprd.weight"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
system.time(land.dyn.mdl(scn.name))


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



