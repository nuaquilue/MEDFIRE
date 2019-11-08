############################ Debugging update.clim.r ############################

rm(list=ls())
setwd("C:/WORK/MEDMOD/SpatialModelsR/MEDFIRE")
scn.name <- "Test01"
## load parameters
source(paste0("outputs/", scn.name, "/scn.def.r"))
## load state variables
source("mdl/read.sp.dyn.vars.r")
land <- read.sp.dyn.vars()


decade <- 10
