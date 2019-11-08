######################################################################################3
##
######################################################################################3


read.sp.dyn.vars <- function(){
  
  library(raster)
  
  print("Reading initial state variables")
  
  ## To not copy all .asc layers in the inputlyrs folder of MEDFIRE in SpatialModelsR, I read them
  ## directly form the MEDFIRE_II folder in SpatialModels
  mdl.path <- "C:/WORK/MEDMOD/SpatialModels/MEDFIRE_II"
  
  ## Read initial state vars
  LCF <- raster(paste0(mdl.path, "/inputlyrs/asc/LCFspp10_100m.asc"))
  BIOMASS <- raster(paste0(mdl.path, "/inputlyrs/asc/Biomass10.10_100m.asc"))
  AGE <- raster(paste0(mdl.path, "/inputlyrs/asc/VegetAge10_100m.asc"))
  TSDIST <- raster(paste0(mdl.path, "/inputlyrs/asc/TSDisturb10_100m.asc"))
  
  ## Build data frame
  land <- data.frame(cell.id=1:ncell(LCF), spp=LCF[], biom=BIOMASS[], age=AGE[], tsdist=TSDIST[])
  land <- land[!is.na(land$spp),]
  land$distype <- NA; land$distype[land$spp<=17] <- 0
  
  save(land, file="inputlyrs/rdata/land.rdata")
   
}