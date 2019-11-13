######################################################################################3
## Build 3 rdata with Study area mask, Coordinates and static spatial variables
######################################################################################3

read.static.vars <- function(){
  
  library(raster)
  library(tidyverse)
  
  print("Reading orographyic variables")
  
  ## To not copy all .asc layers in the inputlyrs folder of MEDFIRE in SpatialModelsR, I read them
  ## directly form the MEDFIRE_II folder in SpatialModels
  mdl.path <- "C:/WORK/MEDMOD/SpatialModels/MEDFIRE_II"
  
  ## MASK of the study area
  MASK <- raster(paste0(mdl.path, "/inputlyrs/asc/LCFspp10_100m.asc"))
  MASK[!is.na(MASK[])] <- 1
  save(MASK, file="inputlyrs/rdata/mask.rdata") 
  
  ## Build a coordinates data frame and then load model static variables in a data.frame format
  coord <- data.frame(cell.id=1:ncell(MASK), coordinates(MASK), mask=MASK[])
  coord <- filter(coord, !is.na(mask)) %>% select(-mask)
  save(coord, file="inputlyrs/rdata/coordinates.rdata") 
  
  ## Read initial state vars
  ELEVATION <- raster(paste0(mdl.path, "/inputlyrs/asc/DEM_100m.asc"))
  ASPECT <- raster(paste0(mdl.path, "/inputlyrs/asc/Aspect_100m.asc"))
  SLOPE <- raster(paste0(mdl.path, "/inputlyrs/asc/SlopeDegree_100m.asc"))
  SOLAR <- raster(paste0(mdl.path, "/inputlyrs/asc/SolarRad_100m.asc"))
  ROAD <- raster(paste0(mdl.path, "/inputlyrs/asc/DensRoad_100m.asc"))
  DIST.PATH <- raster(paste0(mdl.path, "/inputlyrs/asc/DistPath_100m.asc"))
  SLOPE.PCTG <- raster(paste0(mdl.path, "/inputlyrs/asc/SlopePctg_100m.asc"))
  
  ## Build data frame
  orography <- data.frame(cell.id=1:ncell(MASK), elev=ELEVATION[], aspect=ASPECT[], slope=SLOPE[], 
                          solar=SOLAR[], road=ROAD[], dist.path=DIST.PATH[], slope.pctg=SLOPE.PCTG[])
  orography <- orography[!is.na(MASK[]),]
  save(orography, file="inputlyrs/rdata/orography.rdata")
  
}