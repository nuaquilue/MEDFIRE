######################################################################################3
## Build 3 rdata with Study area mask, Coordinates and static spatial variables
######################################################################################3

read.static.vars <- function(){
  
  library(raster)
  library(tidyverse)
  
  print("Reading orographyic, utm, harvest-restriction variables")
  
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
  
  ## Read initial state vars,  build and save the data frame
  ELEVATION <- raster(paste0(mdl.path, "/inputlyrs/asc/DEM_100m.asc"))
  ASPECT <- raster(paste0(mdl.path, "/inputlyrs/asc/Aspect_100m.asc"))
  SLOPE <- raster(paste0(mdl.path, "/inputlyrs/asc/SlopeDegree_100m.asc"))
  SOLAR <- raster(paste0(mdl.path, "/inputlyrs/asc/SolarRad_100m.asc"))
  ROAD <- raster(paste0(mdl.path, "/inputlyrs/asc/DensRoad_100m.asc"))
  orography <- data.frame(cell.id=1:ncell(MASK), elev=ELEVATION[], aspect=ASPECT[], slope=SLOPE[], 
                          solar=SOLAR[], road=ROAD[])
  orography <- orography[!is.na(MASK[]),]
  save(orography, file="inputlyrs/rdata/orography.rdata")
  
  ## UTM layer
  UTM <- raster(paste0(mdl.path, "/inputlyrs/asc/UTM1k_100m.asc"))
  utm <- data.frame(cell.id=1:ncell(UTM),  utm=UTM[])
  save(utm, file="inputlyrs/rdata/utm.rdata")
  
  ## Layers for forest management
  DIST.PATH <- raster(paste0(mdl.path, "/inputlyrs/asc/DistPath_100m.asc"))
  SLOPE.PCTG <- raster(paste0(mdl.path, "/inputlyrs/asc/SlopePctg_100m.asc"))
  PROTECT.AREA <- raster(paste0(mdl.path, "/inputlyrs/asc/ENPE_100m.asc"))
  type.protect.area <- foreign::read.dbf("c:/work/medmod/inputlayers_MEDFIRE_II/protectareas/enpe.dbf")
  protect.area <- as.data.frame(PROTECT.AREA[]); names(protect.area) <- "VALUE"
  protect.area <- left_join(protect.area, select(type.protect.area, VALUE, TYPE), by="VALUE")
  harvest <- data.frame(protect.area=protect.area$TYPE, dist.path=DIST.PATH[], slope.pctg=SLOPE.PCTG[])
  harvest <- harvest[!is.na(MASK[]),]
  save(harvest, file="inputlyrs/rdata/harvest.rdata")
  
  ## Layers for fire
  mdl.path <- "C:/WORK/MEDMOD/SpatialModels/MEDFIRE.v8"
  IGNI.WIND <- raster(paste0(mdl.path, "/inputlyrs/asc/IgniWind_100m.asc"))
  IGNI.TOPO <- raster(paste0(mdl.path, "/inputlyrs/asc/IgniTopo_100m.asc"))
  PWIND.N <- raster(paste0(mdl.path, "/inputlyrs/asc/ProbN_100m.asc"))
  PWIND.NW <- raster(paste0(mdl.path, "/inputlyrs/asc/ProbNW_100m.asc"))
  PWIND.W <- raster(paste0(mdl.path, "/inputlyrs/asc/ProbW_100m.asc"))
  pfst.pwind <- data.frame(pfst.wind=IGNI.WIND[], pfst.topo=IGNI.TOPO[],
                           pwind.n=PWIND.N[], pwind.nw=PWIND.NW[], pwind.w=PWIND.W[])
  pfst.pwind <- pfst.pwind[!is.na(MASK[]),]
  save(pfst.pwind, file="inputlyrs/rdata/pfst.pwind.rdata")
}


