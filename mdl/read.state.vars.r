######################################################################################
## Build land.rdata with the initialization of the 6 state variables:
## Land-cover / Forest species
## Biomass (m2/ha x 10 for forest, kg/ha * 10 for shrub)
## Forest species age 
## Time since last disturbance
## Type of last disturbance
######################################################################################

read.state.vars <- function(work.path){
  
  library(raster)
  
  cat("Reading initial state variables", "\n")
  
  ## Read initial state vars
  LCF <- raster(paste0(work.path, "/inputlyrs/asc/LCFM2010_100m_31N-ETRS89.asc"))
  BIOMASS <- raster(paste0(work.path, "/inputlyrs/asc/Biomass2010x10_100m_31N-ETRS89.asc"))
  AGE <- raster("inputlyrs/asc/ForestAge2010_31N-ETRS89.asc")
  TSDIST <- raster(paste0(work.path, "/inputlyrs/asc/TSDisturb10_100m_31N-ETRS89.asc"))
  
  ## Build data frame
  land <- data.frame(cell.id=1:ncell(LCF), spp=LCF[], biom=BIOMASS[], age=AGE[], tsdist=TSDIST[])
  land <- land[!is.na(land$spp),]
  land$distype <- NA; land$distype[land$spp<=17] <- 0
  land$tburnt <- land$distype
  ## Mark that cells were previously burnt in the disturbance type layer,
  ## So, it's in concordance with TSDIST layer
  land$tsdist[land$tsdist<200] <- hfire
  
  save(land, file="inputlyrs/rdata/land.rdata")
   
}