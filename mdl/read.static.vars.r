######################################################################################
## Build 5 .rdata with
## 1. Raster MASK of the study area 
## 2. Coordinates of the study area
## 3. Orographic variables
## 4. UTM grid as a data frame
## 5. Fire regime related variables
######################################################################################

read.static.vars <- function(){
  
  library(raster)
  library(RANN)
  library(tidyverse)
  
  cat("Reading orographyic, utm, fire regime variables, harvesting-restrictions", "\n")
  
  ## MASK of the study area
  MASK <- raster("inputlyrs/asc/LCFspp_100m_31N-ETRS89.asc")
  MASK[!is.na(MASK[])] <- 1
  crs(MASK) <- CRS("+init=epsg:25831")
  save(MASK, file="inputlyrs/rdata/mask.rdata") 
  
  ## Build a coordinates data frame and then load model static variables in a data.frame format
  coord <- data.frame(cell.id=1:ncell(MASK), coordinates(MASK), mask=MASK[])
  coord <- filter(coord, !is.na(mask)) %>% select(-mask)
  save(coord, file="inputlyrs/rdata/coordinates.rdata") 
  
  ## Read initial state vars,  build and save the data frame
  ELEVATION <- raster("inputlyrs/asc/DEM_100m_31N-ETRS89.asc")
  ASPECT <- raster("inputlyrs/asc/Aspect_100m_31N-ETRS89.asc")
  SLOPE <- raster("inputlyrs/asc/SlopeDegree_100m_31N-ETRS89.asc")
  ROAD <- raster("inputlyrs/asc/DensRoad_100m_31N-ETRS89.asc")
  ROAD[is.na(ROAD[])] <- 0  ## why there are so many NA is ROAD layer?
  orography <- data.frame(cell.id=1:ncell(MASK), elev=ELEVATION[], aspect=ASPECT[], slope=SLOPE[], road=ROAD[])
  orography <- orography[!is.na(MASK[]),]
  slope.st <- c(cellStats(SLOPE,'mean'), cellStats(SLOPE,'sd'))
  orography$slope.stand <- (orography$slope-slope.st[1])/slope.st[2]
  save(orography, file="inputlyrs/rdata/orography.rdata")
  
  ## UTM layer
  UTM <- raster(paste0(work.path, "/inputlyrs/asc/UTM1k_100m_31N-ETRS89.asc"))
  ## Are there NAs in the UTM layer within CAT?
  dta <- data.frame(cell.id=1:ncell(UTM), m=MASK[], coordinates(UTM), z=UTM[]) %>% filter(!is.na(m))
  na.var <- filter(dta, is.na(z))
  for(id in na.var$cell.id){
    neighs <- nn2(select(dta, x, y), filter(na.var, cell.id==id)%>% select(x,y), 
                  searchtype="priority", k=9)
    phago <- mean(dta$z[neighs$nn.idx], na.rm=T)
    if(!is.na(phago))
      na.var$z[na.var$cell.id==id] <- phago
  }
  na.var2 <- filter(na.var, is.na(z))
  dta$z[is.na(dta$z)] <- na.var$z
  UTM[!is.na(MASK[])] <- dta$z
  utm <- data.frame(cell.id=1:ncell(UTM),  utm=UTM[])
  save(utm, file="inputlyrs/rdata/utm.rdata")
  
  ## Layers for forest management
  DIST.PATH <- raster(paste0(work.path, "/inputlyrs/asc/DistPath_100m_31N-ETRS89.asc"))
  SLOPE.PCTG <- raster(paste0(work.path, "/inputlyrs/asc/SlopePctg_100m_31N-ETRS89.asc"))
  ENPE <- raster(paste0(work.path, "/inputlyrs/asc/ENPE_100m_31N-ETRS89.asc"))
  DIST.INDUSTRY <- raster(paste0(work.path, "/inputlyrs/asc/DistIndustry_100m_31N-ETRS89.asc"))
  DIST.BIOMASS <- raster(paste0(work.path, "/inputlyrs/asc/DistBiomass_100m_31N-ETRS89.asc"))
  # NAs in the SLOPE.PTCG layer within CAT?
  dta <- data.frame(cell.id=1:ncell(SLOPE.PCTG), m=MASK[], coordinates(SLOPE.PCTG), z=SLOPE.PCTG[]) %>% filter(!is.na(m))
  na.var <- filter(dta, is.na(z))
  for(id in na.var$cell.id){
    neighs <- nn2(select(dta, x, y), filter(na.var, cell.id==id)%>% select(x,y), 
                  searchtype="priority", k=9)
    phago <- mean(dta$z[neighs$nn.idx], na.rm=T)
    if(!is.na(phago))
      na.var$z[na.var$cell.id==id] <- phago
  }
  dta$z[is.na(dta$z)] <- na.var$z
  na.var2 <- filter(na.var, is.na(z))
  for(id in na.var2$cell.id){
    neighs <- nn2(select(dta, x, y), filter(na.var2, cell.id==id)%>% select(x,y), 
                  searchtype="priority", k=25)
    phago <- mean(dta$z[neighs$nn.idx], na.rm=T)
    if(!is.na(phago))
      na.var2$z[na.var2$cell.id==id] <- phago
  }
  dta$z[is.na(dta$z)] <- na.var2$z
  SLOPE.PCTG[!is.na(MASK[])] <- dta$z
  harvest <- data.frame(cell.id=1:ncell(MASK), enpe=ENPE[], dist.path=DIST.PATH[], 
                        slope.pctg=SLOPE.PCTG[], dist.industry=round(DIST.INDUSTRY[]/10^3),
                        dist.biomass=round(DIST.BIOMASS[]/10^3))  # dist to industry and to biomass in km
  harvest <- harvest[!is.na(MASK[]),]
  harvest$enpe[is.na(harvest$enpe)] <- 0
  save(harvest, file="inputlyrs/rdata/harvest.rdata")
  
  ## Layers for fire
  IGNI.TOPO <- raster("inputlyrs/asc/IgniTopo_100m_31N-ETRS89.asc")
  IGNI.WIND <- raster("inputlyrs/asc/IgniWind_100m_31N-ETRS89.asc")
  PWIND.N <- raster("inputlyrs/asc/ProbN_100m_31N-ETRS89.asc")
  PWIND.NW <- raster("inputlyrs/asc/ProbNW_100m_31N-ETRS89.asc")
  PWIND.W <- raster("inputlyrs/asc/ProbW_100m_31N-ETRS89.asc")
  pfst.pwind <- data.frame(cell.id=1:ncell(MASK), pfst.wind=IGNI.WIND[], pfst.topo=IGNI.TOPO[],
                           pwind.n=PWIND.N[], pwind.nw=PWIND.NW[], pwind.w=PWIND.W[])
  pfst.pwind <- pfst.pwind[!is.na(MASK[]),]
  pfst.pwind$pwind.n[is.na(pfst.pwind$pwind.n)] <- 0
  pfst.pwind$pwind.nw[is.na(pfst.pwind$pwind.nw)] <- 0
  pfst.pwind$pwind.w[is.na(pfst.pwind$pwind.w)] <- 0
  save(pfst.pwind, file="inputlyrs/rdata/pfst.pwind.rdata")
}


