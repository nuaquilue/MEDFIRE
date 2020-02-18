######################################################################################
###  read.climatic.vars()
###
### L'ordre d'espècies segueix sent:
### 1"Pinus sylvestris",2"Pinus uncinata",3"Pinus pinea",4"Pinus halepensis",5"Pinus nigra",
### 6"Pinus pinaster",7"Abies alba",8"Quercus faginea",9"Quercus ilex",10"Quercus suber",
### 11"Quercus humilis",12"Fagus sylvatica",13"OTrees."
######################################################################################

read.climatic.vars <- function(){
  
  library(raster)
  library(tidyverse)
  
  ## Mask of the study area
  load("inputlyrs/rdata/mask.rdata")
  
  ## List the name of the forest species
  species <- c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster", "puncinata",
               "aalba", "qilex", "qsuber", "qfaginea", "qhumilis", "fsylvatica", "other")
       order.spp.sdm <- c(4,5,3,1,6,2,7,9,10,8,11,12,13)
    
  ## Default extent of raster maps of Catalonia  
  extCat <- extent(c(250000, 540000, 4480000, 4760000))
  
  for(clim.scn in c("rcp45", "rcp85")){
    for(decade in seq(10,90,10)){
      
      cat(paste("Building: scenario", clim.scn, "- decade", decade), "\n")
      
      ## Update annual minimum temp and annual precip
      ## Change resolution and extend to match the default
      TEMP <- raster(paste0("inputlyrs/asc/", clim.scn, "/", decade, "/mnan.asc"))
      TEMP <- disaggregate(TEMP, fact=c(10,10))
      TEMP <- extend(TEMP, extCat)
      PRECIP <- raster(paste0("inputlyrs/asc/", clim.scn, "/", decade, "/plan.asc"))
      PRECIP <- disaggregate(PRECIP, fact=c(10,10))
      PRECIP <- extend(PRECIP, extCat)
      RAD <- raster(paste0("inputlyrs/asc/", clim.scn, "/", decade, "/Radan.asc"))
      RAD <- disaggregate(RAD, fact=c(10,10))
      RAD <- extend(RAD, extCat)
      
      ## Build a data frame with MASK, TEMP, PRECIP and RAD
      ## And keep only cells from CAT
      clim <- data.frame(cell.id=1:ncell(MASK), mask=MASK[], temp=TEMP[], precip=PRECIP[], rad=RAD[])
      clim  <-  clim[!is.na(clim$mask),]
      clim <- select(clim, cell.id, temp, precip, rad)
      save(clim, file=paste0("inputlyrs/rdata/climate_", clim.scn, "_", decade, ".rdata"))
      
      for(p in c(1)){
        
        cat(paste("Threshold", p), "\n")
        
        ## Update list of SDMs
        ## Change resolution and extend to match the default
        ## Build a data frame with MASK and SDMs per spp
        load(paste0("inputlyrs/asc/SDM_", p, "p_", clim.scn, "_", decade, "_1km.rdata"))
        sdm  <- data.frame(cell.id=1:ncell(MASK), mask=MASK[])
        for(i in order.spp.sdm){
           sdm.proj[[i]] <- disaggregate(sdm.proj[[i]], fact=c(10,10))
           sdm.proj[[i]] <- extend(sdm.proj[[i]], extCat)
           sdm <- cbind(sdm, data.frame(spp=sdm.proj[[i]][]))
        }
        names(sdm)[3:15] <- species
        # for(i in 1:13)
        #   sdm <- cbind(sdm, data.frame(spp=sdm.proj[[i]][]))
        
        ## Save SDM of all spp in a data.frame, for cells in CAT
        sdm <- sdm[!is.na(sdm$mask),]
        sdm <- select(sdm, -mask) 
        names(sdm)[-1] <- paste0("sdm.", species)
        sdm$sdm.shrub <- 1
        save(sdm, file=paste0("inputlyrs/rdata/sdm_", p, "p_", clim.scn, "_", decade, ".rdata"))
      
      } #p
    } # decade
  } # clim.scn
  
  
}
