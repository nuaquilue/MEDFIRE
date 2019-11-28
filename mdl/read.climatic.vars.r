######################################################################################
###  read.climatic.vars()
###
######################################################################################

read.climatic.vars <- function(){
  
  library(raster)
  library(RANN)  # for nn2()
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
      
      print(paste("Building:", clim.scn, "-", decade))
      
      ## Update annual minimum temp and annual precip
      ## Change resolution and extend to match the default
      TEMP <- raster(paste0("inputlyrs/asc/", clim.scn, "/", decade, "/mnan.asc"))
      TEMP <- disaggregate(TEMP, fact=c(10,10))
      TEMP <- extend(TEMP, extCat)
      PRECIP <- raster(paste0("inputlyrs/asc/", clim.scn, "/", decade, "/plan.asc"))
      PRECIP <- disaggregate(PRECIP, fact=c(10,10))
      PRECIP <- extend(PRECIP, extCat)
      
      ## Update list of SDMs
      ## Change resolution and extend to match the default
      load(paste0("inputlyrs/rdata/SDM_", clim.scn, "_", decade, "_100m.rdata"))
      for(i in 1:length(sdm.proj)){
        sdm.proj[[i]] <- disaggregate(sdm.proj[[i]], fact=c(10,10))
        sdm.proj[[i]] <- extend(sdm.proj[[i]], extCat)
      }
        
      ## Build a data frame with MASK, TEMP, PRECIP and SDMs per spp
      clim <- data.frame(cell.id=1:ncell(MASK), mask=MASK[], temp=TEMP[], precip=PRECIP[])
      for(i in order.spp.sdm)
        clim <- cbind(clim, data.frame(spp=sdm.proj[[i]][]))
      names(clim)[5:17] <- species
      
      ## Save SDM of all spp in a data.frame, for cells in CAT
      sdm <- clim[!is.na(clim$mask),]
      sdm <- select(sdm, -mask, -temp, -precip) 
      names(sdm)[-1] <- paste0("sdm.",species)
      sdm$sdm.shrub <- 1
      save(sdm, file=paste0("inputlyrs/rdata/sdm_", clim.scn, "_", decade, ".rdata"))
      
      ## Now, again, keep only cells from CAT
      clim  <-  clim[!is.na(clim$mask),]
      clim <- select(clim, cell.id, temp, precip)
      save(clim, file=paste0("inputlyrs/rdata/climate_", clim.scn, "_", decade, ".rdata"))
      
    } # decade
  } # clim.scn
  
  
}
