

read.climatic.vars <- function(){
  
  library(raster)
  library(tidyverse)
  
  ## Mask of the study area
  load("inputlyrs/rdata/mask.rdata")
  
  ## Path of .asc rasters
  mdl.path <- "C:/WORK/MEDMOD/SpatialModels/MEDFIRE_II"
  
  ## List the name of the forest species
  species <- c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster", "puncinata",
               "aalba", "qilex", "qsuber", "qfaginea", "qhumilis", "fsylvatica", "other")
  
  for(clim.scn in c("RCP45", "RCP85")){
    for(decade in seq(10,90,10)){
      
      print(paste("Building:", clim.scn, "-", decade))
      
      ## Update temp and precip
      TEMP <- raster(paste0(mdl.path, "/inputlyrs/asc/TempMinAnnual_", clim.scn, "_", decade, "_100m.asc"))
      PRECIP <- raster(paste0(mdl.path, "/inputlyrs/asc/PrecipAccumAnnual_", clim.scn, "_", decade, "_100m.asc"))
      
      ## Build a data frame with MASK, TEMP, PRECIP and SDMs per spp
      clim <- data.frame(cell.id=1:ncell(MASK), mask=MASK[], temp=TEMP[], precip=PRECIP[])
      for(spp in species){
        SDM <- raster(paste0(mdl.path, "/inputlyrs/asc/SDM_", spp, "_", decade,"_100m.asc"))
        clim <- cbind(clim, data.frame(spp=SDM[]))
        names(clim)[ncol(clim)] <- spp
      }
      
      ## Save SDM of all spp in a data.frame, for cells in CAT
      sdm <- clim[!is.na(clim$mask),]
      sdm <- select(sdm, -mask, -temp, -precip) 
      names(sdm)[-1] <- paste0("sdm.",species)
      sdm$sdm.shrub <- 1
      save(sdm, file=paste0("inputlyrs/rdata/sdm_", clim.scn, "_", decade, ".rdata"))
      
      ## Now, again, keeponly cells from CAT
      clim  <-  clim[!is.na(clim$mask),]
      clim <- select(clim, cell.id, temp, precip)
      save(clim, file=paste0("inputlyrs/rdata/climate_", clim.scn, "_", decade, ".rdata"))
      
    } # decade
  } # clim.scn
  
  
}
