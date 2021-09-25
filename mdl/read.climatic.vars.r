######################################################################################
###
######################################################################################

read.climatic.vars <- function(){
  
  library(sp)
  library(raster)
  library(tidyverse)
  
  ## Mask of the study area
  load("inputlyrs/rdata/mask.rdata")
  path = "C:/WORK/onedrive - ctfc.cat/MEDMOD/DataCLIM/"
  
  # ## Dades climàtiques del INFORMED pel període de calibració 1990-1999
  # PLAN <- raster("inputlyrs/asc/plan_AEMET_90-99.asc")
  # MNAN <- raster("inputlyrs/asc/mnan_AEMET_90-99.asc")
  # plan.st <- c(cellStats(PLAN,'mean'), cellStats(PLAN,'sd'))
  # mnan.st <- c(cellStats(MNAN,'mean'), cellStats(MNAN,'sd'))
  
  ##
  for(clim.mdl in c("KNMI-RACMO22E_ICHEC-EC-EARTH",
                    "KNMI-RACMO22E_MOHC-HadGEM2-ES",
                    "SMHI-RCA4_CNRM-CERFACS-CNRM-CM5",
                    "SMHI-RCA4_MPI-M-MPI-ESM-LR",
                    "SMHI-RCA4_MOHC-HadGEM2-ES"))
  {
    
    ## Load historical climate: Minimum temperature and annual precipitation
    cat(paste("Building: scenario historic", "model", clim.mdl, "\n"))
    TMIN <- raster(paste0(path, "ClimDownscaled/TNMM_", clim.mdl, "_Hist19712000_1000m.asc"))
    TMIN <- disaggregate(TMIN, fact=c(10,10))
    tmin.st <- c(cellStats(TMIN,'mean'), cellStats(TMIN,'sd'))
    TMAX <- raster(paste0(path, "ClimDownscaled/TXMM_", clim.mdl, "_Hist19712000_1000m.asc"))
    TMAX <- disaggregate(TMAX, fact=c(10,10))
    tmax.st <- c(cellStats(TMAX,'mean'), cellStats(TMAX,'sd'))
    PRECIP <- raster(paste0(path, "ClimDownscaled/PRCPTOT_", clim.mdl, "_Hist19712000_1000m.asc"))
    PRECIP <- disaggregate(PRECIP, fact=c(10,10))
    precip.st <- c(cellStats(PRECIP,'mean'), cellStats(PRECIP,'sd'))
    
    ## Build data frame with climatic data
    clim <- data.frame(cell.id=1:ncell(MASK), mask=MASK[], tmin=TMIN[], tmax=TMAX[], precip=PRECIP[])
    clim  <-  clim[!is.na(clim$mask),]
    clim$tmin <-  (clim$tmin-tmin.st[1])/tmin.st[2]
    clim$tmax <-  (clim$tmax-tmax.st[1])/tmax.st[2]
    clim$precip <-  (clim$precip-precip.st[1])/precip.st[2]
    clim <- select(clim, cell.id, tmin, tmax, precip)
    save(clim, file=paste0("inputlyrs/rdata/climate_hist_", clim.mdl, ".rdata"))
    
    ## Future climate
    for(clim.scn in c("rcp45", "rcp85")){
      for(decade in seq(10,90,10)){
        
        cat(paste("Building: scenario", clim.scn, "model", clim.mdl, "- decade", decade), "\n")

        ## Update annual minimum temp and annual precip
        ## Change resolution and extend to match the default
        TMIN <- raster(paste0(path, "ClimDownscaled/TNMM_", clim.scn, "_", clim.mdl,
                       "_proj", decade, "_1000m.asc"))
        TMIN <- disaggregate(TMIN, fact=c(10,10))
        TMAX <- raster(paste0(path, "ClimDownscaled/TXMM_", clim.scn, "_", clim.mdl,
                              "_proj", decade, "_1000m.asc"))
        TMAX <- disaggregate(TMAX, fact=c(10,10))
        PRECIP <- raster(paste0(path, "ClimDownscaled/PRCPTOT_", clim.scn, "_", clim.mdl,
                                "_proj", decade, "_1000m.asc"))
        PRECIP <- disaggregate(PRECIP, fact=c(10,10))

        ## Build a data frame with MASK, TEMP and PRECIP and keep only cells from CAT
        clim <- data.frame(cell.id=1:ncell(MASK), mask=MASK[], tmin=TMIN[], tmax=TMAX[], precip=PRECIP[])
        clim  <-  clim[!is.na(clim$mask),]
        clim$tmin <-  (clim$tmin-tmin.st[1])/tmin.st[2]
        clim$tmax <-  (clim$tmax-tmax.st[1])/tmax.st[2]
        clim$precip <-  (clim$precip-precip.st[1])/precip.st[2]
        clim <- select(clim, cell.id, tmin, tmax, precip)
        save(clim, file=paste0("inputlyrs/rdata/climate_", clim.scn, "_", clim.mdl, "_", decade, ".rdata"))
      }
    }
  } 
  

}


