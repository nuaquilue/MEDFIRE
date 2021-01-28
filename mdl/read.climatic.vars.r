######################################################################################
###
######################################################################################

read.climatic.vars <- function(){
  
  library(sp)
  library(raster)
  library(tidyverse)
  
  ## Mask of the study area
  load("inputlyrs/rdata/mask.rdata")
  
  ## Dades climàtiques del INFORMED pel període de calibració 1990-1999
  PLAN <- raster("inputlyrs/asc/plan_AEMET_90-99.asc")
  MNAN <- raster("inputlyrs/asc/mnan_AEMET_90-99.asc")
  plan.st <- c(cellStats(PLAN,'mean'), cellStats(PLAN,'sd'))
  mnan.st <- c(cellStats(MNAN,'mean'), cellStats(MNAN,'sd'))
  
  ##
  for(clim.mdl in c("KNMI-RACMO22E_ICHEC-EC-EARTH",
                    "KNMI-RACMO22E_MOHC-HadGEM2-ES",
                    "SMHI-RCA4_CNRM-CERFACS-CNRM-CM5",
                    "SMHI-RCA4_MPI-M-MPI-ESM-LR",
                    "SMHI-RCA4_MOHC-HadGEM2-ES"))
  {
    ## Future climate
    for(clim.scn in c("rcp45", "rcp85")){
      for(decade in seq(10,90,10)){
        print(paste("Building: scenario", clim.scn, "model", clim.mdl, "- decade", decade))

        ## Update annual minimum temp and annual precip
        ## Change resolution and extend to match the default
        TEMP <- raster(paste0("C:/WORK/MEDMOD/DataCLIM/ClimDownscaled/TNMM_", clim.scn, "_", clim.mdl,
                       "_proj", decade, "_1000m.asc"))
        TEMP <- disaggregate(TEMP, fact=c(10,10))
        PRECIP <- raster(paste0("C:/WORK/MEDMOD/DataCLIM/ClimDownscaled/PRCPTOT_", clim.scn, "_", clim.mdl,
                                "_proj", decade, "_1000m.asc"))
        PRECIP <- disaggregate(PRECIP, fact=c(10,10))

        ## Build a data frame with MASK, TEMP and PRECIP and keep only cells from CAT
        clim <- data.frame(cell.id=1:ncell(MASK), mask=MASK[], temp=TEMP[], precip=PRECIP[])
        clim  <-  clim[!is.na(clim$mask),]
        clim$temp <-  (clim$temp-mnan.st[1])/mnan.st[2]
        clim$precip <-  (clim$precip-plan.st[1])/plan.st[2]
        clim <- select(clim, cell.id, temp, precip)
        save(clim, file=paste0("inputlyrs/rdata/climate_", clim.scn, "_", clim.mdl, "_", decade, ".rdata"))
      }
    }

    ## Historical climate
    print(paste("Building: scenario historic", "model", clim.mdl))
    TEMP <- raster(paste0("C:/WORK/MEDMOD/DataCLIM/ClimDownscaled/TNMM_", clim.mdl, "_Hist19712000_1000m.asc"))
    TEMP <- disaggregate(TEMP, fact=c(10,10))
    PRECIP <- raster(paste0("C:/WORK/MEDMOD/DataCLIM/ClimDownscaled/PRCPTOT_", clim.mdl, "_Hist19712000_1000m.asc"))
    PRECIP <- disaggregate(PRECIP, fact=c(10,10))
    clim <- data.frame(cell.id=1:ncell(MASK), mask=MASK[], temp=TEMP[], precip=PRECIP[])
    clim  <-  clim[!is.na(clim$mask),]
    clim$temp <-  (clim$temp-mnan.st[1])/mnan.st[2]
    clim$precip <-  (clim$precip-plan.st[1])/plan.st[2]
    clim <- select(clim, cell.id, temp, precip)
    save(clim, file=paste0("inputlyrs/rdata/climate_hist_", clim.mdl, ".rdata"))
  } 
  

}


