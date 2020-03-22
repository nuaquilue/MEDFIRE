######################################################################################
###
######################################################################################

read.climatic.vars <- function(work.path){
  
  library(sp)
  library(raster)
  library(tidyverse)
  
  ## Mask of the study area
  load(paste0(work.path, "/inputlyrs/rdata/mask.rdata"))
  
  ## 
  for(clim.scn in c("rcp45", "rcp85")){
    for(clim.mdl in c("KNMI-RACMO22E_ICHEC-EC-EARTH",
                      "KNMI-RACMO22E_MOHC-HadGEM2-ES",
                      "SMHI-RCA4_CNRM-CERFACS-CNRM-CM5",
                      "SMHI-RCA4_MPI-M-MPI-ESM-LR",
                      "SMHI-RCA4_MOHC-HadGEM2-ES")){
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
        clim <- select(clim, cell.id, temp, precip)
        save(clim, file=paste0(work.path, "/inputlyrs/rdata/climate_", clim.scn, "_", clim.mdl, "_", decade, ".rdata"))
      }  
    }
  } 
}



################################ INITIAL SDM AND SQI ################################
######### SQI shurb to calculate initial biomass as function of TSF ##################
hist.clim <- function(clim.scn){

  cat(paste("Building: historic scenario with", clim.scn, "/n"))
  
  library(sp)
  library(raster)
  library(tidyverse)

  ## Mask of the study area and land data frame
  load(paste0(work.path, "/inputlyrs/rdata/mask.rdata"))
  load(paste0(work.path, "/inputlyrs/rdata/land.rdata"))
  load(paste0(work.path, "/inputlyrs/rdata/orography.rdata"))
    
  ## Read coefficients of site quality
  site.quality.spp <- read.table("inputfiles/SiteQualitySpp.txt", header=T)
  site.quality.index <- read.table("inputfiles/SiteQualityIndex.txt", header=T)
  site.quality.shrub <- read.table("inputfiles/SiteQualityShrub.txt", header=T)

  ## Read historical annual minimum temp and annual precip
  ## Change resolution and extend to match the default
  TEMP <- raster(paste0("C:/WORK/MEDMOD/DataCLIM/ClimDownscaled/TNMM_", clim.scn, "_Hist19712000_1000m.asc"))
  TEMP <- disaggregate(TEMP, fact=c(10,10))
  PRECIP <- raster(paste0("C:/WORK/MEDMOD/DataCLIM/ClimDownscaled/PRCPTOT_", clim.scn, "_Hist19712000_1000m.asc"))
  PRECIP <- disaggregate(PRECIP, fact=c(10,10))
  
  ## Build a data frame with MASK, TEMP and PRECIP and keep only cells from CAT
  ## Join land.cover.spp, aspect and slope data
  clim <- data.frame(cell.id=1:ncell(MASK), mask=MASK[], temp=TEMP[], precip=PRECIP[]) %>%
          filter(!is.na(mask)) %>% left_join(select(land, cell.id, spp), by="cell.id") %>%
          left_join(orography, by="cell.id")
  
  ## Compute SQ and SQI
  clim <- select(clim, cell.id, spp, temp, precip, aspect, slope) %>% 
          left_join(site.quality.spp, by="spp") %>% left_join(site.quality.index, by="spp") %>% 
          mutate(aux=c0+c_mnan*temp+c2_mnan*temp*temp+c_plan*precip+c2_plan*precip*precip+c_aspect*ifelse(aspect!=1,0,1)+c_slope*slope/10) %>%
          mutate(sq=1/(1+exp(-1*aux))) %>% mutate(sqi=ifelse(sq<=p50, 1, ifelse(sq<=p90, 2, 3))) %>%
          select(cell.id, spp, temp, precip, sqi)
  
  ## SQI for shrubs
  sqi.shrub <- filter(clim, spp==14) %>% select(spp, temp, precip) %>% left_join(site.quality.shrub, by="spp") %>%
               mutate(aux.brolla=c0_brolla+c_temp_brolla*temp+c_temp2_brolla*temp*temp+c_precip_brolla*precip+c_precip2_brolla*precip*precip,
               aux.maquia=c0_maquia+c_temp_maquia*temp+c_temp2_maquia*temp*temp+c_precip_maquia*precip+c_precip2_maquia*precip*precip,
               aux.boix=c0_boix+c_temp_boix*temp+c_temp2_boix*temp*temp+c_precip_boix*precip+c_precip2_boix*precip*precip,
               sq.brolla=1/(1+exp(-1*aux.brolla)), sq.maquia=1/(1+exp(-1*aux.maquia)), sq.boix=1/(1+exp(-1*aux.boix)),
               sqest.brolla=scale(sq.brolla), sqest.maquia=scale(sq.maquia), sqest.boix=scale(sq.boix),
               sqi=ifelse(sqest.brolla>=sqest.maquia & sqest.brolla>=sqest.boix, 1,
                      ifelse(sqest.maquia>=sqest.brolla & sqest.maquia>=sqest.boix, 2,
                             ifelse(sqest.boix>=sqest.brolla & sqest.boix>=sqest.maquia, 3, 0))) )
  clim$sqi[clim$spp==14] <- sqi.shrub$sqi
  
  sqi <- select(clim, cell.id, sqi)
  names(sqi)[2] <- "x"
  save(sqi, file=paste0(work.path, "/inputlyrs/rdata/sqi.rdata"))
}
