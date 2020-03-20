######################################################################################
###  read.climatic.vars()
###
### L'ordre d'espècies segueix sent:
### 1"Pinus sylvestris",2"Pinus uncinata",3"Pinus pinea",4"Pinus halepensis",5"Pinus nigra",
### 6"Pinus pinaster",7"Abies alba",8"Quercus faginea",9"Quercus ilex",10"Quercus suber",
### 11"Quercus humilis",12"Fagus sylvatica",13"OTrees."
######################################################################################

## TO BE RUN ONCE WE HAVE NEW CLIMATIC LAYERS AT UTM31N - ETRS89

read.climatic.vars <- function(work.path){
  
  library(sp)
  library(raster)
  library(tidyverse)
  
  ## Mask of the study area
  load(paste0(work.path, "/inputlyrs/rdata/mask.rdata"))
  
  ## List the name of the forest species
  species <- c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster", "puncinata",
               "aalba", "qilex", "qsuber", "qfaginea", "qhumilis", "fsylvatica", "other")
  order.spp.sdm <- c(4,5,3,1,6,2,7,9,10,8,11,12,13)
    
  ## Default extent of raster maps of Catalonia  
  extCat <- extent(c(250000, 540000, 4480000, 4760000))
  
  for(clim.scn in c("rcp45", "rcp85")){
    for(decade in seq(10,90,10)){ 
      
      cat(paste("Building: scenario", clim.scn, "- decade", decade), "/n")
      
      ## Update annual minimum temp and annual precip
      ## Change resolution and extend to match the default
      TEMP <- raster(paste0(work.path, "/inputlyrs/asc/", clim.scn, "/", decade, "/mnan.asc"))
      TEMP <- disaggregate(TEMP, fact=c(10,10))
      TEMP <- extend(TEMP, extCat)
      PRECIP <- raster(paste0(work.path, "/inputlyrs/asc/", clim.scn, "/", decade, "/plan.asc"))
      PRECIP <- disaggregate(PRECIP, fact=c(10,10))
      PRECIP <- extend(PRECIP, extCat)
      
      ## Build a data frame with MASK, TEMP and PRECIP 
      ## And keep only cells from CAT
      clim <- data.frame(cell.id=1:ncell(MASK), mask=MASK[], temp=TEMP[], precip=PRECIP[])
      clim  <-  clim[!is.na(clim$mask),]
      clim <- select(clim, cell.id, temp, precip)
      save(clim, file=paste0(work.path, "/inputlyrs/rdata/climate_", clim.scn, "_", decade, ".rdata"))
      
      for(p in c(1)){

        cat(paste("Threshold", p), "/n")

        ## Update list of SDMs
        ## Change resolution (1 km, even if the name is "_100m") and extend to match the default
        ## Build a data frame with MASK and SDMs per spp
        load(paste0(work.path, "/inputlyrs/asc/sdm/SDM_p", p, "_", clim.scn, "_", decade, "_100m.rdata"))
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
        save(sdm, file=paste0(work.path, "/inputlyrs/rdata/sdm_", p, "p_", clim.scn, "_", decade, ".rdata"))

      } #p
    } # decade
  } # clim.scn
  
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

  PRCPTOT_SMHI-RCA4_MOHC-HadGEM2-ES_Hist19712000_1000m
  
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
