######################################################################################
##
######################################################################################

update.clim <- function(land, orography, decade, clim.scn, clim.mdl){

  ## Tracking
  cat("Update climate", "\n") 

  ## Read coefficients of site quality
  site.quality.spp <- read.table("inputfiles/SiteQualitySpp.txt", header=T)
  site.quality.index <- read.table("inputfiles/SiteQualityIndex.txt", header=T)
  site.quality.shrub <- read.table("inputfiles/SiteQualityShrub.txt", header=T)
  
  ## Load SDM and climate (temp and precip)
  load(paste0("inputlyrs/rdata/sdm_base_", clim.scn, "_", clim.mdl, "_", decade, ".rdata"))
  load(paste0("inputlyrs/rdata/climate_", clim.scn, "_", clim.mdl, "_", decade, ".rdata"))
  
  ## Join land.cover.spp, aspect and slope data
  clim <- left_join(clim, select(land, cell.id, spp), by="cell.id") %>%
          left_join(orography, by="cell.id")
  
  ## Assign SDM according to current spp distribution 
  clim$sdm <- NA
  clim$sdm[clim$spp==1] <- sdm$sdm.phalepensis[clim$spp==1]
  clim$sdm[clim$spp==2] <- sdm$sdm.pnigra[clim$spp==2]
  clim$sdm[clim$spp==3] <- sdm$sdm.ppinea[clim$spp==3]
  clim$sdm[clim$spp==4] <- sdm$sdm.psylvestris[clim$spp==4]
  clim$sdm[clim$spp==5] <- sdm$sdm.ppinaster[clim$spp==5]
  clim$sdm[clim$spp==6] <- sdm$sdm.puncinata[clim$spp==6]
  clim$sdm[clim$spp==7] <- sdm$sdm.aalba[clim$spp==7]
  clim$sdm[clim$spp==8] <- sdm$sdm.qilex[clim$spp==8]
  clim$sdm[clim$spp==9] <- sdm$sdm.qsuber[clim$spp==9]
  clim$sdm[clim$spp==10] <- sdm$sdm.qfaginea[clim$spp==10]
  clim$sdm[clim$spp==11] <- sdm$sdm.qhumilis[clim$spp==11]
  clim$sdm[clim$spp==12] <- sdm$sdm.fsylvatica[clim$spp==12]
  clim$sdm[clim$spp==13] <- sdm$sdm.other[clim$spp==13]
  clim$sdm[clim$spp==14] <- 1  ## SDM of shrub is always 1

  
  ## Compute SQ and SQI
  clim <- select(clim, cell.id, spp, temp, precip, sdm, aspect, slope.stand) %>% 
          left_join(site.quality.spp, by="spp") %>% left_join(site.quality.index, by="spp") %>% 
          mutate(aux=c0+c_mnan*temp+c2_mnan*temp*temp+c_plan*precip+c2_plan*precip*precip+
                 c_aspect*ifelse(aspect!=1,0,1)+c_slope*slope.stand) %>%
          mutate(sq=1/(1+exp(-aux))) %>% mutate(sqi=ifelse(sq<=p50, 1, ifelse(sq<=p90, 2, 3))) %>%
          select(cell.id, spp, temp, precip, sdm, sqi)
  
  ## SQI for shrubs
  sqi.shrub <- filter(clim, spp==14) %>% select(spp, temp, precip) %>% 
               mutate(aux.brolla=site.quality.shrub$c0_brolla+site.quality.shrub$c_temp_brolla*temp+site.quality.shrub$c_temp2_brolla*temp*temp+site.quality.shrub$c_precip_brolla*precip+site.quality.shrub$c_precip2_brolla*precip*precip,
                      aux.maquia=site.quality.shrub$c0_maquia+site.quality.shrub$c_temp_maquia*temp+site.quality.shrub$c_temp2_maquia*temp*temp+site.quality.shrub$c_precip_maquia*precip+site.quality.shrub$c_precip2_maquia*precip*precip,
                      aux.boix=site.quality.shrub$c0_boix+site.quality.shrub$c_temp_boix*temp+site.quality.shrub$c_temp2_boix*temp*temp+site.quality.shrub$c_precip_boix*precip+site.quality.shrub$c_precip2_boix*precip*precip,
                      sq.brolla=1/(1+exp(-1*aux.brolla)), sq.maquia=1/(1+exp(-1*aux.maquia)), sq.boix=1/(1+exp(-1*aux.boix))) %>% 
               mutate(sqest.brolla=sq.brolla/max(sq.brolla), sqest.maquia=sq.maquia/max(sq.maquia), sqest.boix=sq.boix/max(sq.boix),
                      sqi=ifelse(sqest.brolla>=sqest.maquia & sqest.brolla>=sqest.boix, 1,
                            ifelse(sqest.maquia>=sqest.brolla & sqest.maquia>=sqest.boix, 2,
                              ifelse(sqest.boix>=sqest.brolla & sqest.boix>=sqest.maquia, 3, 0))))
  clim$sqi[clim$spp==14] <- sqi.shrub$sqi

  return(clim=clim)
}


hist.clim <- function(land, orography, clim.mdl){
  
  cat("Historic climate", "\n")
  
  ## Read coefficients of site quality
  site.quality.spp <- read.table("inputfiles/SiteQualitySpp.txt", header=T)
  site.quality.index <- read.table("inputfiles/SiteQualityIndex.txt", header=T)
  site.quality.shrub <- read.table("inputfiles/SiteQualityShrub.txt", header=T)
  
  ## Load SDM and climate (temp and precip)
  load(paste0("inputlyrs/rdata/sdm_base_hist_", clim.mdl, ".rdata"))
  load(paste0("inputlyrs/rdata/climate_hist_", clim.mdl, ".rdata"))
  
  ## Join land.cover.spp, aspect and slope data
  clim <- left_join(clim, select(land, cell.id, spp), by="cell.id") %>%
          left_join(orography, by="cell.id")
  
  ## Assign SDM according to current spp distribution 
  clim$sdm <- NA
  clim$sdm[clim$spp==1] <- sdm$sdm.phalepensis[clim$spp==1]
  clim$sdm[clim$spp==2] <- sdm$sdm.pnigra[clim$spp==2]
  clim$sdm[clim$spp==3] <- sdm$sdm.ppinea[clim$spp==3]
  clim$sdm[clim$spp==4] <- sdm$sdm.psylvestris[clim$spp==4]
  clim$sdm[clim$spp==5] <- sdm$sdm.ppinaster[clim$spp==5]
  clim$sdm[clim$spp==6] <- sdm$sdm.puncinata[clim$spp==6]
  clim$sdm[clim$spp==7] <- sdm$sdm.aalba[clim$spp==7]
  clim$sdm[clim$spp==8] <- sdm$sdm.qilex[clim$spp==8]
  clim$sdm[clim$spp==9] <- sdm$sdm.qsuber[clim$spp==9]
  clim$sdm[clim$spp==10] <- sdm$sdm.qfaginea[clim$spp==10]
  clim$sdm[clim$spp==11] <- sdm$sdm.qhumilis[clim$spp==11]
  clim$sdm[clim$spp==12] <- sdm$sdm.fsylvatica[clim$spp==12]
  clim$sdm[clim$spp==13] <- sdm$sdm.other[clim$spp==13]
  clim$sdm[clim$spp==14] <- 1  ## SDM of shrub is always 1
  
  ## Compute SQ and SQI
  clim <- select(clim, cell.id, spp, temp, precip, sdm, aspect, slope.stand) %>% 
          left_join(site.quality.spp, by="spp") %>% left_join(site.quality.index, by="spp") %>% 
          mutate(aux=c0+c_mnan*temp+c2_mnan*temp*temp+c_plan*precip+c2_plan*precip*precip+
                   c_aspect*ifelse(aspect!=1,0,1)+c_slope*slope.stand) %>%
          mutate(sq=1/(1+exp(-1*aux))) %>% mutate(sqi=ifelse(sq<=p50, 1, ifelse(sq<=p90, 2, 3))) %>%
          select(cell.id, spp, temp, precip, sdm, sqi)
  
  ## SQI for shrubs
  sqi.shrub <- filter(clim, spp %in% 14) %>% select(spp, temp, precip) %>% 
               mutate(aux.brolla=site.quality.shrub$c0_brolla+site.quality.shrub$c_temp_brolla*temp+site.quality.shrub$c_temp2_brolla*temp*temp+site.quality.shrub$c_precip_brolla*precip+site.quality.shrub$c_precip2_brolla*precip*precip,
                      aux.maquia=site.quality.shrub$c0_maquia+site.quality.shrub$c_temp_maquia*temp+site.quality.shrub$c_temp2_maquia*temp*temp+site.quality.shrub$c_precip_maquia*precip+site.quality.shrub$c_precip2_maquia*precip*precip,
                      aux.boix=site.quality.shrub$c0_boix+site.quality.shrub$c_temp_boix*temp+site.quality.shrub$c_temp2_boix*temp*temp+site.quality.shrub$c_precip_boix*precip+site.quality.shrub$c_precip2_boix*precip*precip,
                      sq.brolla=1/(1+exp(-1*aux.brolla)), sq.maquia=1/(1+exp(-1*aux.maquia)), sq.boix=1/(1+exp(-1*aux.boix)))%>% 
               mutate(sqest.brolla=sq.brolla/max(sq.brolla), sqest.maquia=sq.maquia/max(sq.maquia), sqest.boix=sq.boix/max(sq.boix),
                      sqi=ifelse(sqest.brolla>=sqest.maquia & sqest.brolla>=sqest.boix, 1,
                            ifelse(sqest.maquia>=sqest.brolla & sqest.maquia>=sqest.boix, 2,
                              ifelse(sqest.boix>=sqest.brolla & sqest.boix>=sqest.maquia, 3, 0))))
  clim$sqi[clim$spp==14] <- sqi.shrub$sqi
  
  save(clim, file=paste0("inputlyrs/rdata/clim_hist_", clim.mdl,".rdata"))
}



