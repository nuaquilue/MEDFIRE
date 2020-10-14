######################################################################################
##
######################################################################################

land.dyn.mdl <- function(scn.name){
  
  ## Load required packages and functions 
  suppressPackageStartupMessages({
    library(viridis)
    library(raster)  
    library(RANN)  
    library(Rcpp)
    library(tidyverse)
  })
  source("mdl/update.clim.r")
  source("mdl/update.interface.r")
  source("mdl/land.cover.change.r")
  source("mdl/prob.igni.r")
  source("mdl/growth.r")
  source("mdl/drought.r")
  source("mdl/cohort.establish.r")
  source("mdl/afforestation.r")
  source("mdl/forest.mgmt.r")
  source("mdl/fire.regime.r")
  source("mdl/post.fire.r")
  source("mdl/auxiliars.r")
  sourceCpp("mdl/is.in.cpp")
  
  
  ## Load scenario definition (global variables and scenario parameters)
  ## and customized scenario parameters
  source(paste0("outputs/", scn.name, "/scn.def.r"))
  if(file.exists(paste0("outputs/", scn.name, "/scn.custom.def.r")))
    source(paste0("outputs/", scn.name, "/scn.custom.def.r"))
  
  
  ## Load:
  ## 1. Mask of the study area (raster)
  ## 2. Data frame with cell.id and coordinates x, y
  ## 3. Data frame of the model static variables 
  ## 4. Data frame with interface value
  load("inputlyrs/rdata/mask.rdata")
  load("inputlyrs/rdata/coordinates.rdata")
  load("inputlyrs/rdata/orography.rdata")
  load("inputlyrs/rdata/harvest.rdata")
  load("inputlyrs/rdata/interface.rdata")
  
  
  ## Set the directory for writing spatial outputs (create it, if it does not exist yet) 
  if(write.sp.outputs){      
    if(!file.exists(paste0(out.path, "/lyr")))
      dir.create(file.path(getwd(), out.path, "/lyr"), showWarnings = F) 
  }

  
  ## List the name of the forest species
  species <- c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster", "puncinata",
               "aalba", "qilex", "qsuber", "qfaginea", "qhumilis", "fsylvatica", "other")
                  
  
  ## Translation equations from Basal Area to Volum, Volum with bark and Carbon
  eq.ba.vol <- read.table("inputfiles/EqBasalAreaVol.txt", header=T)
  eq.ba.volbark <- read.table("inputfiles/EqBasalAreaVolWithBark.txt", header=T)
  eq.ba.carbon <- read.table("inputfiles/EqBasalAreaCarbon.txt", header=T)
  
  
  ## Climatic severity and pctg hot days tabes
  clim.severity <- read.table(paste0("inputfiles/", file.clim.severity, ".txt"), header=T)
  
  
  ## Build the baseline time sequence and the time sequence of the processes (shared for all runs). 
  ## 1. Climate change, 2. Land-cover changes, 3. Forest management
  ## 4. Wildfires, 5. Prescribed burns, 6. Drought, 7. Post-fire regeneration,
  ## 8. Cohort establihsment, 9. Afforestation, 10. Growth
  time.seq <- seq(1, time.horizon, 1)
  if(time.horizon==1)
    clim.schedule <- 1
  else
    clim.schedule <- seq(1, time.horizon-1, clim.step)
  lchg.schedule <- seq(1, time.horizon, lchg.step)
  mgmt.schedule <- seq(1, time.horizon, mgmt.step)
  fire.schedule <- seq(1, time.horizon, fire.step)
  pb.schedule <- seq(1, time.horizon, pb.step)
  drought.schedule <- seq(1, time.horizon, drought.step)
  post.fire.schedule <- seq(1, time.horizon, post.fire.step)
  cohort.schedule <- seq(1, time.horizon, cohort.step)
  afforest.schedule <- seq(1, time.horizon, afforest.step)
  growth.schedule <- seq(1, time.horizon, growth.step)
  

  ## Tracking data.frames
  track.harvest <- data.frame(run=NA, year=NA, spp=NA, vol.sawlog=NA, vol.wood=NA)
  track.fire <- data.frame(run=NA, year=NA, swc=NA, clim.sever=NA, fire.id=NA, fst=NA, wind=NA, atarget=NA, 
                            aburnt.highintens=NA, aburnt.lowintens=NA, asupp.fuel=NA, asupp.sprd=NA)
  track.fire.spp <- data.frame(run=NA, year=NA, fire.id=NA, spp=NA, aburnt=NA, bburnt=NA)
  track.step <- data.frame(run=NA, year=NA, fire.id=NA, step=NA, nneigh=NA, nneigh.in=NA, nburn=NA, nff=NA)
  track.pb <- data.frame(run=NA, year=NA, clim.sever=NA, fire.id=NA, 
                          wind=NA, atarget=NA, aburnt.lowintens=NA)
  track.drougth <- data.frame(run=NA, year=NA, spp=NA, ha=NA)
  track.cohort <- data.frame(run=NA, year=NA, spp.out=NA, Var2=NA, Freq=NA)
  track.post.fire <- data.frame(run=NA, year=NA, spp.out=NA, Var2=NA, Freq=NA)
  track.afforest <- data.frame(run=NA, year=NA, Var1=NA, Freq=NA)
  track.land <- data.frame(run=NA, year=NA, spp=NA, area=NA, vol=NA, volbark=NA, carbon=NA)
  track.target <- data.frame(run=NA, year=NA, swc=NA, atarget=NA)
  
  ## Start the simulations   
  irun <- 1
  for(irun in 1:nrun){
    
    ## Copy the schedulings in auxiliar vectors (only for those processes included in the current version)
    temp.clim.schedule <- clim.schedule
    temp.lchg.schedule <- lchg.schedule
    temp.mgmt.schedule <- mgmt.schedule
    temp.fire.schedule <- fire.schedule
    temp.pb.schedule <- pb.schedule
    temp.drought.schedule <- drought.schedule
    temp.post.fire.schedule <- post.fire.schedule
    temp.cohort.schedule <- cohort.schedule
    temp.afforest.schedule <- afforest.schedule
    temp.growth.schedule <- growth.schedule
    
    
    ## Load initial spatial dynamic state variables in a data.frame format
    load("inputlyrs/rdata/land.rdata")
    
    
    ## Start the discrete time sequence 
    t <- 1
    for(t in time.seq){
      
      ## Track scenario, replicate and time step
      cat(paste0("scn: ", scn.name," - run: ", irun, "/", nrun, " - time: ", t, "/", time.horizon), "\n")
      
      
      ## 1. CLIMATE CHANGE  
      if(processes[clim.id] & t %in% temp.clim.schedule){
        clim <- update.clim(MASK, land, orography, decade=(1+floor(t/10))*10, clim.scn, clim.mdl)
        load(paste0("inputlyrs/rdata/sdm_base_", clim.scn, "_", clim.mdl, "_", (1+floor(t/10))*10, ".rdata"))
        temp.clim.schedule <- temp.clim.schedule[-1] 
      }
      
      
      ## 2. LAND-COVER CHANGE
      if(processes[lchg.id] & t %in% temp.lchg.schedule){
        # Urbanization
        chg.cells <- land.cover.change(land, coord, interface, 1, t, numeric())
        land$spp[land$cell.id %in% chg.cells] <- 20 # urban
        land$biom[land$cell.id %in% chg.cells] <- NA
        land$age[land$cell.id %in% chg.cells] <- NA
        land$tsdist[land$cell.id %in% visit.cells] <- NA  # don't care the time since it's urban
        land$typdist[land$cell.id %in% chg.cells] <- "lchg.urb"
        land$tburnt[land$cell.id %in% chg.cells] <- NA
        # Agriculture conversion
        visit.cells <- chg.cells
        chg.cells <- land.cover.change(land, coord, interface, 2, t, visit.cells)
        land$spp[land$cell.id %in% chg.cells] <- 16 # arableland or 17 - permanent crops
        land$biom[land$cell.id %in% chg.cells] <- NA
        land$age[land$cell.id %in% chg.cells] <- NA
        land$typdist[land$cell.id %in% chg.cells] <- "lchg.crp"
        land$tsdist[land$cell.id %in% visit.cells] <- 0
        land$tburnt[land$cell.id %in% chg.cells] <- 0
        # Rural abandonment
        visit.cells <- c(visit.cells, chg.cells)
        chg.cells <- land.cover.change(land, coord, interface, 3, t, visit.cells)
        land$spp[land$cell.id %in% chg.cells] <- 14  # shrub
        land$biom[land$cell.id %in% chg.cells] <- 0
        land$age[land$cell.id %in% chg.cells] <- 0
        land$typdist[land$cell.id %in% chg.cells] <- "lchg.nat"
        land$tsdist[land$cell.id %in% visit.cells] <- 0
        land$tburnt[land$cell.id %in% chg.cells] <- 0
        # Update interface values
        interface <- update.interface(land)
        temp.lchg.schedule <- temp.lchg.schedule[-1] 
        rm(chg.cells); rm(visit.cells)
      }
      
      
      ## 3. FOREST MANAGEMENT (under development)
      if(processes[mgmt.id] & t %in% temp.mgmt.schedule){
        cut.out <- forest.mgmt(land, harvest, clim, t, out.path, MASK)
        land$typdist[land$cell.id %in% cut.out$cell.id] <- "cut"
        land$tsdist[land$cell.id %in% cut.out$cell.id] <- 0
        land$typcut[land$cell.id %in% cut.out$cell.id] <- cut.out$typcut
        land$tscut[land$cell.id %in% cut.out$cell.id] <- 0
        land$age[land$cell.id %in% cut.out$cell.id & land$typcut=="fin"] <- 0
        land$biom[land$cell.id %in% cut.out$cell.id] <- 
        land$biom[land$cell.id %in% cut.out$cell.id]-cut.out$ba.extract
        track.harvest <- rbind(track.harvest, data.frame(run=irun, year=t, 
            group_by(cut.out, spp) %>% summarize(vol.sawlog=round(sum(vol.sawlog),1), vol.wood=round(sum(vol.wood),1))))
        temp.mgmt.schedule <- temp.mgmt.schedule[-1] 
      }
      
      
      ## 4. FIRE
      if(processes[fire.id] & t %in% temp.fire.schedule){
        # Decide climatic severity of the year (default is mild)
        clim.sever <- 0
        if(runif(1,0,100) < clim.severity[clim.severity$year==t, ncol(clim.severity)]) # not-mild
          clim.sever <- 1
        # Burnt
        fire.out <- fire.regime(land, coord, orography, clim, interface, 1:3, clim.sever, t, 0, MASK, out.path, irun, nff, crazy)
        # Track fires and Burnt spp & Biomass
        if(nrow(fire.out[[1]])>0)
          track.fire <- rbind(track.fire, data.frame(run=irun, fire.out[[1]]))
        burnt.cells <- fire.out[[2]] %>% select(-igni)
        if(nrow(burnt.cells)>0){
          aux <- left_join(burnt.cells, select(land, cell.id, spp, biom), by="cell.id") %>%
                 mutate(bburnt=ifelse(fintensity>fire.intens.th, biom, biom*(1-fintensity))) %>%
                 group_by(fire.id, spp) %>% summarize(aburnt=length(spp), bburnt=round(sum(bburnt, na.rm=T),1))
          track.fire.spp <-  rbind(track.fire.spp, data.frame(run=irun, year=t, aux)) 
          track.step <- rbind(track.step, data.frame(run=irun, fire.out[[3]]))
        }
        # Done with fires! When high-intensity fire, age = biom = 0 and dominant tree species may change
        # when low-intensity fire, age remains, spp remains and biomass.t = biomass.t-1 * (1-fintensity)
        burnt.cells$intens <- burnt.cells$fintensity>fire.intens.th
        land$tsdist[land$cell.id %in% burnt.cells$cell.id] <- 0
        land$tburnt[land$cell.id %in% burnt.cells$cell.id] <- land$tburnt[land$cell.id %in% burnt.cells$cell.id] + 1
        land$typdist[land$cell.id %in% burnt.cells$cell.id[burnt.cells$intens]] <- "highfire"
        land$typdist[land$cell.id %in% burnt.cells$cell.id[!burnt.cells$intens]] <- "lowfire"
        land$biom[land$cell.id %in% burnt.cells$cell.id[burnt.cells$intens]] <- 0
        land$biom[land$cell.id %in% burnt.cells$cell.id[!burnt.cells$intens]] <- 
           land$biom[land$cell.id %in% burnt.cells$cell.id[!burnt.cells$intens]]*(1-burnt.cells$fintensity[!burnt.cells$intens])
        temp.fire.schedule <- temp.fire.schedule[-1] 
        rm(fire.out); rm(aux)  
      }
      
      
      ## 5. PRESCRIBED BURNS
      id.fire <- 0
      if(processes[pb.id] & t %in% temp.pb.schedule){
        # Annual area burnt for PB
        annual.burnt.area <- ifelse(exists("burnt.cells"), nrow(burnt.cells), 0)
        fire.out <- fire.regime(land, coord, orography, clim, interface, 4, clim.sever, t, annual.burnt.area, MASK, out.path, irun, nff)
        # Track pb and Done with prescribed burns!
        if(nrow(fire.out[[1]])>0){
          track.pb <- rbind(track.pb, data.frame(run=irun, fire.out[[1]][,c(1,3,4,6,7,9)]))
          pb.cells <- fire.out[[2]] %>% select(-igni)  
          land$tsdist[land$cell.id %in% pb.cells$cell.id] <- 0
          land$tburnt[land$cell.id %in% pb.cells$cell.id] <- land$tburnt[land$cell.id %in% pb.cells$cell.id] + 1
          land$typdist[land$cell.id %in% pb.cells$cell.id] <- "pb"
          land$biom[land$cell.id %in% pb.cells$cell.id] <- land$biom[land$cell.id %in% pb.cells$cell.id]*(1-pb.cells$fintensity)
          temp.pb.schedule <- temp.pb.schedule[-1] 
        }
        rm(fire.out)
      }
      
      
      ## 6. DROUGHT
      killed.cells <- integer()
      if(processes[drought.id] & t %in% temp.drought.schedule){
        killed.cells <- drought(land, clim, t)
        land$tsdist[land$cell.id %in% killed.cells] <- 0
        land$typdist[land$cell.id %in% killed.cells] <- "drght"
        track.drougth <- rbind(track.drougth,
                              data.frame(run=irun, year=t, 
                                         filter(land, cell.id %in% killed.cells) %>% group_by(spp) %>% summarize(ha=length(spp))) )
        temp.drought.schedule <- temp.drought.schedule[-1] 
      }
      
      
      ## 7. POST-FIRE REGENERATION
      if(processes[post.fire.id] & t %in% temp.post.fire.schedule){
        ## forest transition of tree species burnt in high intensity
        aux  <- post.fire(land, coord, orography, clim, sdm)
        if(nrow(aux)>0){
          spp.out <- land$spp[land$cell.id %in% aux$cell.id]
          land$spp[land$cell.id %in% aux$cell.id] <- aux$spp
          clim$spp[clim$cell.id %in% aux$cell.id] <- aux$spp
          clim$sdm[clim$cell.id %in% aux$cell.id] <- 1
          clim$sqi[clim$cell.id %in% aux$cell.id] <- aux$sqi
          track.post.fire <- rbind(track.post.fire, data.frame(run=irun, year=t, table(spp.out, aux$spp)))  
        }
        # Reset age of cells burnt in high intensity
        land$age[land$cell.id %in% burnt.cells$cell.id[burnt.cells$intens] & !is.na(land$spp) & land$spp<=14] <- 0
        temp.post.fire.schedule <- temp.post.fire.schedule[-1] 
        rm(aux); rm(spp.out)
      }
      
      
      ## 8. COHORT ESTABLISHMENT
      if(processes[cohort.id] & t %in% temp.cohort.schedule & length(killed.cells)>0){
        aux  <- cohort.establish(land, coord, orography, clim, sdm)
        spp.out <- land$spp[land$cell.id %in% killed.cells]
        land$spp[land$cell.id %in% killed.cells] <- aux$spp
        land$age[land$cell.id %in% aux$cell.id] <- 0  ## not sure if 0 or decade - t -1
        clim$spp[clim$cell.id %in% killed.cells] <- aux$spp
        clim$sdm[clim$cell.id %in% killed.cells] <- 1
        clim$sqi[clim$cell.id %in% killed.cells] <- aux$sqi
        track.cohort <- rbind(track.cohort, data.frame(run=irun, year=t, table(spp.out, aux$spp)))
        temp.cohort.schedule <- temp.cohort.schedule[-1] 
        rm(aux); rm(spp.out); rm(killed.cells)
      }
      
      
      ## 9. AFFORESTATION
      if(processes[afforest.id] & t %in% temp.afforest.schedule){
        aux  <- afforestation(land, coord, orography, clim, sdm)
        land$spp[land$cell.id %in% aux$cell.id] <- aux$spp
        land$age[land$cell.id %in% aux$cell.id] <- 0
        land$tsdist[land$cell.id %in% aux$cell.id] <- 0
        land$typdist[land$cell.id %in% aux$cell.id] <- "afforest"
        clim$spp[clim$cell.id %in% aux$cell.id] <- aux$spp
        clim$sdm[clim$cell.id %in% aux$cell.id] <- 1
        clim$sqi[clim$cell.id %in% aux$cell.id] <- aux$sqi
        track.afforest <- rbind(track.afforest, data.frame(run=irun, year=t, table(aux$spp)))
        temp.afforest.schedule <- temp.afforest.schedule[-1] 
        rm(aux)
      }
      
      
      ## 10. GROWTH
      if(processes[growth.id] & t %in% temp.growth.schedule){
        land$biom <- growth.10y(land, clim)
        land$age <- pmin(land$age+1,600)
        land$tsdist <- pmin(land$tsdist+1,600)
        land$tscut <- pmin(land$tscut+1,600)
        aux <- filter(land, spp<=13) %>% select(spp, biom) %>% left_join(eq.ba.vol, by="spp") %>% 
               mutate(vol=cx*biom/10+cx2*biom*biom/100) %>% select(-cx, -cx2) %>%
               left_join(eq.ba.volbark, by="spp") %>% 
               mutate(volbark=cx*biom/10+cx2*biom*biom/100) %>% select(-cx, -cx2) %>% 
               left_join(eq.ba.carbon, by="spp") %>% 
               mutate(carbon=c*biom/10) %>% group_by(spp) %>% select(-c) %>%
               summarise(area=length(vol), vol=sum(vol), volbark=sum(volbark), carbon=sum(carbon))  
        aux.shrub <- filter(land, spp==14) %>% select(spp, biom) %>% group_by(spp) %>%
                     summarise(area=length(biom), vol=sum(biom), volbark=0, carbon=0)  
        track.land <- rbind(track.land, data.frame(run=irun, year=t, aux), data.frame(run=irun, year=t, aux.shrub))
        temp.growth.schedule <- temp.growth.schedule[-1] 
        rm(aux); rm(aux.shrub)
      }
      
      
      # Print maps every time step with ignition and low/high intenstiy burnt
      if(write.sp.outputs){
        cat("... writing output layers", "\n")
        MAP <- MASK
        MAP[!is.na(MASK[])] <- ifelse(land$typdist %in% c("lchg.urb", "lchg.crp", "lchg.nat"), 1, 
                                      ifelse(land$typdist == "cut", 2, 
                                             ifelse(land$typdist %in% c("highfire", "lowfire"), 3,   
                                                    ifelse(land$typdist == "pb", 4,
                                                           ifelse(land$typdist == "drght", 5,
                                                                  ifelse(land$typdist == "afforest", 6, NA))))))
        writeRaster(MAP, paste0(out.path, "/lyr/TypeDist_r", irun, "t", t, ".tif"), format="GTiff", overwrite=T)
      }
      
      ## Deallocate memory
      gc(verbose=F); cat("\n")
      
    } # time
  
    # Print maps at the end of the simulation period per each run
    if(write.sp.outputs){
      MAP <- MASK
      MAP[!is.na(MASK[])] <- land$tburnt
      writeRaster(MAP, paste0(out.path, "/lyr/TimesBurnt_r", irun, ".tif"), format="GTiff", overwrite=T)
    }

  } # run
  
  cat("... writing outputs", "\n")
  write.table(track.harvest[-1,], paste0(out.path, "/Harvest.txt"), quote=F, row.names=F, sep="\t")
  track.fire$rem <- pmax(0, track.fire$atarget-track.fire$aburnt.highintens-track.fire$aburnt.lowintens-
                           track.fire$asupp.fuel - track.fire$asupp.sprd)
  write.table(track.fire[-1,], paste0(out.path, "/Fires.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.fire.spp[-1,], paste0(out.path, "/FiresSpp.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.step[-1,], paste0(out.path, "/FiresStep.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.pb[-1,], paste0(out.path, "/PrescribedBurns.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.drougth[-1,], paste0(out.path, "/Drought.txt"), quote=F, row.names=F, sep="\t")
  names(track.post.fire)[4:5] <- c("spp.in", "ha")
  write.table(track.post.fire[-1,], paste0(out.path, "/PostFire.txt"), quote=F, row.names=F, sep="\t")
  names(track.cohort)[4:5] <- c("spp.in", "ha")
  write.table(track.cohort[-1,], paste0(out.path, "/Cohort.txt"), quote=F, row.names=F, sep="\t")
  names(track.afforest)[3:4] <- c("spp", "ha")
  write.table(track.afforest[-1,], paste0(out.path, "/Afforestation.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.land[-1,], paste0(out.path, "/Land.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.target[-1,], paste0(out.path, "/FireRegime.txt"), quote=F, row.names=F, sep="\t")
}
