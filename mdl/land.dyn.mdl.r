######################################################################################
##
######################################################################################


land.dyn.mdl <- function(scn.name){
  
  ## Load required packages and functions 
  library(raster)  
  library(RANN)  # for nn2()
    # library(SpaDES)  # for adj()
  library(tidyverse)
  source("mdl/update.clim.r")
  source("mdl/update.interface.r")
  source("mdl/prob.igni.r")
  source("mdl/growth.r")
  source("mdl/drought.r")
  source("mdl/cohort.establish.r")
  source("mdl/afforestation.r")
  source("mdl/forest.mgmt.r")
  
  
  ## Load scenario definition (global variables and scenario parameters)
  ## and customized scenario parameters
  source(paste0("outputs/", scn.name, "/scn.def.r"))
  if(file.exists(paste0("outputs/", scn.name, "/scn.custom.def.r")))
    source(paste0("outputs/", scn.name, "/scn.custom.def.r"))
  
  
  ## Set the directory for writing spatial outputs (create if it does not exist yet) 
  if(write.sp.outputs){      
    if(!file.exists(paste0(out.path, "/asc")))
      dir.create(file.path(getwd(), out.path, "/asc"), showWarnings = F) 
    if(!file.exists(paste0(out.path, "/rdata")))
      dir.create(file.path(getwd(), out.path, "/rdata"), showWarnings = F) 
  }

  
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
  
  
  ## List the name of the forest species
  species <- c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster", "puncinata",
               "aalba", "qilex", "qsuber", "qfaginea", "qhumilis", "fsylvatica", "other")
                  
  
  ## Global constants
  eq.ba.vol <- read.table("inputfiles/EqBasalAreaVol.txt", header=T)
  eq.ba.volbark <- read.table("inputfiles/EqBasalAreaVolWithBark.txt", header=T)
  eq.ba.carbon <- read.table("inputfiles/EqBasalAreaCarbon.txt", header=T)
  
  ## Build the baseline time sequence and the time sequence of the processes (shared for all runs). 
  ## 1. Climate change, 2. Interfaces, 3. Forest management
  ## 4. Wildfires, 5. Prescribed burns, 6. Drought, 7. Post-fire regeneration,
  ## 8. Cohort establihsment, 9. Afforestation, 10. Growth
  time.seq <- seq(1, time.horizon, 1)
  if(time.horizon==1)
    clim.schedule <- 1
  else
    clim.schedule <- seq(1, time.horizon-1, clim.step)
  if(interface.step==0)
    interface.schedule <- 0
  else
    interface.schedule <- seq(1, time.horizon, interface.step)
  fmgmt.schedule <- seq(1, time.horizon, fmgmt.step)
  fire.schedule <- seq(1, time.horizon, fire.step)
  pb.schedule <- seq(1, time.horizon, pb.step)
  drought.schedule <- seq(1, time.horizon, drought.step)
  post.fire.schedule <- seq(1, time.horizon, post.fire.step)
  cohort.schedule <- seq(1, time.horizon, cohort.step)
  afforest.schedule <- seq(1, time.horizon, afforest.step)
  growth.schedule <- seq(1, time.horizon, growth.step)
  
  
  ## Give identificators to processes of the model
  clim.id <- 1;   interface.id <- 2;   fmgmt.id <- 3;   fire.id <- 4;   pb.id <- 5
  drought.id <- 6;   post.fire.id <- 7; cohort.id <- 8;  afforest.id <- 9;   growth.id <- 10
  
  
  ## Initialize model global parameters (equal for all scn)
  ## It's faster to initialize them in land.dyn.mdl() function than throught define.scenario()
  ## Anyways, they should be in define.scenario.r
  spp.distrib.rad <- 20 	# neighborhood radius to determine which species belong to that region (in pixels)
  shrub.colon.rad <- 5 		#

  
  ## Tracking data.frames
  track.fmgmt <- data.frame(run=NA, year=NA, spp=NA, sylvi=NA, sawlog=NA, wood=NA)
  track.drougth <- data.frame(run=NA, year=NA, spp=NA, ha=NA)
  track.cohort <- data.frame(run=NA, year=NA, spp.out=NA, Var2=NA, Freq=NA)
  track.afforest <- data.frame(run=NA, year=NA, Var1=NA, Freq=NA)
  track.land <- data.frame(run=NA, year=NA, spp=NA, area=NA, vol=NA, volbark=NA, carbon=NA)
  
  
  ## Start the simulations   
  irun=1   # for testing
  for(irun in 1:nrun){
    
    ## Copy the schedulings in auxiliar vectors (only for those processes included in the current version)
    temp.clim.schedule <- clim.schedule
    temp.interface.schedule <- interface.schedule
    temp.fmgmt.schedule <- fmgmt.schedule
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
    t=1  # for testing
    for(t in time.seq){
      
      ## Track scenario, replicate and time step
      print(paste0("scn: ", scn.name," - run: ", irun, "/", nrun, " - time: ", t, "/", time.horizon))
    
      
      ## 1. CLIMATE CHANGE  
      if(processes[clim.id] & t %in% temp.clim.schedule){
        clim <- update.clim(land, orography, MASK, species, decade=(1+floor(t/10))*10, clim.scn, psdm)
        load(paste0("inputlyrs/rdata/sdm_", psdm, "p_", clim.scn, "_", (1+floor(t/10))*10, ".rdata"))
        temp.clim.schedule <- temp.clim.schedule[-1] 
      }
      
      
      ## 2. INTERFACE
      if(processes[interface.id] & t %in% temp.interface.schedule){
        interface <- update.interface(land)
        temp.interface.schedule <- temp.interface.schedule[-1] 
      }
      
      
      ## 3. FOREST MANAGEMENT
      if(processes[fmgmt.id] & t %in% temp.fmgmt.schedule){
        aux <- forest.mgmt(land, clim, harvest, coord, t)
        land$tsdist[land$cell.id %in% aux$cell.id] <- 0
        land$distype[land$cell.id %in% aux$cell.id] <- fmgmt.id*10+aux$sylvi
        track.fmgmt <- rbind(track.fmgmt, 
                             data.frame(run=irun, year=t, 
                                        group_by(aux, spp, sylvi) %>% summarize(sawlog=sum(vol.sawlog), wood=sum(vol.wood))))
        temp.fmgmt.schedule <- temp.fmgmt.schedule[-1] 
      }
      
      
      burnt.cells <- integer()
      ## 4. FIRE
      if(processes[fire.id] & t %in% temp.fire.schedule){
        pigni <- prob.igni(land, clim, orography, interface)
        temp.fire.schedule <- temp.fire.schedule[-1] 
      }
      
      
      ## 5. PRESCRIBED BURNS
      if(processes[pb.id] & t %in% temp.pb.schedule){
        temp.pb.schedule <- temp.pb.schedule[-1] 
      }
      
      
      ## 6. DROUGHT
      kill.cells <- integer()
      if(processes[drought.id] & t %in% temp.drought.schedule){
        kill.cells <- drought(land, clim, t)
        land$tsdist[land$cell.id %in% kill.cells] <- 0
        land$distype[land$cell.id %in% kill.cells] <- drought.id
        track.drougth <- rbind(track.drougth,
                              data.frame(run=irun, year=t, 
                                         filter(land, cell.id %in% kill.cells) %>% group_by(spp) %>% summarize(ha=length(spp))) )
        temp.drought.schedule <- temp.drought.schedule[-1] 
      }
      
      
      ## 7. POST-FIRE REGENERATION
      if(processes[post.fire.id] & t %in% temp.post.fire.schedule){
        temp.post.fire.schedule <- temp.post.fire.schedule[-1] 
      }
      
      
      ## 8. COHORT ESTABLISHMENT
      if(processes[cohort.id] & t %in% temp.cohort.schedule & length(kill.cells)>0){
        aux  <- cohort.establish(land, clim, orography, sdm, coord, spp.distrib.rad, drought.id)
        spp.out <- land$spp[land$cell.id %in% kill.cells]
        land$spp[land$cell.id %in% kill.cells] <- aux$spp
        land$biom[land$cell.id %in% kill.cells] <- growth.10y(aux, aux)
        clim$spp[clim$cell.id %in% kill.cells] <- aux$spp
        clim$sdm[clim$cell.id %in% kill.cells] <- 1
        clim$sqi[clim$cell.id %in% kill.cells] <- aux$sqi
        track.cohort <- rbind(track.cohort, data.frame(run=irun, year=t, table(spp.out, aux$spp)))
        rm(aux); rm(kill.cells); gc()
        temp.cohort.schedule <- temp.cohort.schedule[-1] 
      }
      
      
      ## 9. AFFORESTATION
      if(processes[afforest.id] & t %in% temp.afforest.schedule){
        aux  <- afforestation(land, clim, orography, sdm, coord, shrub.colon.rad)
        land$spp[land$cell.id %in% aux$cell.id] <- aux$spp
        land$biom[land$cell.id %in% aux$cell.id] <- growth.10y(aux, aux)
        clim$spp[clim$cell.id %in% aux$cell.id] <- aux$spp
        clim$sdm[clim$cell.id %in% aux$cell.id] <- 1
        clim$sqi[clim$cell.id %in% aux$cell.id] <- aux$sqi
        track.afforest <- rbind(track.afforest, data.frame(run=irun, year=t, table(aux$spp)))
        temp.afforest.schedule <- temp.afforest.schedule[-1] 
      }
      
      
      ## 10. GROWTH
      if(processes[growth.id] & t %in% temp.growth.schedule){
        land$biom <- growth.10y(land, clim)
        land$age <- pmin(land$age+1,600)
        land$tsdist <- pmin(land$tsdist+1,600)
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
      }
      
    } # time
  
  } # run
  
  print("Writing outputs")
  write.table(track.fmgmt[-1,], paste0(out.path, "/Management.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.drougth[-1,], paste0(out.path, "/Drought.txt"), quote=F, row.names=F, sep="\t")
  names(track.cohort)[4:5] <- c("spp.in", "ha")
  write.table(track.cohort[-1,], paste0(out.path, "/Cohort.txt"), quote=F, row.names=F, sep="\t")
  names(track.afforest)[3:4] <- c("spp", "ha")
  write.table(track.afforest[-1,], paste0(out.path, "/Afforestation.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.land[-1,], paste0(out.path, "/Land.txt"), quote=F, row.names=F, sep="\t")

}