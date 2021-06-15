######################################################################################
##
######################################################################################

land.dyn.mdl <- function(scn.name){
  
  ## Load required packages and functions 
  suppressPackageStartupMessages({
    library(raster)  
    library(RANN)  
    library(Rcpp)
    library(dplyr, warn.conflicts=F)
    options(dplyr.summarise.inform=F)
  })
  source("mdl/afforestation.r")
  source("mdl/auxiliars.r")
  source("mdl/cohort.establish.r")
  source("mdl/drought.r")
  source("mdl/fire.regime.r")
  source("mdl/sustainable.mgmt.r")
  source("mdl/growth.r")
  source("mdl/land.cover.change.r")
  source("mdl/post.fire.r")
  source("mdl/prob.igni.r")
  source("mdl/update.clim.r")
  source("mdl/update.interface.r")
  sourceCpp("mdl/is.in.cpp")
  `%notin%` <- Negate(`%in%`)
  
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
  load("inputlyrs/rdata/pfst.pwind.rdata")
  if(spin.up)
    load("inputlyrs/rdata/wildfires.rdata")
  
  
  ## Set the directory for writing spatial outputs (create it, if it does not exist yet) 
  if(write.maps){      
    if(!file.exists(paste0(out.path, "/rdata")))
      dir.create(file.path(getwd(), out.path, "/rdata"), showWarnings = F) 
  }

  
  ## List the name of the forest species
  species <- c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster", "puncinata",
               "aalba", "qilex", "qsuber", "qfaginea", "qhumilis", "fsylvatica", "other")
                  
  
  ## Translation equations from Basal Area to Volum, Volum with bark and Carbon
  eq.ba.vol <- read.table("inputfiles/EqBasalAreaVol.txt", header=T)
  eq.ba.volbark <- read.table("inputfiles/EqBasalAreaVolWithBark.txt", header=T)
  eq.ba.carbon <- read.table("inputfiles/EqBasalAreaCarbon.txt", header=T)
  site.quality.shrub <- read.table("inputfiles/SiteQualityShrub.txt", header=T)
  
  
  ## Climatic severity 
  clim.severity <- read.table(paste0("inputfiles/", file.clim.severity, ".txt"), header=T)
  
  
  ## Build the baseline time sequence and the time sequence of the processes (shared for all runs). 
  ## 1. Climate change, 2. Land-cover changes, 3. Forest management
  ## 4. Wildfires, 5. Prescribed burns, 6. Drought, 7. Post-fire regeneration,
  ## 8. Cohort establihsment, 9. Afforestation, 10. Growth
  time.seq <- 
  lchg.schedule <- 
  mgmt.schedule <- 
  fire.schedule <- 
  pb.schedule <- 
  drought.schedule <-
  post.fire.schedule <- 
  cohort.schedule <- 
  afforest.schedule <- 
  growth.schedule <- seq(1, time.horizon, time.step)
  if(spin.up & time.horizon>10){
    lchg.schedule <- seq(11, time.horizon, time.step)
    fire.schedule <- seq(11, time.horizon, time.step)
  }
  if(spin.up & time.horizon<=10){
    lchg.schedule <- 
    fire.schedule <- numeric()
  }
  clim.schedule <- seq(1, time.horizon, time.step*10) 
    
  ## Tracking data.frames
  track.harvest <- data.frame(run=NA, year=NA, spp=NA, vol.sawlog=NA, vol.wood=NA)
  track.fire <- data.frame(run=NA, year=NA, swc=NA, clim.sever=NA, fire.id=NA, fst=NA, wind=NA, atarget=NA, 
                           aburnt.highintens=NA, aburnt.lowintens=NA, asupp.fuel=NA, asupp.sprd=NA, rem=NA)
  track.fire.spp <- data.frame(run=NA, year=NA, fire.id=NA, spp=NA, aburnt=NA, bburnt=NA)
  # track.step <- data.frame(run=NA, year=NA, fire.id=NA, step=NA, nneigh=NA, nneigh.in=NA, nburn=NA, nff=NA)
  # track.sr <- data.frame(run=NA, year=NA, swc=NA, clim.sever=NA, cell.id=NA, fire.id=NA, spp=NA, age=NA, fi=NA, pb=NA,
  #                        nsource=NA, nsupp.sprd=NA, nsupp.fuel=NA, tosupp.sprd=NA, tosupp.fuel=NA, burn=NA)
  # track.sr.source <- data.frame(run=NA, year=NA, swc=NA, clim.sever=NA, cell.id=NA, spp=NA, biom=NA, age=NA, fuel=NA,
  #                               source.id=NA, position=NA, dist=NA, windir=NA, nsupp.sprd=NA, nsupp.fuel=NA,
  #                               elev.x=NA, elev.y=NA, dif.elev=NA, dif.wind=NA, slope=NA, wind=NA, sr=NA, fi=NA, pb=NA)
  track.pb <- data.frame(run=NA, year=NA, clim.sever=NA, fire.id=NA, 
                          wind=NA, atarget=NA, aburnt.lowintens=NA)
  track.drougth <- data.frame(run=NA, year=NA, spp=NA, ha=NA)
  track.cohort <- data.frame(run=NA, year=NA, spp.out=NA, spp.in=NA, ha=NA) #Var2=NA, Freq=NA)
  track.post.fire <- data.frame(run=NA, year=NA, spp.out=NA, spp.in=NA, ha=NA) #Var2=NA, Freq=NA)
  track.afforest <- data.frame(run=NA, year=NA, spp=NA, ha=NA) #Var1=NA, Freq=NA)
  track.land <- data.frame(run=NA, year=NA, spp=NA, age.class=NA, area=NA, vol=NA, volbark=NA, carbon=NA)
  
  
  ## Start the simulations   
  irun <- 1
  for(irun in 1:nrun){

    ## Load initial spatial dynamic state variables in a data.frame format
    load("inputlyrs/rdata/land.rdata")
    if(spin.up)
      load("inputlyrs/rdata/land.cover.changes.rdata")
    
    ## Land at time 0, at the initial stage
    aux.forest <- filter(land, spp<=13) %>% select(spp, age, biom) %>% left_join(eq.ba.vol, by="spp") %>% 
                  mutate(vol=cx*biom/10+cx2*biom*biom/100) %>% select(-cx, -cx2) %>%
                  left_join(eq.ba.volbark, by="spp") %>% 
                  mutate(volbark=cx*biom/10+cx2*biom*biom/100) %>% select(-cx, -cx2) %>% 
                  left_join(eq.ba.carbon, by="spp") %>% 
                  mutate(carbon=c*biom/10) %>% 
                  mutate(age.class=ifelse(spp<=7 & age<=15, "young", ifelse(spp<=7 & age<=50, "mature",
                         ifelse(spp<=7 & age>50, "old", ifelse(spp>7 & spp<=13 & age<=15, "young",
                         ifelse(spp>7 & spp<=13 & age<=50, "mature", "old")))))) %>%       
                  group_by(spp, age.class) %>% select(-c) %>%
                  summarise(area=length(vol), vol=sum(vol), volbark=sum(volbark), carbon=sum(carbon))  
    aux.shrub <- filter(land, spp==14) %>% select(spp, biom) %>% group_by(spp) %>%
                 summarise(age.class=NA, area=length(biom), vol=sum(biom), volbark=0, carbon=0)  
    aux.other <- filter(land, spp>14) %>% select(spp) %>% group_by(spp) %>%
                 summarise(age.class=NA, area=length(spp), vol=0, volbark=0, carbon=0)  
    track.land <- rbind(track.land, data.frame(run=irun, year=0, aux.forest), data.frame(run=irun, year=0, aux.shrub),
                        data.frame(run=irun, year=0, aux.other))
    
    ## Start the discrete time sequence 
    t <- 1
    for(t in time.seq){
      
      ## Track scenario, replicate and time step
      cat(paste0("scn: ", scn.name," - run: ", irun, "/", nrun, " - time: ", t, "/", time.horizon), "\n")
      decade=(1+floor((t-1)/10))*10
      
      ## 1. CLIMATE CHANGE  
      if(!is.climate.change & t==1){
        load(paste0("inputlyrs/rdata/clim_hist_", clim.mdl, ".rdata"))
        load(paste0("inputlyrs/rdata/sdm_base_hist_", clim.mdl, ".rdata"))
      }
      if(is.climate.change & t %in% clim.schedule){
        clim <- update.clim(land, orography, decade, clim.scn, clim.mdl)
        load(paste0("inputlyrs/rdata/sdm_base_", clim.scn, "_", clim.mdl, "_", decade, ".rdata"))
      }

      
      ## 2. LAND-COVER CHANGE
      if(spin.up & t<=10){
        cat("Observed land-cover changes", "\n")
        ## Select the cells 
        urban.cells <- c(sample(unlist(filter(land.cover.changes, code==1420) %>% select(cell.id)), 189, replace=F),
                         sample(unlist(filter(land.cover.changes, code==1520) %>% select(cell.id)), 4, replace=F),
                         sample(unlist(filter(land.cover.changes, code==1620) %>% select(cell.id)), 516, replace=F))
        water.cells <- c(sample(unlist(filter(land.cover.changes, code==1419) %>% select(cell.id)), 220, replace=F),
                         sample(unlist(filter(land.cover.changes, code==1519) %>% select(cell.id)), 71, replace=F),
                         sample(unlist(filter(land.cover.changes, code==1619) %>% select(cell.id)), 501, replace=F))
        grass.cells <- c(sample(unlist(filter(land.cover.changes, code==1415) %>% select(cell.id)), 84, replace=F),
                         sample(unlist(filter(land.cover.changes, code==1615) %>% select(cell.id)), 119, replace=F))
        shrub.cells <- sample(unlist(filter(land.cover.changes, code==1614) %>% select(cell.id)), 6340, replace=F)
        ## Apply the changes in "land" and "clim
        land$spp[land$cell.id %in% urban.cells] <- clim$spp[clim$cell.id %in% urban.cells] <- 20 
        land$spp[land$cell.id %in% water.cells] <- clim$spp[clim$cell.id %in% urban.cells] <- 19
        land$spp[land$cell.id %in% grass.cells] <- clim$spp[clim$cell.id %in% urban.cells] <- 15
        land$spp[land$cell.id %in% shrub.cells] <- clim$spp[clim$cell.id %in% urban.cells] <- 14
        land$biom[land$cell.id %in% c(urban.cells, water.cells, grass.cells)] <- NA
        land$biom[land$cell.id %in% shrub.cells] <- 0
        land$age[land$cell.id %in% c(urban.cells, water.cells, grass.cells)] <- NA
        land$age[land$cell.id %in% shrub.cells] <- 0
        land$tsdist[land$cell.id %in% c(urban.cells, water.cells)] <- NA
        land$tsdist[land$cell.id %in% c(grass.cells, shrub.cells)] <- 0
        land$typdist[land$cell.id %in% grass.cells] <- "lchg.agri"
        land$typdist[land$cell.id %in% shrub.cells] <- "lchg.rabn"
        land$tburnt[land$cell.id %in% c(urban.cells, water.cells)] <- NA
        land$tburnt[land$cell.id %in% c(grass.cells, shrub.cells)] <- 0
        clim$sdm[land$cell.id %in% c(urban.cells, water.cells, grass.cells)] <- NA
        clim$sqi[land$cell.id %in% c(urban.cells, water.cells, grass.cells)] <- NA
        ## Update sdm and sqi for shrublands
        clim$sdm[clim$cell.id %in% shrub.cells] <- 1
        if(length(shrub.cells)>0){
          sqi.shrub <- filter(clim, cell.id %in% shrub.cells) %>% select(spp, temp, precip) %>% 
                       mutate(aux.brolla=site.quality.shrub$c0_brolla+site.quality.shrub$c_temp_brolla*temp+site.quality.shrub$c_temp2_brolla*temp*temp+site.quality.shrub$c_precip_brolla*precip+site.quality.shrub$c_precip2_brolla*precip*precip,
                              aux.maquia=site.quality.shrub$c0_maquia+site.quality.shrub$c_temp_maquia*temp+site.quality.shrub$c_temp2_maquia*temp*temp+site.quality.shrub$c_precip_maquia*precip+site.quality.shrub$c_precip2_maquia*precip*precip,
                              aux.boix=site.quality.shrub$c0_boix+site.quality.shrub$c_temp_boix*temp+site.quality.shrub$c_temp2_boix*temp*temp+site.quality.shrub$c_precip_boix*precip+site.quality.shrub$c_precip2_boix*precip*precip,
                              sq.brolla=1/(1+exp(-1*aux.brolla)), sq.maquia=1/(1+exp(-1*aux.maquia)), sq.boix=1/(1+exp(-1*aux.boix))) %>% 
                       mutate(sqest.brolla=sq.brolla/max(sq.brolla), sqest.maquia=sq.maquia/max(sq.maquia), sqest.boix=sq.boix/max(sq.boix),
                              sqi=ifelse(sqest.brolla>=sqest.maquia & sqest.brolla>=sqest.boix, 1,
                                    ifelse(sqest.maquia>=sqest.brolla & sqest.maquia>=sqest.boix, 2,
                                      ifelse(sqest.boix>=sqest.brolla & sqest.boix>=sqest.maquia, 3, 0))))
          clim$sqi[clim$cell.id %in% shrub.cells] <- sqi.shrub$sqi
        }
        ## Change in the base dataframe, to not repeat
        land.cover.changes$code[land.cover.changes$cell.id %in% urban.cells] <- 2020
        land.cover.changes$code[land.cover.changes$cell.id %in% water.cells] <- 1919
        land.cover.changes$code[land.cover.changes$cell.id %in% grass.cells] <- 1515
        land.cover.changes$code[land.cover.changes$cell.id %in% shrub.cells] <- 1414
        
        if(any(is.infinite(sqi.shrub$sq.brolla)) | any(is.na(sqi.shrub$sq.brolla))){
          write.table(sqi.shrub, paste0(out.path, "/ErrorSQIshrub.txt"), quote=F, row.names=F, sep="\t")
          stop("Error SQI shrub")
        }
        
      }
      if(is.land.cover.change & t %in% lchg.schedule){
        # Urbanization
        chg.cells <- land.cover.change(land, coord, interface, 1, t, numeric())
        land$spp[land$cell.id %in% chg.cells] <- clim$spp[clim$cell.id %in% chg.cells] <- 20 # urban
        land$biom[land$cell.id %in% chg.cells] <- NA
        land$age[land$cell.id %in% chg.cells] <- NA
        land$tsdist[land$cell.id %in% chg.cells] <- NA  # don't care the time since it's urban
        land$typdist[land$cell.id %in% chg.cells] <- "lchg.urb"
        land$tburnt[land$cell.id %in% chg.cells] <- NA
        clim$sdm[clim$cell.id %in% chg.cells] <- clim$sqi[clim$cell.id %in% chg.cells] <- NA
        # Agriculture conversion
        visit.cells <- chg.cells
        chg.cells <- land.cover.change(land, coord, interface, 2, t, visit.cells)
        land$spp[land$cell.id %in% chg.cells]<- clim$spp[clim$cell.id %in% chg.cells] <- 16 # arableland or 17 - permanent crops
        land$biom[land$cell.id %in% chg.cells] <- NA
        land$age[land$cell.id %in% chg.cells] <- NA
        land$typdist[land$cell.id %in% chg.cells] <- "lchg.agri"
        land$tsdist[land$cell.id %in% chg.cells] <- 0
        land$tburnt[land$cell.id %in% chg.cells] <- 0
        clim$sdm[clim$cell.id %in% chg.cells] <- clim$sqi[clim$cell.id %in% chg.cells] <- NA
        # Rural abandonment
        visit.cells <- c(visit.cells, chg.cells)
        chg.cells <- land.cover.change(land, coord, interface, 3, t, visit.cells)
        land$spp[land$cell.id %in% chg.cells] <- clim$spp[clim$cell.id %in% chg.cells] <- 14  # shrub
        land$biom[land$cell.id %in% chg.cells] <- 0
        land$age[land$cell.id %in% chg.cells] <- 0
        land$typdist[land$cell.id %in% chg.cells] <- "lchg.rabn"
        land$tsdist[land$cell.id %in% chg.cells] <- 0
        land$tburnt[land$cell.id %in% chg.cells] <- 0
        clim$sdm[clim$cell.id %in% chg.cells] <- 1
        sqi.shrub <- filter(clim, cell.id %in% chg.cells) %>% select(spp, temp, precip) %>% 
                     mutate(aux.brolla=site.quality.shrub$c0_brolla+site.quality.shrub$c_temp_brolla*temp+site.quality.shrub$c_temp2_brolla*temp*temp+site.quality.shrub$c_precip_brolla*precip+site.quality.shrub$c_precip2_brolla*precip*precip,
                            aux.maquia=site.quality.shrub$c0_maquia+site.quality.shrub$c_temp_maquia*temp+site.quality.shrub$c_temp2_maquia*temp*temp+site.quality.shrub$c_precip_maquia*precip+site.quality.shrub$c_precip2_maquia*precip*precip,
                            aux.boix=site.quality.shrub$c0_boix+site.quality.shrub$c_temp_boix*temp+site.quality.shrub$c_temp2_boix*temp*temp+site.quality.shrub$c_precip_boix*precip+site.quality.shrub$c_precip2_boix*precip*precip,
                            sq.brolla=1/(1+exp(-1*aux.brolla)), sq.maquia=1/(1+exp(-1*aux.maquia)), sq.boix=1/(1+exp(-1*aux.boix))) #%>% 
        if(is.infinite(max(sqi.shrub$sq.brolla)) | is.infinite(max(sqi.shrub$sq.maquia))  | is.infinite(max(sqi.shrub$sq.boix)) ){
          write.table(sqi.shrub, paste0(out.path, "/ErrorSQIshrub.txt"), quote=F, row.names=F, sep="\t")
          cat("INF in sqi.shrub - SIM.LC.CHANGE")  
        }
        sqi.shrub <- mutate(sqi.shrub, sqest.brolla=sq.brolla/max(sq.brolla), sqest.maquia=sq.maquia/max(sq.maquia), sqest.boix=sq.boix/max(sq.boix),
                            sqi=ifelse(sqest.brolla>=sqest.maquia & sqest.brolla>=sqest.boix, 1,
                                  ifelse(sqest.maquia>=sqest.brolla & sqest.maquia>=sqest.boix, 2,
                                   ifelse(sqest.boix>=sqest.brolla & sqest.boix>=sqest.maquia, 3, 0))))
        clim$sqi[clim$cell.id %in% chg.cells] <- sqi.shrub$sqi
        
        if(any(is.infinite(sqi.shrub$sq.brolla)) | any(is.na(sqi.shrub$sq.brolla))){
          write.table(sqi.shrub, paste0(out.path, "/ErrorSQIshrub.txt"), quote=F, row.names=F, sep="\t")
          stop("Error SQI shrub")
        }
        
        # Update interface values
        interface <- update.interface(land)
      }
      
      
      ## 3. FOREST MANAGEMENT (under development)
      if(is.harvest & t %in% mgmt.schedule){
        cut.out <- sustainable.mgmt(land, harvest, clim, t)
        sustain <- cut.out$sustain
        extracted.sawlog <- cut.out$extracted.sawlog
        extracted.wood <- cut.out$extracted.wood
        # track the cells that have been cut
        land$typdist[land$cell.id %in% c(extracted.sawlog$cell.id, extracted.wood$cell.id)] <- "cut"
        land$tsdist[land$cell.id %in% c(extracted.sawlog$cell.id, extracted.wood$cell.id)] <- 
          land$tscut[land$cell.id %in% c(extracted.sawlog$cell.id, extracted.wood$cell.id)] <- 0
        # track the type of intervention (e.g. thinning, prep.cut, seed.cut, removal.cut)
        land$typcut[land$cell.id %in% extracted.sawlog$cell.id] <- extracted.sawlog$todo
        land$typcut[land$cell.id %in% extracted.wood$cell.id] <- extracted.wood$todo
        # change the age of the cells after removal.cut.
        # wood is extracted in quercus stands by clear.cut
        land$age[land$cell.id %in% extracted.wood$cell.id] <- 0 
        # sawlogs are extracted in conifer stands under a shelterwood sytem.
        # after the removal.cut, the stand is in regeneration and it already has 10 years.
        land$age[land$cell.id %in% extracted.sawlog$cell.id[extracted.sawlog$spp %notin% c(8,10,11)]] <- 9 # sum 1 at the end of the year
        land$age[land$cell.id %in% extracted.sawlog$cell.id[extracted.sawlog$spp %in% c(8,10,11)]] <- 0 # quercus are clear cut
        # change the basal area in harvested stands
        land$biom[land$cell.id %in% extracted.sawlog$cell.id] <- 
          land$biom[land$cell.id %in% extracted.sawlog$cell.id]-extracted.sawlog$ba.extract*10
        land$biom[land$cell.id %in% extracted.wood$cell.id] <- 
          land$biom[land$cell.id %in% extracted.wood$cell.id]-extracted.wood$ba.extract*10
        # after removal.cut make explicity that basal area is 0
        land$biom[land$cell.id %in% extracted.sawlog$cell.id[extracted.sawlog$todo=="removal.cut"] ] <- 0
        # but there's regeneration of 9 year old in areas harvested under shelterwood
        for(i in 1:9)
          land$biom[land$cell.id %in% extracted.sawlog$cell.id[extracted.sawlog$todo=="removal.cut" & extracted.sawlog$spp %notin% c(8,10,11)]] <- 
              growth(land[land$cell.id %in% extracted.sawlog$cell.id[extracted.sawlog$todo=="removal.cut" & extracted.sawlog$spp %notin% c(8,10,11)],], clim, paste("Cohort age", i))
        
        # count the vol extracted per each spp
        aux <- rbind(group_by(extracted.sawlog,spp) %>% summarize(vol.sawlog=sum(vol.extract.sawlog), vol.wood=sum(vol.extract.wood)),
                     group_by(extracted.wood,spp) %>% summarize(vol.sawlog=0, vol.wood=sum(vol.extract.sawlog+vol.extract.wood)))
        track.harvest <- rbind(track.harvest, data.frame(run=irun, year=t, 
          group_by(aux, spp) %>% summarize(vol.sawlog=round(sum(vol.sawlog),1), vol.wood=round(sum(vol.wood),1)) ))
      }
      
      
      ## 4. FIRE
      if(spin.up & t<=10){
        cat("Observed wildfires", "\n")
        a <- !is.na(wildfires[,t+1])
        burnt.cells <- data.frame(cell.id=wildfires$cell.id[a], fintensity=1)
        ## Make it effectively burnt
        land$tsdist[land$cell.id %in% burnt.cells$cell.id] <- 0
        land$tburnt[land$cell.id %in% burnt.cells$cell.id] <- land$tburnt[land$cell.id %in% burnt.cells$cell.id] + 1
        land$typdist[land$cell.id %in% burnt.cells$cell.id] <- "highfire"
        land$biom[land$cell.id %in% burnt.cells$cell.id] <- 0
      }
      if(is.wildfire & t %in% fire.schedule){
        # Decide climatic severity of the year (default is mild)
        clim.sever <- 0
        if(runif(1,0,100) < clim.severity[clim.severity$year==t, ncol(clim.severity)]) # not-mild
          clim.sever <- 1
        # Burnt
        fire.out <- fire.regime(land, coord, orography, clim, interface, pfst.pwind,
                                1:3, clim.sever, t, 0, MASK, out.path, irun, rpb)
        # Track fires and Burnt spp & Biomass
        if(nrow(fire.out[[1]])>0)
          track.fire <- rbind(track.fire, data.frame(run=irun, fire.out[[1]]))
        burnt.cells <- fire.out[[2]] %>% select(-igni)
        if(nrow(burnt.cells)>0){
          aux <- left_join(burnt.cells, select(land, cell.id, spp, biom), by="cell.id") %>%
                  mutate(bburnt=ifelse(fintensity>fire.intens.th, biom, biom*(1-fintensity))) %>%
                  group_by(fire.id, spp) %>% summarize(aburnt=length(spp), bburnt=round(sum(bburnt, na.rm=T),1))
          track.fire.spp <-  rbind(track.fire.spp, data.frame(run=irun, year=t, aux)) 
          # track.step <- rbind(track.step, data.frame(run=irun, fire.out[[3]]))
          # track.sr <- rbind(track.sr, data.frame(run=irun, fire.out[[3]]))
        }
        # if(nrow(fire.out[[4]])>0)
        #   track.sr.source <- rbind(track.sr.source, data.frame(run=irun, fire.out[[4]]))
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
      }
      
      
      ## 5. PRESCRIBED BURNS
      id.fire <- 0
      if(is.prescribed.burn & t %in% pb.schedule){
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
        }
      }
      
      
      ## 6. DROUGHT
      killed.cells <- integer()
      if(is.drought & t %in% drought.schedule){
        killed.cells <- drought(land, clim, decade, t)
          # ## detect SDM=0 after the 1st decade when there is no climate change
          # if(scn.name=="Scn_NULL" & t>10 & length(killed.cells)>0){
          #   write.table(land, paste0(out.path, "/ErrorSDMland_", irun, "_", t, ".txt"), quote=F, row.names=F, sep="\t")
          #   write.table(clim, paste0(out.path, "/ErrorSDMclim_", irun, "_", t, ".txt"), quote=F, row.names=F, sep="\t")
          #   cat("SDM=0", "\n")
          # }
        land$tsdist[land$cell.id %in% killed.cells] <- 0
        land$typdist[land$cell.id %in% killed.cells] <- "drght"
        if(length(killed.cells)>0)
          track.drougth <- rbind(track.drougth, data.frame(run=irun, year=t, 
                                 filter(land, cell.id %in% killed.cells) %>% group_by(spp) %>% summarize(ha=length(spp))))
      }
      
      
      ## 7. POST-FIRE REGENERATION
      if(is.postfire & t %in% post.fire.schedule){
        ## forest transition of tree species burnt in high intensity
        aux  <- post.fire(land, coord, orography, clim, sdm)
        if(nrow(aux)>0){
          spp.out <- land$spp[land$cell.id %in% aux$cell.id]
          land$spp[land$cell.id %in% aux$cell.id] <- aux$spp
          clim$spp[clim$cell.id %in% aux$cell.id] <- aux$spp
          clim$sdm[clim$cell.id %in% aux$cell.id] <- 1
          clim$sqi[clim$cell.id %in% aux$cell.id] <- aux$sqi
          track <-  data.frame(table(spp.out, aux$spp))
          names(track) <- c("spp.out", "spp.in", "ha")
          track.post.fire <- rbind(track.post.fire, data.frame(run=irun, year=t, track))  
        }
        # Reset age of cells burnt in high intensity
        land$age[land$cell.id %in% burnt.cells$cell.id[burnt.cells$intens] & !is.na(land$spp) & land$spp<=14] <- 0
      }
      
      
      ## 8. COHORT ESTABLISHMENT
      if(is.cohort.establish & t %in% cohort.schedule & length(killed.cells)>0){
        aux <- cohort.establish(land, coord, orography, clim, sdm)
        spp.out <- land$spp[land$cell.id %in% killed.cells]
        land$spp[land$cell.id %in% killed.cells] <- aux$spp
        land$age[land$cell.id %in% killed.cells] <- t-(decade-10)-1 ## 0 to 9, so after growth(), age is 1 to 10
        land$biom[land$cell.id %in% killed.cells] <- 0  ## biomass at 0,
        if(t-(decade-10)-1 > 0){ ## increase biomass up to cohort.age1, and it will increase in growth() one year more
          for(i in 1:(t-(decade-10)-1))
            land$biom[land$cell.id %in% killed.cells] <- 
              growth(land[land$cell.id %in% killed.cells,], clim, paste("Cohort", i))
        }
        clim$spp[clim$cell.id %in% killed.cells] <- aux$spp
        clim$sdm[clim$cell.id %in% killed.cells] <- 1
        clim$sqi[clim$cell.id %in% killed.cells] <- aux$sqi
        track <- data.frame(table(spp.out, aux$spp))
        names(track) <- c("spp.out", "spp.in", "ha")
        track.cohort <- rbind(track.cohort, data.frame(run=irun, year=t, track))
      }
      
      
      ## 9. AFFORESTATION
      if(is.afforestation & t %in% afforest.schedule){
        aux  <- afforestation(land, coord, orography, clim, sdm)
        land$spp[land$cell.id %in% aux$cell.id] <- aux$spp
        land$biom[land$cell.id %in% aux$cell.id] <- 0
        land$age[land$cell.id %in% aux$cell.id] <- 0
        land$tsdist[land$cell.id %in% aux$cell.id] <- 0
        land$typdist[land$cell.id %in% aux$cell.id] <- "afforest"
        clim$spp[clim$cell.id %in% aux$cell.id] <- aux$spp
        clim$sdm[clim$cell.id %in% aux$cell.id] <- 1
        clim$sqi[clim$cell.id %in% aux$cell.id] <- aux$sqi
        track <- data.frame(table(aux$spp))
        names(track) <- c("spp", "ha")
        track.afforest <- rbind(track.afforest, data.frame(run=irun, year=t, track))
      }
      
      
      ## 10. GROWTH
      if(is.growth & t %in% growth.schedule){
        land$biom <- growth(land, clim, "Species")
        land$age <- pmin(land$age+1,600)
        land$tsdist <- pmin(land$tsdist+1,600)
        land$tscut <- pmin(land$tscut+1,600)
        aux.forest <- filter(land, spp<=13) %>% select(spp, age, biom) %>% left_join(eq.ba.vol, by="spp") %>% 
                      mutate(vol=cx*biom/10+cx2*biom*biom/100) %>% select(-cx, -cx2) %>%
                      left_join(eq.ba.volbark, by="spp") %>% 
                      mutate(volbark=cx*biom/10+cx2*biom*biom/100) %>% select(-cx, -cx2) %>% 
                      left_join(eq.ba.carbon, by="spp") %>% 
                      mutate(carbon=c*biom/10) %>% 
                      mutate(age.class=ifelse(spp<=7 & age<=15, "young", ifelse(spp<=7 & age<=50, "mature",
                             ifelse(spp<=7 & age>50, "old", ifelse(spp>7 & spp<=13 & age<=15, "young",
                             ifelse(spp>7 & spp<=13 & age<=50, "mature", "old")))))) %>%       
                      group_by(spp, age.class) %>% select(-c) %>%
                      summarise(area=length(vol), vol=sum(vol), volbark=sum(volbark), carbon=sum(carbon))  
        aux.shrub <- filter(land, spp==14) %>% select(spp, biom) %>% group_by(spp) %>%
                     summarise(age.class=NA, area=length(biom), vol=sum(biom), volbark=0, carbon=0)  
        aux.other <- filter(land, spp>14) %>% select(spp) %>% group_by(spp) %>%
                     summarise(age.class=NA, area=length(spp), vol=0, volbark=0, carbon=0)  
        track.land <- rbind(track.land, data.frame(run=irun, year=t, aux.forest), data.frame(run=irun, year=t, aux.shrub),
                            data.frame(run=irun, year=t, aux.other))
      }
      
      
      ## Print maps every time step with ignition and low/high intenstiy burnt
      if(write.maps & t %in% seq(write.freq, time.horizon, write.freq)){
        cat("... writing maps", "\n")
        save(land, file=paste0(out.path, "/rdata/land_r", irun, "t", t, ".rdata"))
        # MAP <- MASK; MAP[!is.na(MASK[])] <- land$spp
        # writeRaster(MAP, paste0(out.path, "/lyr/Spp_r", irun, "t", t, ".tif"), format="GTiff", overwrite=T)
        # MAP <- MASK; MAP[!is.na(MASK[])] <- land$biom
        # writeRaster(MAP, paste0(out.path, "/lyr/Biom_r", irun, "t", t, ".tif"), format="GTiff", overwrite=T)
        # MAP <- MASK; MAP[!is.na(MASK[])] <- land$age
        # writeRaster(MAP, paste0(out.path, "/lyr/Age_r", irun, "t", t, ".tif"), format="GTiff", overwrite=T)
        # MAP <- MASK
        # MAP[!is.na(MASK[])] <- ifelse(land$typdist %in% c("lchg.urb", "lchg.crp", "lchg.nat"), 1, 
        #                               ifelse(land$typdist == "cut", 2, 
        #                                      ifelse(land$typdist %in% c("highfire", "lowfire"), 3,   
        #                                             ifelse(land$typdist == "pb", 4,
        #                                                    ifelse(land$typdist == "drght", 5,
        #                                                           ifelse(land$typdist == "afforest", 6, NA))))))
        # writeRaster(MAP, paste0(out.path, "/lyr/TypeDist_r", irun, "t", t, ".tif"), format="GTiff", overwrite=T)
      }
      
      write.table(track.harvest[-1,], paste0(out.path, "/Harvest.txt"), quote=F, row.names=F, sep="\t")
      
    } # time
  
    ## Overwrite text outputs each run, at the end of the simulation period
    ## To save some runs in case R closes before finishing all the runs
    cat("... writing outputs", "\n")
    write.table(track.harvest[-1,], paste0(out.path, "/Harvest.txt"), quote=F, row.names=F, sep="\t")
    write.table(track.fire[-1,], paste0(out.path, "/Fires.txt"), quote=F, row.names=F, sep="\t")
    write.table(track.fire.spp[-1,], paste0(out.path, "/BurntSpp.txt"), quote=F, row.names=F, sep="\t")
    # write.table(track.step[-1,], paste0(out.path, "/FiresStep.txt"), quote=F, row.names=F, sep="\t")
    # write.table(track.sr[-1,], paste0(out.path, "/FireSprd.txt"), quote=F, row.names=F, sep="\t")
    # write.table(track.sr.source[-1,], paste0(out.path, "/FireSprdSource.txt"), quote=F, row.names=F, sep="\t")
    write.table(track.pb[-1,], paste0(out.path, "/PrescribedBurns.txt"), quote=F, row.names=F, sep="\t")
    write.table(track.drougth[-1,], paste0(out.path, "/Drought.txt"), quote=F, row.names=F, sep="\t")
    write.table(track.post.fire[-1,], paste0(out.path, "/PostFire.txt"), quote=F, row.names=F, sep="\t")
    track.cohort <- filter(track.cohort, ha>0)
    write.table(track.cohort[-1,], paste0(out.path, "/Cohort.txt"), quote=F, row.names=F, sep="\t")
    write.table(track.afforest[-1,], paste0(out.path, "/Afforestation.txt"), quote=F, row.names=F, sep="\t")
    write.table(track.land[-1,], paste0(out.path, "/Land.txt"), quote=F, row.names=F, sep="\t")
    
    # Print maps at the end of the simulation period per each run
    # if(write.maps){
    #   MAP <- MASK
    #   MAP[!is.na(MASK[])] <- land$tburnt
    #   writeRaster(MAP, paste0(out.path, "/lyr/TimesBurnt_r", irun, ".tif"), format="GTiff", overwrite=T)
    # }

  } # run
  

}
