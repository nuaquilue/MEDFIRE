######################################################################################
## In "preparatory" harvesting, between 65% - 85% of harvesting goes for sawlogs, the remainder goes for wood
######################################################################################

forest.mgmt <- function(land, harvest, clim, t, out.path, MASK){
  
  ## Tracking
  cat("Forest Management")
  
  ## Function to select items not in a vector
  `%notin%` <- Negate(`%in%`)
  
  ## Read (1) Harvesting rules, (2) Eq. of basal area to volume, and (3) Sawlog and Wood demands
  rules <- read.table("inputfiles/MgmtRules.txt", header=T) %>% select(-name, -orgest)
  eq.ba.vol <- read.table("inputfiles/EqBasalAreaVol.txt", header=T)
  # eq.ba.volbark <- read.table("inputfiles/EqBasalAreaVolWithBark.txt", header=T)
  dmnd <- read.table(paste0("inputfiles/", file.dmnd.harvest, ".txt"), header=T)
  dmnd.sawlog <- dmnd$Sawlogs[t]
  dmnd.wood <- dmnd$Primary[t]
  # dmnd.sawlog <- 500000; dmnd.wood <- 100000
  
  ## Track cut cells
  track.cut.cells <- data.frame(cut.id=NA, cell.id=NA, spp=NA, cut.sawlog=NA, typcut=NA, 
                                pextract=NA, pwood=NA, vol.sawlog=NA, vol.wood=NA, ba.extract=NA)
  cut.id <- 0
  visit.cells <- integer()
  
  ## Num of neighbours in a Moore neighbourhood. This neighbourhood is applied when computing volume around 
  ## a target cell, and to spread the harvesting intervention into.
  default.neigh <- data.frame(x=c(-1,1,2900,-2900,2899,-2901,2901,-2899))
  default.nneigh <- nrow(default.neigh)
  
  ## To be sure that non-harvestable covers are cut (no-forest and qsuber,  not managed for sawlogs neither wood)
  i <- land$spp<=13 & land$spp!=9      # 1.315.162 at t=1
  subland <- land[i,]   
  
  
  ## Find cells suitable for management: forest with slope.pctg <=30% and dist.path <= 500m
  ## All tree species are harvestable exept Qsuber
  lambda <- 1/200
  suit.mgmt <- left_join(select(subland, -typdist,-tsdist, -tburnt), harvest, by="cell.id") %>%
               filter(slope.pctg <= 30) %>%                  # exclude by slope > 30%
               filter(enpe %notin% c(1,3,4,5)) %>%           # exclude 'parc nacional', 'paratge natural interès nacional', 'reserva natural fauna salvatge', and 'reseva natural integral'
               mutate(ppath=exp(-dist.path*lambda))          # dist - ppath: 0 - 1, 100 - 0.6065, 141 - 0.493, 200 - 0.36788
  suit.mgmt$access <- suit.mgmt$ppath >= runif(nrow(suit.mgmt), 0, 0)     ## with upper th 0.49, ~10% is not accessible
  suit.mgmt <- filter(suit.mgmt, access) 
  areas.mgmt <- group_by(suit.mgmt, spp) %>% summarise(suitable=length(spp))
  # plot cells suitable for mgmt
  dta <- data.frame(cell.id=1:ncell(MASK)) %>% left_join(select(suit.mgmt, cell.id, spp), by="cell.id")
  MASK[] <- dta$spp; plot(MASK, col=rainbow(12))
  writeRaster(MASK, paste0(out.path, "/lyr/suit.mgmt_t", t, ".tif"), format="GTiff", overwrite=T)
  
      # suit.mgmt <- left_join(select(subland, -typdist,-tsdist, -tburnt), harvest, by="cell.id") %>%
      #   filter(slope.pctg <= 30) %>%                  # exclude by slope > 30%
      #   filter(enpe %notin% c(1,3,4,5)) 
      # writeRaster(MASK, paste0(out.path, "/lyr/suit.mgmt_NOPATH_t", t, ".tif"), format="GTiff", overwrite=T)
  
  ## Find patches using a 8-neigbour rule, to have an idea of the size of patches that can be harvested
  MASK[!is.na(MASK[])] <- 1
  CLUSTER <- clump(MASK)
  # plot(CLUSTER, col=plasma(max(CLUSTER[], na.rm=T)))
  ## Build a data frame with cell coordinates and cluster id
  df <- data.frame(cell.id=1:ncell(CLUSTER), coordinates(CLUSTER), clust=getValues(CLUSTER))
  ptch <- filter(df, !is.na(clust)) %>% group_by(clust) %>% summarise(xm=mean(x), ym=mean(y), size=length(clust)) 
  head(ptch)
  
  ## Find those forest cells that can be sustainably harvested
  sustain <- filter(subland, cell.id %in% suit.mgmt$cell.id) %>% select(-typdist,-tsdist, -tburnt) %>% 
             left_join(select(clim, cell.id, sqi), by="cell.id") %>% 
             left_join(rules, by=c("spp", "sqi")) 
  
  # Determine which type of intervention to do
  sustain$todo[is.na(sustain$typcut)] <- 
            ifelse(is.na(sustain$minage.prep[is.na(sustain$typcut)]),
                   ifelse(is.na(sustain$minage.diss[is.na(sustain$typcut)]), "fin", "diss"), "prep")
  sustain$todo[!is.na(sustain$typcut) & sustain$typcut=="prep"] <- "diss"
  sustain$todo[!is.na(sustain$typcut) & sustain$typcut=="diss"] <- "fin"
  sustain$todo[!is.na(sustain$typcut) & sustain$typcut=="fin"] <- "fin"

  # min age and min ab
  sustain$exclude.age <- ifelse(sustain$todo=="prep", sustain$minage.prep,
                                ifelse(sustain$todo=="diss", sustain$minage.diss, sustain$minage.fin))
  sustain$exclude.ab <- ifelse(sustain$todo=="prep", sustain$minab.prep,
                                ifelse(sustain$todo=="diss", sustain$minab.diss, sustain$minab.fin))
  sustain$exclude.tafter <- ifelse(sustain$todo=="prep", 0,
                               ifelse(sustain$todo=="diss", sustain$tafter.diss, sustain$tafter.fin))
  
  # Exclude those locations that do not verify age nor ab minimal conditions
  sustain$exclude <- F
   # sustain$exclude <- sustain$age<sustain$exclude.age & sustain$biom<sustain$exclude.ab*10  # decimals
  # reconsider "excludes", time after cut 
      # sustain$exclude[!is.na(sustain$tscut)] <- sustain$exclude[!is.na(sustain$tscut)] & 
      #   (sustain$tscut[!is.na(sustain$tscut)] < sustain$exclude.tafter[!is.na(sustain$tscut)])  
  # when there's no minab, force at least, verify age condition (e.g. phalepensis - sqi1, ppinea - sqi1, ...)
  # sustain$exclude[sustain$exclude.ab==0] <- sustain$age[sustain$exclude.ab==0]<sustain$exclude.age[sustain$exclude.ab==0] 
  sustain <- filter(sustain, !exclude) %>% select(-exclude)
  
  # Retain the ptcg of extraction and thab according to the 'todo' prescription 
  sustain$pctgextract <- ifelse(sustain$todo=="prep", sustain$pctgextract.prep,
                                ifelse(sustain$todo=="diss", sustain$pctgextract.diss, sustain$pctgextract.fin))
  sustain$thab <- ifelse(sustain$todo=="prep", sustain$thab.prep,
                         ifelse(sustain$todo=="diss", sustain$thab.diss, 0))
  aux <- group_by(sustain, spp) %>% summarise(sustain=length(spp))
  areas.mgmt <- left_join(areas.mgmt, aux, by="spp") %>% mutate(psustain=round(100*sustain/suitable))
  # plot cells sustainable mgmt
  dta <- data.frame(cell.id=1:ncell(MASK)) %>% left_join(select(sustain, cell.id, spp), by="cell.id")
  MASK[] <- dta$spp
  writeRaster(MASK, paste0(out.path, "/lyr/sustain.mgmt_t", t, ".tif"), format="GTiff", overwrite=T)
  
  ## Compute the volume that can be extracted in locations where harvesting is sustainable
  volume <- select(sustain, cell.id, spp, biom, pctgextract) %>% 
            left_join(eq.ba.vol, by="spp") %>% 
            mutate(vol=cx*biom/10+cx2*biom*biom/100) %>% select(-cx, -cx2) %>% 
            mutate(vol.extract = vol*pctgextract/100) 
            # mutate(vol.fake=ifelse(spp<=7, vol.extract*5, vol.extract))
  aux <- group_by(volume, spp) %>% summarise(vol.extract=sum(vol.extract))
  areas.mgmt <- left_join(areas.mgmt, aux, by="spp")
  

  ## To compute the probability of being harvested first, compute the volume available in the neighborhodd
  neigh.id <- data.frame(cell.id=as.integer(rep(volume$cell.id, each=default.nneigh)+
                                            rep(default.neigh$x, nrow(volume))),
                         source.id=rep(volume$cell.id, each=default.nneigh)) %>% 
              left_join(volume, by="cell.id") %>% group_by(source.id) %>% 
              summarise(vol.neigh=sum(vol.extract, na.rm=T))
              # summarise(vol.neigh=sum(vol.fake, na.rm=T))
  
  
  ## Set probability of sawlog extraction according to (1) volume available for extraction, 
  ## (2) volume available for extraction in the neighborhood, (3) distance to forest industries, and
  ## (4) type of species, conifer - 1 vs. deciduous - 0.
  w1 <- 0.4; w2 <- 0.2; w3 <- 0; w4 <- 0.4
  # w1 <- 0.3; w2 <- 0.3; w3 <- 0; w4 <- 0.4
  # w1 <- w2 <- w3 <- 0; w4 <- 1           
  pextract <- left_join(volume, neigh.id, by=c("cell.id"="source.id")) %>% 
              left_join(select(harvest, cell.id, dist.industry), by="cell.id") %>% 
              mutate(f1=scales::rescale(pmin(vol.extract, quantile(vol.extract, p=0.9)), to=c(0,1)) ,
              # mutate(f1=scales::rescale(pmin(vol.fake, quantile(vol.fake, p=0.9)), to=c(0,1)) ,
                     f2=scales::rescale(pmin(vol.neigh, quantile(vol.neigh, p=0.9)), to=c(0,1)) ,
                     f3=scales::rescale(pmin(1/dist.industry, quantile(1/dist.industry, p=0.9)), to=c(0,1)),
                     f4=ifelse(spp<=7,1,0) ) %>% 
              mutate(p=w1*f1+w2*f2+w3*f3+w4*f4)
        # plot probability of extraction
        dta <- data.frame(cell.id=1:ncell(MASK)) %>% left_join(select(pextract, cell.id, p))
        MASK[] <- dta$p
        # plot(MASK, col=viridis(6))
        writeRaster(MASK, paste0(out.path, "/lyr/pextract2_t", t, ".tif"), format="GTiff", overwrite=T)
  
  ## Finally, adjust the demands according to the available volume
  dmnd.sawlog <- min(dmnd.sawlog, sum(volume$vol.extract))
  dmnd.wood <- min(dmnd.wood, sum(volume$vol.extract)-dmnd.sawlog)
  
  
  ## Let's start harvesting for sawlogs and wood
  more.wood <- T
  while(dmnd.sawlog>0){
    
    ## ID for each harvesting event, and restart step
    cut.id <- cut.id+1; step <- 1
    
    ## Select target size of the intervention (instead to be area-based could be vol-based)  ## THINK ABOUT IT
    target.area <- round(pmin(rlnorm(1, meanlog=3, sdlog = 0.6), 60))
    
    ## Select an ignition point where to start the harvesting intervention
    igni.id <- sample(pextract$cell.id, 1, replace=F, pextract$p)
    
    ## Harvest this first cell and decrease overall demand (later on, if it is already reached, stop cutting
    area.cut <- 1
    aux <- filter(sustain, cell.id==igni.id) %>% left_join(eq.ba.vol, by="spp") %>% 
           mutate(vol=cx*biom/10+cx2*biom*biom/100, vol.extract=vol*pctgextract/100,
                  pwood=ifelse(todo=="fin", runif(1, 1-0.95, 1-0.9), runif(1, 1-0.85, 1-0.65)))
    
    ## Decide if harvesting for Sawlog or for Wood  (I think I'll have to adjust these thresholds)
    cut.sawlog <- T
    if(more.wood)
       cut.sawlog <- ifelse(aux$spp<=7, runif(1, 0, 1)<=runif(1, 0.75, 0.9), runif(1, 0, 1)<=runif(1, 0.1, 0.25))
      # cut.sawlog <- ifelse(aux$spp<=7, TRUE, runif(1, 0, 1)<=runif(1, 0.1, 0.25))
    
    ## If harvesting for sawlogs, 
    if(cut.sawlog){
      # and need more wood, allocate few of the harvesting to wood
      if(more.wood){
        dmnd.sawlog <- dmnd.sawlog - aux$vol.extract * (1-aux$pwood) 
        dmnd.wood <- dmnd.wood - aux$vol.extract * aux$pwood   ## dmnd wood is in m3 or t. If tones, need to covert volume to mass!!!  
      }
      # but if NO need more wood, then, allocate all to sawlogs
      else{
        dmnd.sawlog <- dmnd.sawlog - aux$vol.extract
        aux$pwood <- 0
      }
    }
    ## If harvesting for wood (more is needed, othwersie the before breaks already has been activated)
    else{
      if(more.wood){
        dmnd.wood <- dmnd.wood - aux$vol.extract
        aux$pwood <- 1  
      }
      else
        cut.id <- cut.id - 1
    }
    
    
    ## Track cutting this first cell of the patch
    if(cut.sawlog | (!cut.sawlog & more.wood)){
      front <- igni.id
      visit.cells <- c(visit.cells, igni.id)
      track.cut.cells <- rbind(track.cut.cells, 
                               data.frame(cut.id=cut.id, cell.id=igni.id, spp=aux$spp, cut.sawlog=cut.sawlog, 
                                          typcut=aux$todo, pextract=aux$pctgextract, pwood=aux$pwood,
                                          vol.sawlog=aux$vol.extract*(1-aux$pwood), vol.wood=aux$vol.extract*aux$pwood,
                                          ba.extract=aux$biom))
      pextract <- filter(pextract, cell.id!=igni.id)  ## remove that igni from the pool of selectable cells
    }
      
    
    ## Keep extracting wood?
    if(dmnd.wood<=0)
      more.wood <- F
    
    
    ## Cut as much as left of the target area to be harvested, but if sawlog demand is not reach,
    ## and if harvesting for wood, still it's needed more
    ## Otherwise, no need to keep cutting
    while(area.cut<target.area & dmnd.sawlog>0 & (more.wood & !cut.sawlog)){
      
      ## Increment tracking step
      step <- step + 1
      
      ## Build a data frame with the 8 neighbours of cells in the front, and
      ## filter cells that have not been visited yet.
      neigh.id <- data.frame(cell.id=as.integer(rep(front, each=default.nneigh)+ rep(default.neigh$x, length(front))),
                             source.id=rep(front, each=default.nneigh)) %>% filter(cell.id %notin% visit.cells) 
      
      ## Now find those neighbours that are currenty in Catalonia
      ## is_inCpp returns the position of neigh.id$cell.id in the 'land' data.frame (not the cell.id)!
      neigh.in.land <- is_inCpp(neigh.id$cell.id, subland$cell.id)
      i.land.in.neigh <- unique(neigh.in.land[which(neigh.in.land!=-1)])
      ## If all the available neighbours are out of Catalonia, stop spreading
      if(length(i.land.in.neigh)==0)
        break
      
      ## For all neighbours, find the intervention 'todo' and the 'pctgextract', and
      ## only keep those neighbours that can be harvested
      neigh.land <- subland[i.land.in.neigh,]  %>% left_join(select(sustain, cell.id, todo, pctgextract), by="cell.id") %>% 
        filter(!is.na(todo))
      if(nrow(neigh.land)==0)
        break
      
      ## Do harvesting and track it
      area.cut <- area.cut + nrow(neigh.land)
      aux <- filter(sustain, cell.id %in% neigh.land$cell.id) %>% left_join(eq.ba.vol, by="spp") %>% 
             mutate(vol=cx*biom/10+cx2*biom*biom/100, vol.extract=vol*pctgextract/100)
      aux$pwood <- runif(nrow(aux), 1-0.95, 1-0.9) * (aux$todo=="fin") +
                   runif(nrow(aux), 1-0.85, 1-0.65) * (aux$todo!="fin")
      
      ## If harvesting for sawlogs, 
      if(cut.sawlog){
        # and need more wood, allocate few of the harvesting to wood
        if(more.wood){
          dmnd.sawlog <- dmnd.sawlog - sum(aux$vol.extract*(1-aux$pwood))
          dmnd.wood <- dmnd.wood - sum(aux$vol.extract*aux$pwood)
        }
        # but if NO need more wood, then, allocate all to sawlogs
        else{
          dmnd.sawlog <- dmnd.sawlog - sum(aux$vol.extract)
          aux$pwood <- 0
        }
      }
      ## If harvesting for wood (more is needed)
      else{
        dmnd.wood <- dmnd.wood - sum(aux$vol.extract)
        aux$pwood <- 1
      }
      
      visit.cells <- c(visit.cells, neigh.land$cell.id)
      track.cut.cells <- rbind(track.cut.cells, 
                               data.frame(cut.id=cut.id, cell.id=aux$cell.id, spp=aux$spp, cut.sawlog=cut.sawlog, 
                                          typcut=aux$todo, pextract=aux$pctgextract, pwood=aux$pwood,
                                          vol.sawlog=aux$vol.extract*(1-aux$pwood), 
                                          vol.wood=aux$vol.extract*aux$pwood, ba.extract=aux$biom))
      pextract <- filter(pextract, cell.id %notin% aux$cell.id)  # remove harvested cells from the pool of selectable cells
      
      ## Keep harvesting. If there there are no cells in the front, stop trying to cut.
      front <- neigh.land$cell.id
      if(length(front)==0)
        break
      
      ## Keep extracting wood? If harvesting for wood, but no need more, then stop
      if(dmnd.wood<=0)
        more.wood <- F
    }
   
    print(paste("cut.id", cut.id, "- cut.sawlog", cut.sawlog,
                "- rem.dmnd.sawlog", round(dmnd.sawlog), "- rem.dmnd.wood", round(dmnd.wood)))
  }

  
  ## Cells harvested
  track.cut.cells <- track.cut.cells[-1,]
          
  
  #### *************  BY NOW, NO EXTRA WOOD TO EXTRACT. DISMISS THIS PART ************* ####
  # ## Update available volume to be extracted
  # volume <- filter(volume, cell.id %notin% track.cut.cells$cell.id)
  # 
  # ## To compute the probability of being harvested first, compute the volume available in the neighborhodd
  # neigh.id <- data.frame(cell.id=as.integer(rep(volume$cell.id, each=default.nneigh)+
  #                                             rep(default.neigh$x, nrow(volume))),
  #                        source.id=rep(volume$cell.id, each=default.nneigh)) %>% 
  #             left_join(volume, by="cell.id") %>% group_by(source.id) %>% 
  #             summarise(vol.neigh=sum(vol.extract, na.rm=T))
  # 
  # ## Set probability of *wood* extraction according to (1) volume available for extraction, 
  # ## (2) volume available for extraction in the neighborhood, (3) distance to forest industries, and
  # ## (4) type of species, conifer - 1 vs. deciduous - 0.
  # pextract <- left_join(volume, neigh.id, by=c("cell.id"="source.id")) %>% 
  #             left_join(select(harvest, cell.id, dist.biomass), by="cell.id") %>% 
  #             mutate(f1=scales::rescale(pmin(vol.extract, quantile(vol.extract, p=0.9)), to=c(0,1)) ,
  #                    f2=scales::rescale(pmin(vol.neigh, quantile(vol.neigh, p=0.9)), to=c(0,1)) ,
  #                    f3=scales::rescale(pmin(1/dist.biomass, quantile(1/dist.biomass, p=0.9)), to=c(0,1)),
  #                    f4=ifelse(spp<=7,0,1)) %>% 
  #             mutate(p=w1*f1+w2*f2+w3*f3+w4*f4)
  # 
  # 
  # ## Finally, adjust the wood demand according to the available volume
  # dmnd.wood <- min(dmnd.wood, sum(volume$vol.extract))
  # 
  # 
  # ## NOW, extract only for wood 
  # while(dmnd.sawlog>0){
  #   
  #   ## ID for each harvesting event, and restart step
  #   cut.id <- cut.id+1; step <- 1
  # }
  
  return(list(track.cut.cells=track.cut.cells, areas.mgmt=areas.mgmt))
}
  
 
  