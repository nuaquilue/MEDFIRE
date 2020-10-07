######################################################################################
##
######################################################################################

forest.mgmt <- function(land, harvest, clim){
  
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
  
  ## Track cut cells
  track.cut.cells <- data.frame(cut.id=NA, cell.id=NA, typcut=NA, pextract=NA, pwood=NA)
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
  suit.mgmt$access <- suit.mgmt$ppath >= runif(nrow(suit.mgmt), 0, 0.49)     ## ~10% is not accessible
  suit.mgmt <- filter(suit.mgmt, access) 
  
  
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
  sustain$exclude <- sustain$age<sustain$exclude.age & sustain$biom<sustain$exclude.ab*10  # decimals
  sustain$exclude[!is.na(sustain$tscut)] <- sustain$exclude[!is.na(sustain$tscut)] & 
    (sustain$tscut[!is.na(sustain$tscut)] < sustain$exclude.tafter[!is.na(sustain$tscut)])  # reconsider "excludes", time after cut
  sustain$exclude[sustain$exclude.ab==0] <-   # when there's no minab, force at least, verify age condition (e.g. phalepensis - sqi1, ppinea - sqi1, ...)
    sustain$age[sustain$exclude.ab==0]<sustain$exclude.age[sustain$exclude.ab==0] 
      # table(sustain$exclude)
  sustain <- filter(sustain, !exclude) %>% select(-exclude)
  
  # Retain the ptcg of extraction and thab according to the 'todo' prescription 
  sustain$pctgextract <- ifelse(sustain$todo=="prep", sustain$pctgextract.prep,
                                ifelse(sustain$todo=="diss", sustain$pctgextract.diss, sustain$pctgextract.fin))
  sustain$thab <- ifelse(sustain$todo=="prep", sustain$thab.prep,
                         ifelse(sustain$todo=="diss", sustain$thab.diss, 0))
  
  
  ## Compute the volume that can be extracted in locations where harvesting is sustainable
  volume <- select(sustain, cell.id, spp, biom, pctgextract) %>% left_join(eq.ba.vol, by="spp") %>% 
            mutate(vol=cx*biom/10+cx2*biom*biom/100) %>% select(-cx, -cx2) %>% 
            mutate(vol.extract = vol*pctgextract/100)
  
  ## Finally, adjust the demand according to the available volume
  dmnd.sawlog <- min(dmnd.sawlog, sum(volume$vol.extract))
  
  
  ## To compute the probability of being harvested first, compute the volume available in the neighborhodd
  neigh.id <- data.frame(cell.id=as.integer(rep(volume$cell.id, each=default.nneigh)+
                                            rep(default.neigh$x, nrow(volume))),
                         source.id=rep(volume$cell.id, each=default.nneigh)) %>% 
              left_join(volume, by="cell.id") %>% group_by(source.id) %>% 
              summarise(vol.neigh=sum(vol.extract, na.rm=T))
  
  
  ## Set probability of extraction according to (1) volume available for extraction, 
  ## (2) volume available for extraction in the neighborhood, (3) distance to forest industries, and
  ## (4) type of species, conifer - 1 vs. deciduous - 0.
  w1 <- 0.4; w2 <- 0.2; w3 <- 0.3; w4 <- 0.1
  pextract <- left_join(volume, neigh.id, by=c("cell.id"="source.id")) %>% 
              left_join(select(harvest, cell.id, dist.industry), by="cell.id") %>% 
              mutate(f1=scales::rescale(pmin(vol.extract, quantile(vol.extract, p=0.9)), to=c(0,1)) ,
                     f2=scales::rescale(pmin(vol.neigh, quantile(vol.neigh, p=0.9)), to=c(0,1)) ,
                     f3=scales::rescale(pmin(1/dist.industry, quantile(1/dist.industry, p=0.9)), to=c(0,1)),
                     f4=ifelse(spp<=7,1,0) ) %>% 
              mutate(p=w1*f1+w2*f2+w3*f3+w4*f4)
  rm(volume)
    # # plot probability of extraction
    # dta <- data.frame(cell.id=1:ncell(MASK)) %>% left_join(select(pextract, cell.id, p))
    # MASK[] <- dta$p
    # plot(MASK, col=viridis(6))
  
  
  ## Let's start harvesting
  while(dmnd.sawlog>0){
    
    ## ID for each harvesting event, and restart step
    cut.id <- cut.id+1; step <- 1
    
    ## Select target size of the intervention (instead to be area-based could be vol-based)  ## THINK ABOUT IT
    target.area <- round(pmin(rlnorm(1, meanlog=3, sdlog = 0.6), 60))
    
    ## Select an ignition point where to start the harvesting intervention
    igni.id <- sample(pextract$cell.id, 1, replace=F, pextract$p)
    
    ## Initialize tracking variables
    front <- igni.id
    visit.cells <- c(visit.cells, igni.id)
    x <- runif(1, 1-0.85, 1-0.65)
    track.cut.cells <- rbind(track.cut.cells, data.frame(cut.id=cut.id, cell.id=igni.id, 
                             typcut=sustain$todo[sustain$cell.id==igni.id], 
                             pextract=sustain$pctgextract[sustain$cell.id==igni.id], 
                             pwood=ifelse(sustain$todo[sustain$cell.id==igni.id]=="prep", x, 0) ))
    
    ## Harvest this first cell and decrease overall demand, if already reach it, stop cutting
    area.cut <- 1
    aux <- filter(sustain, cell.id==igni.id) %>% left_join(eq.ba.vol, by="spp") %>% 
           mutate(vol=cx*biom/10+cx2*biom*biom/100, vol.extract=vol*pctgextract/100)
    if(as.numeric(filter(track.cut.cells, cell.id==igni.id) %>% select(pwood))>0){
      dmnd.sawlog <- dmnd.sawlog - aux$vol.extract * (1-x)
      dmnd.wood <- dmnd.wood - aux$vol.extract * x   ## dmnd wood is in m3 or t. If tones, need to covert volume to mass!!!
    }
    else
      dmnd.sawlog <- dmnd.sawlog - aux$vol.extract
    if(dmnd.sawlog<0)
      break
    
    
    ## Cut as much as left of the target area to be harvested
    while(area.cut < target.area){
      
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
      
      ## Do harvesting, track it, and keep harvesting if area.target is not reach
      area.cut <- area.cut + nrow(neigh.land)
      aux <- filter(sustain, cell.id %in% neigh.land$cell.id) %>% left_join(eq.ba.vol, by="spp") %>% 
             mutate(vol=cx*biom/10+cx2*biom*biom/100, vol.extract=vol*pctgextract/100)
      aux$x <- runif(nrow(aux), 1-0.85, 1-0.65) * (aux$todo=="prep")
      dmnd.sawlog <- dmnd.sawlog -  sum(aux$vol.extract*(1-x))
      dmnd.wood <- dmnd.wood -  sum(aux$vol.extract*x)
      visit.cells <- c(visit.cells, neigh.land$cell.id)
      track.cut.cells <- rbind(track.cut.cells, 
                               data.frame(cut.id=cut.id, cell.id=aux$cell.id, typcut=aux$todo, 
                                          pextract=aux$pctgextract, pwood=aux$x))
      front <- neigh.land$cell.id
      
      ## If there there are no cells in the front, stop trying to cut.
      if(length(front)==0)
        break
    }
    
  }

  
  ## NOW wood demand
  while(dmnd.sawlog>0){
    
  }
  
  return(track.cut.cells=track.cut.cells[-1,])
}
  
  
  ## Idem for "final" harvesting
  if(dmnd.sawlog>0){
    suit.harvest <- filter(suit.mgmt, spp %in% c(1:7,12:13)) %>%
      left_join(select(clim, cell.id, sqi), by="cell.id") %>% left_join(mgmt.rules, by = c("spp", "sqi")) %>%
      filter(biom/10>=minab.fin) %>% mutate(ba.extract=pctgextract.fin*biom/1000) %>%
      left_join(eq.ba.vol, by = "spp") %>% mutate(vol.extract=cx*ba.extract+cx2*ba.extract*ba.extract) %>%
      mutate(priority=(slope.pctg+1)*(dist.path+1)*(1/vol.extract))
    suit.harvest$vol.sawlog <- suit.harvest$vol.extract*runif(nrow(suit.harvest), 0.90, 0.95)
    suit.harvest$vol.wood <- suit.harvest$vol.extract - suit.harvest$vol.sawlog
    suit.harvest <- suit.harvest[order(suit.harvest$priority, decreasing=F),]  
    cum.vol <- cumsum(suit.harvest$vol.sawlog) 
    if(max(cum.vol)>=dmnd.sawlog)
      cell.id.harvest <- suit.harvest$cell.id[1:which(cum.vol>dmnd.sawlog)[1]]
    else
      cell.id.harvest <- suit.harvest$cell.id
    dmnd.sawlog <- dmnd.sawlog - sum(suit.harvest$vol.sawlog[suit.harvest$cell.id %in% cell.id.harvest])
    dmnd.wood <- dmnd.wood - sum(suit.harvest$vol.wood[suit.harvest$cell.id %in% cell.id.harvest])
    harvesting <- filter(suit.harvest, cell.id %in% cell.id.harvest) %>%
                  select(cell.id, spp, vol.sawlog, vol.wood) %>% mutate(sylvi=3)
  }
  
  ## Idem for "dissmenatory" harvesting
  if(dmnd.sawlog>0){
    suit.harvest <- filter(suit.mgmt, spp %in% c(1:7,12:13)) %>%
      left_join(select(clim, cell.id, sqi), by="cell.id") %>% left_join(mgmt.rules, by = c("spp", "sqi")) %>%
      filter(biom/10>=minab.diss) %>%
      mutate(ba.extract=pmin(biom/10-thab.diss, pctgextract.diss*biom/1000)) %>%
      left_join(eq.ba.vol, by = "spp") %>% mutate(vol.extract=cx*ba.extract+cx2*ba.extract*ba.extract) %>%
      mutate(priority=(slope.pctg+1)*(dist.path+1)*(1/vol.extract))
    suit.harvest$vol.sawlog <- suit.harvest$vol.extract*runif(nrow(suit.harvest), 0.65, 0.85)
    suit.harvest$vol.wood <- suit.harvest$vol.extract - suit.harvest$vol.sawlog
    suit.harvest <- suit.harvest[order(suit.harvest$priority, decreasing=F),]  
    cum.vol <- cumsum(suit.harvest$vol.sawlog) 
    if(max(cum.vol)>=dmnd.sawlog)
      cell.id.harvest <- suit.harvest$cell.id[1:which(cum.vol>dmnd.sawlog)[1]]
    else
      cell.id.harvest <- suit.harvest$cell.id
    dmnd.sawlog <- dmnd.sawlog - sum(suit.harvest$vol.sawlog[suit.harvest$cell.id %in% cell.id.harvest])
    dmnd.wood <- dmnd.wood - sum(suit.harvest$vol.wood[suit.harvest$cell.id %in% cell.id.harvest])
    harvesting <- rbind(harvesting,
                        filter(suit.harvest, cell.id %in% cell.id.harvest) %>%
                          select(cell.id, spp, vol.sawlog, vol.wood) %>% mutate(sylvi=2))
  }
  
  
  ## Find locations suitable for "preparatory" harvesting according to current biomass
  ## Prioritize locations according to slope, dist.path and volume
  ## Harvest as much sawlogs as needed to meet the demand
  ## Between 65% - 85% of harvesting goes for sawlogs, the remainder goes for wood
  ## In the prioritization we should include some spatial (neighbourhood) criteria 
  ## and try to cluster the interventions
  if(dmnd.sawlog>0){
    suit.harvest <- filter(suit.mgmt, spp %in% c(1:7,12:13)) %>%
                    left_join(select(clim, cell.id, sqi), by="cell.id") %>% left_join(mgmt.rules, by = c("spp", "sqi")) %>%
                    filter(biom/10>=minab.prep) %>%
                    mutate(ba.extract=pmin(biom/10-thab.prep, pctgextract.prep*biom/1000)) %>%
                    left_join(eq.ba.vol, by = "spp") %>% mutate(vol.extract=cx*ba.extract+cx2*ba.extract*ba.extract) %>%
                    mutate(priority=(slope.pctg+1)*(dist.path+1)*(1/vol.extract))
    suit.harvest$vol.sawlog <- suit.harvest$vol.extract*runif(nrow(suit.harvest), 0.65, 0.85)
    suit.harvest$vol.wood <- suit.harvest$vol.extract - suit.harvest$vol.sawlog
    suit.harvest <- suit.harvest[order(suit.harvest$priority, decreasing=F),]  
    cum.vol <- cumsum(suit.harvest$vol.sawlog) 
    if(max(cum.vol)>=dmnd.sawlog)
      cell.id.harvest <- suit.harvest$cell.id[1:which(cum.vol>dmnd.sawlog)[1]]
    else
      cell.id.harvest <- suit.harvest$cell.id
    dmnd.sawlog <- dmnd.sawlog - sum(suit.harvest$vol.sawlog[suit.harvest$cell.id %in% cell.id.harvest])
    dmnd.wood <- dmnd.wood - sum(suit.harvest$vol.wood[suit.harvest$cell.id %in% cell.id.harvest])
    harvesting <- rbind(harvesting,
                        filter(suit.harvest, cell.id %in% cell.id.harvest) %>%
                        select(cell.id, spp, vol.sawlog, vol.wood) %>% mutate(sylvi=1) )
  }
  
  
  
  ## Now harvest for Wood
  if(dmnd.wood>0){
    suit.harvest <- filter(suit.mgmt, spp %in% 8:10) %>%
                    left_join(select(clim, cell.id, sqi), by="cell.id") %>% left_join(mgmt.rules, by = c("spp", "sqi")) %>%
                    filter(biom/10>=minab.fin) %>% mutate(ba.extract=pctgextract.fin*biom/1000) %>%
                    left_join(eq.ba.vol, by = "spp") %>% mutate(vol.extract=cx*ba.extract+cx2*ba.extract*ba.extract) %>%
                    mutate(priority=(slope.pctg+1)*(dist.path+1)*(1/vol.extract))
    suit.harvest$vol.sawlog <- 0
    suit.harvest$vol.wood <- suit.harvest$vol.extract - suit.harvest$vol.sawlog
    suit.harvest <- suit.harvest[order(suit.harvest$priority, decreasing=F),]  
    cum.vol <- cumsum(suit.harvest$vol.extract) 
    if(max(cum.vol)>=dmnd.wood)
      cell.id.harvest <- suit.harvest$cell.id[1:which(cum.vol>dmnd.wood)[1]]
    else
      cell.id.harvest <- suit.harvest$cell.id
    dmnd.wood <- dmnd.wood - sum(suit.harvest$vol.extract[suit.harvest$cell.id %in% cell.id.harvest])
    harvesting <- rbind(harvesting,
                        filter(suit.harvest, cell.id %in% cell.id.harvest) %>%
                        select(cell.id, spp, vol.sawlog, vol.wood) %>% mutate(sylvi=4))
  }
  
  toc()
  return(harvesting)
}
