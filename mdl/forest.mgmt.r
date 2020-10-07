######################################################################################
## In "preparatory" harvesting, between 65% - 85% of harvesting goes for sawlogs, the remainder goes for wood
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
  track.cut.cells <- data.frame(cut.id=NA, cell.id=NA, typcut=NA, 
                                pextract=NA, pwood=NA, vol.sawlog=NA, vol.wood=NA)
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
  volume <- select(sustain, cell.id, spp, biom, pctgextract) %>% 
            left_join(eq.ba.vol, by="spp") %>% 
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
  w1 <- 0.3; w2 <- 0; w3 <- 0.3; w4 <- 0.4
  pextract <- left_join(volume, neigh.id, by=c("cell.id"="source.id")) %>% 
              left_join(select(harvest, cell.id, dist.industry), by="cell.id") %>% 
              mutate(f1=scales::rescale(pmin(vol.extract, quantile(vol.extract, p=0.9)), to=c(0,1)) ,
                     f2=scales::rescale(pmin(vol.neigh, quantile(vol.neigh, p=0.9)), to=c(0,1)) ,
                     f3=scales::rescale(pmin(1/dist.industry, quantile(1/dist.industry, p=0.9)), to=c(0,1)),
                     f4=ifelse(spp<=7,1,0) ) %>% 
              mutate(p=w1*f1+w2*f2+w3*f3+w4*f4)
  rm(volume); rm(w1); rm(w2); rm(w3); rm(w4)
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
    
    ## Harvest this first cell and decrease overall demand, if already reach it, stop cutting
    area.cut <- 1
    aux <- filter(sustain, cell.id==igni.id) %>% left_join(eq.ba.vol, by="spp") %>% 
           mutate(vol=cx*biom/10+cx2*biom*biom/100, vol.extract=vol*pctgextract/100,
                  pwood=ifelse(todo=="fin", runif(1, 1-0.95, 1-0.9), runif(1, 1-0.85, 1-0.65)))
    if(aux$pwood>0){
      dmnd.sawlog <- dmnd.sawlog - aux$vol.extract * (1-aux$pwood)
      dmnd.wood <- dmnd.wood - aux$vol.extract * aux$pwood   ## dmnd wood is in m3 or t. If tones, need to covert volume to mass!!!
    }
    else
      dmnd.sawlog <- dmnd.sawlog - aux$vol.extract

    
    ## Track cutting this first cell of the patch
    front <- igni.id
    visit.cells <- c(visit.cells, igni.id)
    track.cut.cells <- rbind(track.cut.cells, 
                             data.frame(cut.id=cut.id, cell.id=igni.id, typcut=aux$todo, 
                             pextract=aux$pctgextract, pwood=aux$pwood,
                             vol.sawlog=aux$vol.extract*(1-aux$pwood), vol.wood=aux$vol.extract*aux$pwood))
    pextract <- filter(pextract, cell.id!=igni.id)  ## remove that igni from the pool of selectable cells
    
    
    ## Cut as much as left of the target area to be harvested, but if sawlog demand is not reach.
    ## Otherwise, no need to keep cutting
    while(area.cut<target.area & dmnd.sawlog>0){
      
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
      dmnd.sawlog <- dmnd.sawlog -  sum(aux$vol.extract*(1-aux$pwood))
      dmnd.wood <- dmnd.wood -  sum(aux$vol.extract*aux$pwood)
      visit.cells <- c(visit.cells, neigh.land$cell.id)
      track.cut.cells <- rbind(track.cut.cells, 
                               data.frame(cut.id=cut.id, cell.id=aux$cell.id, typcut=aux$todo, 
                                          pextract=aux$pctgextract, pwood=aux$pwood,
                                          vol.sawlog=aux$vol.extract*(1-aux$pwood), 
                                          vol.wood=aux$vol.extract*aux$pwood))
      pextract <- filter(pextract, cell.id %notin% aux$cell.id)  # remove harvested cells from the pool of selectable cells
      
      ## Keep harvesting. If there there are no cells in the front, stop trying to cut.
      front <- neigh.land$cell.id
      if(length(front)==0)
        break
    }
   
    print(paste("cut.id", cut.id, "- rem.dmnd.sawlog", dmnd.sawlog, "- rem.dmnd.wood", dmnd.wood))
  }

  
  ## com ha anat això??
  a <- filter(track.cut.cells, !is.na(cut.id)) %>% 
        group_by( cut.id) %>% summarise(vol=sum(vol.sawlog+vol.wood),
       vol.sawlog=sum(vol.sawlog), vol.wood=sum(vol.wood))
  sum(a$vol); sum(a$vol.sawlog); sum(a$vol.wood)
  b <- filter(track.cut.cells, !is.na(cut.id)) %>% 
       left_join(select(sustain, cell.id, spp), by="cell.id") %>% 
        group_by(spp) %>% summarise(vol.sawlog=sum(vol.sawlog), vol.wood=sum(vol.wood))
  sum(b$vol.sawlog[b$spp<=7]); round(100*sum(b$vol.sawlog[b$spp<=7])/sum(b$vol.sawlog))
  sum(b$vol.sawlog[b$spp>7]); round(100*sum(b$vol.sawlog[b$spp>7])/sum(b$vol.sawlog))
  
  
  ## NOW wood demand
  while(dmnd.sawlog>0){
    
    ## ID for each harvesting event, and restart step
    cut.id <- cut.id+1; step <- 1
  }
  
  return(track.cut.cells=track.cut.cells[-1,])
}
  
 
  