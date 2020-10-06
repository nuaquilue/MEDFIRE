######################################################################################
##
######################################################################################

forest.mgmt <- function(land, harvest, clim, t){
  
  ## Tracking
  cat("Forest Management"); tic("  t")
  
  ## Function to select items not in a vector
  `%notin%` <- Negate(`%in%`)
  
  ## Harvesting rules
  rules <- read.table("inputfiles/MgmtRules.txt", header=T) %>% select(-name, -orgest)
  
  ## Eq. of basal area to volume
  eq.ba.vol <- read.table("inputfiles/EqBasalAreaVol.txt", header=T)
  # eq.ba.volbark <- read.table("inputfiles/EqBasalAreaVolWithBark.txt", header=T)
  
  ## Sawlog and Wood demands
  dmnd <- read.table(paste0("inputfiles/", file.dmnd.harvest, ".txt"), header=T)
  dmnd.sawlog <- dmnd$Sawlogs[t]
  dmnd.wood <- dmnd$Primary[t]
  
  
  ## Num of neighbours in a circular neighbourhood according to radius (radius is in pixels)
  ## Assume that the neighbourhood is a star, with the maximum number of pixels in the
  ## east-west or north-south direction is 2*radius + 1 (1 is the center cell).
  ## The num of pixels is sequentially: 3+1*2, 5+3*2+1*2, 7+5*2+3*2+1*2, ...
      # nneigh <- seq(3,41,2) + cumsum(seq(1,40,2)*2)
  
  ## Find cells suitable for management: forest with slope.pctg <=30% and dist.path <= 500m
  ## All tree species are harvestable exept Qsuber
  lambda <- 1/200
  suit.mgmt <- left_join(select(land, -tsdist, -distype), harvest, by="cell.id") %>%
               filter(spp <= 13 & slope.pctg <= 30) %>% filter(spp != 9) %>% # exclude quercus suber, not managed for sawlogs neither wood
               filter(enpe %notin% c(1,3,4,5)) %>%                           # exclude 'parc nacional', 'paratge natural interès nacional', 'reserva natural fauna salvatge', and 'reseva natural integral'
               mutate(ppath=exp(-dist.path*lambda))                          # dist - ppath: 0 - 1, 100 - 0.6065, 141 - 0.493, 200 - 0.36788
  suit.mgmt$access <- suit.mgmt$ppath >= runif(nrow(suit.mgmt), 0, 0.49)     ## ~10% is not accessible
  suit.mgmt <- filter(suit.mgmt, access)
  
  ## Find those forest cells that can be sustainably harvested
  sustain <- filter(land, cell.id %in% suit.mgmt$cell.id) %>% select(-tsdist, -tburnt) %>% 
             left_join(select(clim, cell.id, sqi), by="cell.id") %>% 
             left_join(rules, by=c("spp", "sqi")) 
  
  ## Determine which type of intervention to do
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
  
  # Exclude those locations that do not verify age nor ab minimal conditions
  sustain$exclude <- sustain$age<sustain$exclude.age & sustain$biom<sustain$exclude.ab*10  # decimals
  table(sustain$exclude)
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
  default.neigh <- data.frame(x=c(-1,1,2900,-2900,2899,-2901,2901,-2899))
  default.nneigh <- nrow(default.neigh)
  neigh.id <- data.frame(cell.id=as.integer(rep(volume$cell.id, each=default.nneigh)+
                                            rep(default.neigh$x, nrow(volume))),
                         source.id=rep(volume$cell.id, each=default.nneigh)) %>% 
              left_join(volume, by="cell.id") %>% group_by(source.id) %>% 
              summarise(vol.neigh=sum(vol.extract, na.rm=T))
  
  
  ## Set probability of extraction according to (1) volume available for extraction, 
  ## (2) volume available for extraction in the neighborhood, (3) distance to forest industries, and
  ## (4) type of species, conifer - 1 vs. deciduous - 0.
  w1 <- 0.4; w2 <- 0.2; w3 <- 0.3; w4 <- 0.1
  prob.extract <- left_join(volume, neigh.id, by=c("cell.id"="source.id")) %>% 
                  left_join(select(harvest, cell.id, dist.industry), by="cell.id") %>% 
                  mutate(f1=scales::rescale(pmin(vol.extract, quantile(vol.extract, p=0.9)), to=c(0,1)) ,
                         f2=scales::rescale(pmin(vol.neigh, quantile(vol.neigh, p=0.9)), to=c(0,1)) ,
                         f3=scales::rescale(pmin(1/dist.industry, quantile(1/dist.industry, p=0.9)), to=c(0,1)),
                         f4=ifelse(spp<=7,1,0) ) %>% 
                  mutate(p=w1*f1+w2*f2+w3*f3+w4*f4)
    # # plot probability of extraction
    # dta <- data.frame(cell.id=1:ncell(MASK)) %>% left_join(select(prob.extract, cell.id, p))
    # MASK[] <- dta$p
    # plot(MASK, col=viridis(6))
  
  
  ## Let's start harvesting
  if(dmnd.wood>0){
    
    ## Select target size of the intervention
    target.area <- round(pmin(rlnorm(1, meanlog=3, sdlog = 0.6), 60))
  
    neighs <- nn2(select(dta, x, y), filter(na.var2, cell.id==id)%>% select(x,y), 
                  searchtype="priority", k=target.area)

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
