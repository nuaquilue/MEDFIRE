######################################################################################
###  fire.regime()
###
######################################################################################

fire.regime <- function(land, coord, orography, pigni, swc, clim.sever, t, 
                        burnt.cells, burnt.intens, annual.burnt=0){
                        
  cat(paste0("Fires in SWC: ", ifelse(swc==1, "Wind.", ifelse(swc==2, "Heat.", 
                               ifelse(swc==3, "Regular.", "Prescribed.")))))

  ## Function to select items not in a vector
  `%notin%` <- Negate(`%in%`)
  
  ## Read and load input data
  load("inputlyrs/rdata/pfst.pwind.rdata")
  clim.severity <- read.table(paste0("inputfiles/", file.clim.severity, ".txt"), header=T)
  pctg.hot.days <- read.table(paste0("inputfiles/", file.pctg.hot.days, ".txt"), header=T)
  prob.hot <- read.table("inputfiles/ProbHot.txt", header=T)
  prob.conv <- read.table("inputfiles/ProbConv.txt", header=T)
  aba.dist <- read.table("inputfiles/AnnualBurntAreaDist.txt", header=T)
  fs.dist <- read.table("inputfiles/FireSizeDist.txt", header=T)
  fire.supp <- read.table(paste0("inputfiles/", file.fire.suppression, ".txt"), header=T)
  spp.flammability <- read.table("inputfiles/SppSpreadRate.txt", header=T)
  fst.sprd.weight <- read.table(paste0("inputfiles/", file.sprd.weight, ".txt"), header=T)
  
  
  ## Reset TrackFires data frame each run
  track.fire <- data.frame(year=NA, swc=NA, clim.sever=NA, fire.id=NA, fst=NA, 
                            wind=NA, atarget=NA, aburnt.highintens=NA, 
                            aburnt.lowintens=NA, asupp.fuel=NA, asupp.sprd=NA)
  
  
  ## Wind direction between neigbours
  ## Wind direction is coded as 0-N, 45-NE, 90-E, 135-SE, 180-S, 225-SW, 270-W, 315-NE
  default.neigh <- data.frame(x=c(-1,1,2900,-2900,2899,-2901,2901,-2899,-2,2,5800,-5800),
                              windir=c(270,90,180,0,225,315,135,45,270,90,180,0),
                              dist=c(100,100,100,100,141.421,141.421,141.421,141.421,200,200,200,200))
  default.nneigh <- nrow(default.neigh)
  
  
  ## Find either fixed or stochastic annual target area for wildfires
  if(swc<4){ 
    ## Fixed
    if(sum(clim.severity[clim.severity$year==t,2:4])>0){  #
      is.aba.fix <- T
      area.target <- clim.severity[clim.severity$year==t, swc+1]
    }
    ## Find stochastic annual burnt area
    else{ 
      is.aba.fix <- F
      if(clim.sever==1 & swc<=2){  # decide if climatic severity is extrem for 'wind' or 'heat' swc
        pctg <- pctg.hot.days[pctg.hot.days$year==t, swc+1]
        prob.extrem <- 1/(1+exp(-(prob.hot$inter[swc] + prob.hot$slope[swc]*pctg)))
        if(runif(1,0,100) <= prob.extrem) # extreme
            clim.sever <- 2
      }
      area.target <- round(min(200000, max(10, 
                           rlnorm(1, aba.dist$meanlog[aba.dist$clim==clim.sever & aba.dist$swc==swc],
                                    aba.dist$sdlog[aba.dist$clim==clim.sever & aba.dist$swc==swc])))) 
    }  
  }
  ## Find annual target area for prescribed burns
  else{
    if(!is.na(pb.target.area))
      area.target <- pb.target.area
    else{
      accum.burnt.area[2:7] <- accum.burnt.area[1:6]
      accum.burnt.area[1] <- annual.burnt
      area.target <- pmax(0,pb.convenient.area*7-sum(accum.burnt.area))
    }
  }  
  cat(paste(" Annual target area:", area.target), "\n")
  
  
  ## Update prob.igni according to swc
  pigni <- data.frame(cell.id=land$cell.id, p=pigni*pfst.pwind[,ifelse(swc==1,1,2)])
  pigni <- filter(pigni, !is.na(p) & p>0)
  pfst.pwind$cell.id <- land$cell.id
  
  
  ## Pre-select the coordinates of old Mediterranean vegetation, i.e.
  ## Pinus halepensis, Pinus nigra, and Pinus pinea of age >=30 years.
  ## to compute probability of being a convective fire
  old.forest.coord <- filter(land, spp<=3 & age>=30) %>% select(cell.id) %>% left_join(coord, by = "cell.id")

  
  ## Start burning until annual area target is not reached
  fire.id <- 0
  track.spread <- data.frame(fire.id=fire.id, cell.id=NA, step=NA, spp=NA,
                             slope=0, wind=0, flam=0, aspc=0, sr=1, pb=1, burning=1)
  while(area.target>0){
    
    ## ID for each fire event
    fire.id <- fire.id+1
    
    ## Select an ignition point, to then decide the fire spread type, the fire suppression level,
    ## the wind direction and the target fire size according to clim and fire spread type
    igni.id <- sample(pigni$cell.id, 1, replace=F, pigni$p)
    
    ## Assign the fire spread type 
    if(swc==1 | swc==3)
      fire.spread.type <- swc
    else if(swc==4)
      fire.spread.type <- 3
    else{
      neighs <- nn2(coord[,-1], filter(coord, cell.id==igni.id)[,-1], searchtype="standard", k=100)
      nneigh <- sum(neighs$nn.dists[,]<=500)  #sqrt(2*500^2)
      old.neighs <- nn2(old.forest.coord[,-1], filter(coord, cell.id==igni.id)[,-1], searchtype="standard", k=100)
      old.nneigh <- sum(old.neighs$nn.dists[,]<=500) #sqrt(2*500^2)
      z <- filter(prob.conv, clim==clim.sever)$inter + filter(prob.conv, clim==clim.sever)$slope*(old.nneigh/nneigh)*100
      1/(1+exp(-z))
      fire.spread.type <- ifelse(runif(1,0,1)<=1/(1+exp(-z)),2,3)
    }

    ## According to the fire spread type, look at the weights of each factor on spread rate
    wwind <- fst.sprd.weight[1,fire.spread.type+1]
    wslope <- fst.sprd.weight[2,fire.spread.type+1]
    wflam <- fst.sprd.weight[3,fire.spread.type+1]
    waspc <- fst.sprd.weight[4,fire.spread.type+1]
    
    ## Assign the fire suppression levels
    sprd.th <- filter(fire.supp, clim==clim.sever, fst==fire.spread.type)$sprd.th
    fuel.th <- filter(fire.supp, clim==clim.sever, fst==fire.spread.type)$fuel.th
    
    ## Assign the main wind direction according to the fire spread type
    ## Wind directions: 0-N, 45-NE, 90-E, 135-SE, 180-S, 225-SW, 270-W, 315-NE
    if(fire.spread.type==1)  # N, NW or W according to map
      fire.wind <- sample(c(0,315,270), 1, replace=F, p=filter(pfst.pwind,cell.id==igni.id)[3:5])
    if(fire.spread.type==2)  # S 80%, SW 10%, SE 10%
      fire.wind <- sample(c(180,225,135), 1, replace=F, p=c(80,10,10))
    if(fire.spread.type==3)  # any at random
      fire.wind <- sample(seq(0,315,45), 1, replace=F)
    
    
    ## Derive target fire size from a power-law according to clima and fire.spread.type 
    ## Or prescribed size from a log-normal
    if(swc<4){
      log.size <- seq(1.7, 5, 0.01)
      log.num <- filter(fs.dist, clim==clim.sever, fst==fire.spread.type)$intercept +
        filter(fs.dist, clim==clim.sever, fst==fire.spread.type)$slope * log.size
      fire.size.target <- sample(round(10^log.size), 1, replace=F, prob=10^log.num)
    }
    else
      fire.size.target <- max(1,min(round(rlnorm(1,pb.mean,pb.sd)),100))
    ## Bound fire.size.target to not exceed remaining area.target
    if(fire.size.target>area.target)
      fire.size.target <- area.target
    
    ## Initialize tracking variables
    fire.front <- igni.id
    aburnt.lowintens <- 0
    aburnt.highintens <- 1  # ignition always burnt, and it does in high intensity
    if(swc==4){
      aburnt.lowintens <- 1
      aburnt.highintens <- 0} 
    asupp.sprd <- 0
    asupp.fuel <- 0
    burnt.cells <- c(burnt.cells, igni.id)
    visit.cells <- igni.id
    burnt.intens <- c(burnt.intens, ifelse(swc<4,T,F))
    
    ## Tracking
    fire.step <- 1
    track.spread <- rbind(track.spread, data.frame(fire.id=fire.id, cell.id=igni.id, step=fire.step, 
                               spp=land$spp[land$cell.id==igni.id],
                               slope=0, wind=0, flam=0, aspc=0, sr=1, pb=1, burning=1))
    
    
    ## Start speading from active cells (i.e. the fire front)
    while((aburnt.lowintens+aburnt.highintens+asupp.fuel+asupp.sprd)<fire.size.target){
      
      ## Build a data frame with the theoretical 12 (=default.nneigh=) neighbours of cells in fire.front, 
      ## Add the wind direction and the distance.
      ## Filter for neighbours that are currenty in Catalonia,
      ## And have not been visited yet
      neigh.id <- data.frame(cell.id=as.integer(rep(fire.front, each=default.nneigh)+rep(default.neigh$x, length(fire.front))),
                             source.id=rep(fire.front, each=default.nneigh),
                             dist=rep(default.neigh$dist,length(fire.front)),
                             windir=rep(default.neigh$windir,length(fire.front)) ) %>%
                  filter(cell.id %notin% visit.cells) 
      neigh.in.land <- is_inCpp(neigh.id$cell.id, land$cell.id)
      neig.id <- neigh.id[which(neigh.in.land!=-1),]             ##only neigh that are in land
      i.land.in.neigh <- neigh.in.land[which(neigh.in.land!=-1)] ##neigh.cellid indexes in land
      i.land.in.neigh <- unique(i.land.in.neigh)
      
      
      ## For all neighbours, compute fuel and flammability factors
      neigh.land <- land[i.land.in.neigh,] %>%
                    mutate(fuel=ifelse(spp %in% c(15,16,17), 0.5,
                                  ifelse(spp==14, 0.01638*biom,  # or 0.01638???
                                    ifelse(age<=7, 0.2,
                                      ifelse(biom<200, 0.4,
                                        ifelse(biom<480, 0.95, 0.6)))))) %>%
                    left_join(spp.flammability[,c(1,fire.spread.type+1)], by="spp") 
      neigh.land$flam <- wflam*neigh.land[, ncol(neigh.land)]
      
      ## For all neighbours, compute aspc factor ()
      ## Also keep fire.front cells as we need their elevation to compute diff.elevation
      i.orography.in.neigh.ff <- c(i.land.in.neigh, is_inCpp(fire.front, orography$cell.id)) ##neigh.cellid and fire front indexes in land
      neigh.orography <- orography[i.orography.in.neigh.ff,] %>%
                         mutate(aspc=waspc*ifelse(aspect==1, 0.1, ifelse(aspect==3, 0.9, ifelse(aspect==4, 0.4, 0.3))))
      
      ## Get spread rate by:
      ## Joining to the neig.id data.frame the neigh.land and keep only burnable neighs 
      ## Joining to this df, the neigh.orography to get the elevation of the source cells
      ## Joining to this df, the neigh.orography to get the elevation of the neighbour cells
      ## Computing slope and wind factors
      sprd.rate <- left_join(neigh.id, select(neigh.land, cell.id, spp, fuel, flam), by="cell.id") %>%
                   filter(spp<=17) %>%
                   left_join(select(neigh.orography, cell.id, elev), by=c("source.id"="cell.id")) %>%
                   left_join(select(neigh.orography, cell.id, elev, aspc), by="cell.id") %>% 
                     mutate(dif.elev = elev.y-elev.x, 
                          slope = wslope * pmax(pmin(dif.elev/dist,0.5),-0.5)+0.5, 
                          wind = wwind * (ifelse(abs(windir-fire.wind)>180, 
                                            360-abs(windir-fire.wind), abs(windir-fire.wind)))/180) %>% 
                   mutate(sr=slope+wind+flam+aspc, pb=1+rpb*log(sr*fuel)) %>% 
                   group_by(cell.id) %>% 
                   summarize(step=fire.step, spp=mean(spp), slope=max(slope), wind=max(wind),
                              flam=max(flam), aspc=max(aspc), sr=max(sr), pb=max(pb))
      
      ## Now compute probability of burning and actual burning state (T or F):
      sprd.rate$burning <- runif(nrow(sprd.rate), 0, pb.upper.th) <= sprd.rate$pb & sprd.rate$pb > pb.lower.th
      if(nrow(sprd.rate)>0)
        track.spread <- rbind(track.spread, data.frame(fire.id=fire.id, sprd.rate))
      
      ## If at least there's a burning cell, continue, otherwise, stop
      if(!any(sprd.rate$burning))
        break
      
      ## Mark the cells burnt and visit, and select the new fire front
      ## 'mad' -> median absolute deviation
      burnt.cells <- c(burnt.cells, sprd.rate$cell.id[sprd.rate$burning])
      visit.cells <- c(visit.cells, sprd.rate$cell.id)
      burnt.intens <- c(burnt.intens, sprd.rate$sr[sprd.rate$burning]>ifelse(swc<4,fire.intens.th,100))
      exclude.th <- min(max(sprd.rate$sr)-0.005, 
                        rnorm(1,mean(sprd.rate$sr[sprd.rate$burning])-mad(sprd.rate$sr[sprd.rate$burning])/2,
                              mad(sprd.rate$sr[sprd.rate$burning])))
      fire.front <- sprd.rate$cell.id[sprd.rate$burning & sprd.rate$sr>=exclude.th]
      
      ## Increase area burnt in either high or low intensity (Prescribed burns always burnt in low intensity)
      aburnt.lowintens <- aburnt.lowintens + sum(sprd.rate$burning & sprd.rate$sr<=ifelse(swc<4,fire.intens.th,100))
      aburnt.highintens <- aburnt.highintens + sum(sprd.rate$burning & sprd.rate$sr>ifelse(swc<4,fire.intens.th,100))
    
      ## Increment fire.step  
      fire.step <- fire.step+1
      
      ## In the case, there are no cells in the fire front, stop trying to burn.
      ## This happens when no cells have burnt in the current spreading step
      if(length(fire.front)==0)
        break
      
    } # while 'fire'
    
    ## Write info about this fire
    track.fire <- rbind(track.fire, data.frame(year=t, swc, clim.sever, fire.id, fst=fire.spread.type, 
                                               wind=fire.wind, atarget=fire.size.target, aburnt.highintens, 
                                               aburnt.lowintens, asupp.fuel, asupp.sprd))
    # cat(paste("Fire:", fire.id, "- aTarget:", fire.size.target, "- aBurnt:", aburnt.lowintens+aburnt.highintens), "\n")
    
    ## Update annual burnt area
    area.target <- area.target - (aburnt.lowintens+aburnt.highintens)
    
  }  #while 'year'
  
  return(list(burnt.cells=burnt.cells, burnt.intens=burnt.intens, 
              track.fire=track.fire[-1,], track.spread=track.spread[-1,]))
}

