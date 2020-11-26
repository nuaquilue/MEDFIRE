######################################################################################
###  fire.regime()
###
######################################################################################

fire.regime <- function(land, coord, orography, clim, interface, all.swc, clim.sever, t, 
                        annual.burnt.area, MASK, out.path, irun, crazy, nx, nff){

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
  
  ## MAP fire.ids
  MAP <- MASK
  mask <- data.frame(cell.id=land$cell.id, id=NA, step=NA)
  
  ## Reset TrackFires data frame each run and swc
  track.fire <- data.frame(year=NA, swc=NA, clim.sever=NA, fire.id=NA, fst=NA, wind=NA, atarget=NA, 
                           aburnt.highintens=NA, aburnt.lowintens=NA, asupp.sprd=NA, asupp.fuel=NA)
  track.burnt.cells <- data.frame(fire.id=NA, cell.id=NA, igni=NA, fintensity=NA)
  track.step <- data.frame(year=NA, fire.id=NA, step=NA, nneigh=NA, nneigh.in=NA, nburn=NA, nff=NA)
  fire.id <- 0
  burnt.cells <- visit.cells <- integer()
  annual.aburnt <- annual.asupp <- 0
  
  ## Choose target area per each swc and burn it
  for(swc in all.swc){
    
    ## Print SWC
    cat(paste0("Fires in SWC: ", ifelse(swc==1, "Wind.", 
                                    ifelse(swc==2, "Heat.", 
                                        ifelse(swc==3, "Regular.", "Prescribed.")))))
    
    
    ## To be sure that non-burnable covers do not burn (water, rock, urban), nor agriculture land
    ## under prescribed burns
    if(swc<4)
      i <- land$spp<=17        # 2.938.560
    else
      i <- land$spp<=15        # 1.937.915
    subland <- land[i,]   
    suborography <- orography[i,]
    source.supp <- data.frame(cell.id=subland$cell.id, source.supp.sprd=F, source.supp.fuel=F)
    
    
    ## Find either fixed or stochastic annual target area for wildfires
    if(swc<4){ 
      ## Fixed
      if(sum(clim.severity[clim.severity$year==t,2:4])>0){  
        is.aba.fix <- T
        area.target <- min(200000-(annual.aburnt+annual.asupp), clim.severity[clim.severity$year==t, swc+1])
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
        # maximum annual area target is 200.000 ha (for the three SWC together)
        area.target <- round(min(200000-(annual.aburnt+annual.asupp), 
                             max(10,rlnorm(1, aba.dist$meanlog[aba.dist$clim==clim.sever & aba.dist$swc==swc],
                                              aba.dist$sdlog[aba.dist$clim==clim.sever & aba.dist$swc==swc])))) 
      }  
    }
    ## Find annual target area for prescribed burns
    else{
      if(!is.na(pb.target.area))
        area.target <- pb.target.area
      else{
        accum.burnt.area[2:7] <- accum.burnt.area[1:6]
        accum.burnt.area[1] <- annual.burnt.area
        area.target <- pmax(0,pb.convenient.area*7-sum(accum.burnt.area))
      }
    }  
    cat(paste(" Annual target area:", area.target), "\n")
    print.maps <- F
    if(area.target>0)
      print.maps <- T
    
    
    ## Update prob.igni according to swc
    pigni <- prob.igni(land, orography, clim, interface)
    pigni <- mutate(pigni, psft=p*pfst.pwind[,ifelse(swc==1,1,2)+1]) %>%
            filter(cell.id %in% subland$cell.id)
    
    
    ## Pre-select the coordinates of old Mediterranean vegetation, i.e.
    ## Pinus halepensis, Pinus nigra, and Pinus pinea of age >=30 years.
    ## to compute probability of being a convective fire
    old.forest.coord <- filter(subland, spp<=3 & age>=30) %>% select(cell.id) %>% left_join(coord, by = "cell.id")
    
    
    ## Start burn until annual area target is not reached
    while(area.target>0){
      
      ## ID for each fire event, and restart step
      fire.id <- fire.id+1
      step <- 1
      
      ## Select an ignition point, to then decide the fire spread type, the fire suppression level,
      ## the wind direction and the target fire size according to clim and fire spread type
      ## What if selected igni has already been burnt?? How can I control it? pigni$psft==0 of burnt cells??
      igni.id <- sample(pigni$cell.id, 1, replace=F, pigni$psft)
      
      ## Start with the 12 neigbours of the ignition
      ## Wind direction is coded as 0-N, 45-NE, 90-E, 135-SE, 180-S, 225-SW, 270-W, 315-NE
      default.neigh <- data.frame(x=c(-1,1,2900,-2900,2899,-2901,2901,-2899,-2,2,5800,-5800),
                                  windir=c(270,90,180,0,225,315,135,45,270,90,180,0),
                                  dist=c(100,100,100,100,141.421,141.421,141.421,141.421,200,200,200,200))
      # default.neigh <- data.frame(x=c(-1,1,2900,-2900,2899,-2901,2901,-2899),
      #                             windir=c(270,90,180,0,225,315,135,45),
      #                             dist=c(100,100,100,100,141.421,141.421,141.421,141.421))
      
      default.nneigh <- nrow(default.neigh)
      sub.default.neigh <- default.neigh
      
      ## Assign the fire spread type 
      # if(swc==1 | swc==3)
      #   fire.spread.type <- swc
      # else if(swc==4)
      #   fire.spread.type <- 3
      # else{
      #   neighs <- nn2(coord[,-1], filter(coord, cell.id==igni.id)[,-1], searchtype="standard", k=100)
      #   nneigh <- sum(neighs$nn.dists[,]<=500)  #sqrt(2*500^2)
      #   old.neighs <- nn2(old.forest.coord[,-1], filter(coord, cell.id==igni.id)[,-1], searchtype="standard", k=100)
      #   old.nneigh <- sum(old.neighs$nn.dists[,]<=500) #sqrt(2*500^2)
      #   z <- filter(prob.conv, clim==clim.sever)$inter + filter(prob.conv, clim==clim.sever)$slope*(old.nneigh/nneigh)*100
      #   1/(1+exp(-z))
      #   fire.spread.type <- ifelse(runif(1,0,1)<=1/(1+exp(-z)),2,3)
      # }
      if(swc<4)
        fire.spread.type <- swc
      if(swc==4)
        fire.spread.type <- 3
      
      ## According to the fire spread type, look at the weights of each factor on spread rate
      rpb <- fst.sprd.weight[1,fire.spread.type+1]
      wwind <- fst.sprd.weight[2,fire.spread.type+1]
      wslope <- fst.sprd.weight[3,fire.spread.type+1]
      wflam <- fst.sprd.weight[4,fire.spread.type+1]
      waspc <- fst.sprd.weight[5,fire.spread.type+1]
      spp.flam <- filter(spp.flammability, fst==fire.spread.type) %>% select(-fst)
      
      ## Assign the fire suppression levels
      sprd.th <- filter(fire.supp, clim==clim.sever, fst==fire.spread.type)$sprd.th
      fuel.th <- filter(fire.supp, clim==clim.sever, fst==fire.spread.type)$fuel.th
      
      ## Assign the main wind direction according to the fire spread type
      ## Wind directions: 0-N, 45-NE, 90-E, 135-SE, 180-S, 225-SW, 270-W, 315-NE
      if(fire.spread.type==1)  # N, NW or W according to map
        fire.wind <- sample(c(0,315,270), 1, replace=F, p=filter(pfst.pwind, cell.id==igni.id)[4:6])
      if(fire.spread.type==2)  # S 80%, SW 10%, SE 10%
        fire.wind <- sample(c(180,225,135), 1, replace=F, p=c(80,10,10))
      if(fire.spread.type==3)  # any at random
        fire.wind <- sample(seq(0,315,45), 1, replace=F)
      
      ## Derive target fire size from a power-law according to clima and fire.spread.type 
      ## Or prescribed size from a log-normal
      if(swc<4){
        if(swc==3)
          log.size <- seq(1.7, 3.4, 0.01)  ## max fire size is 2500 ha
        else
          log.size <- seq(1.7, 5, 0.01)  ## max fire size is 100.000 ha
        log.num <- filter(fs.dist, clim==clim.sever, fst==fire.spread.type)$intercept +
          filter(fs.dist, clim==clim.sever, fst==fire.spread.type)$slope * log.size
        fire.size.target <- sample(round(10^log.size), 1, replace=F, prob=10^log.num)
      }
      else
        fire.size.target <- max(1,min(round(rlnorm(1,pb.mean,pb.sd)),100))
      ## Bound fire.size.target to not exceed remaining area.target
      if(fire.size.target>area.target)
        fire.size.target <- area.target
      
      ## Assign fire front acceleration as function of fire size target
      fi.acc <- ifelse(fire.size.target>1000, fi.accelerate, 1)
      
      ## Initialize tracking variables
      ## Ignition always burnt, and it does in high intensity when no-PB
      fire.front <- igni.id
      aburnt.lowintens <- ifelse(swc==4, 1, 0)
      aburnt.highintens <- ifelse(swc==4, 0, 1)
      asupp.sprd <- asupp.fuel <- 0
      n.lowsprd <- n.lowfuel <- 0
      visit.cells <- c(visit.cells, igni.id) 
      mask$id[mask$cell.id==igni.id] <- fire.id
      mask$step[mask$cell.id==igni.id] <- step
      track.burnt.cells <- rbind(track.burnt.cells, data.frame(fire.id=fire.id, cell.id=igni.id, igni=T, fintensity=1))
      
      
      ## Start speading from active cells (i.e. the fire front)
      while((aburnt.lowintens+aburnt.highintens+asupp.sprd+asupp.fuel)<fire.size.target){
        
        step <- step+1
        
        ## Build a data frame with the theoretical 12 (=default.nneigh) neighbours of cells in fire.front, 
        ## and add the per definition wind direction and the distance.
        ## Filter cells that have not been visited yet.
        neigh.id <- data.frame(cell.id=as.integer(rep(fire.front, each=default.nneigh)+
                                                    rep(sub.default.neigh$x, length(fire.front))),
                               source.id=rep(fire.front, each=default.nneigh),
                               dist=rep(sub.default.neigh$dist,length(fire.front)),
                               windir=rep(sub.default.neigh$windir,length(fire.front)) ) %>%
          filter(cell.id %notin% visit.cells) %>% 
          left_join(filter(source.supp, cell.id %in% fire.front), by=c("source.id" ="cell.id"))  # look if source cell has been suppressed
        
        ## Now find those neighbours that are currenty in Catalonia and are not burnable
        ## is_inCpp returns the position of neigh.id$cell.id in the 'subland' data.frame (not the cell.id)!
        neigh.in.land <- is_inCpp(neigh.id$cell.id, subland$cell.id)
        i.land.in.neigh <- unique(neigh.in.land[which(neigh.in.land!=-1)])
        ## If all the available neighbours are out of Catalonia, stop spreading
        if(length(i.land.in.neigh)==0)
          break
        
        ## For all neighbours, compute fire intenstiy and flammability factors
        ## fire intenstiy and flam will be NA for non burnable covers
        neigh.land <- subland[i.land.in.neigh,] %>%
          mutate(fuel=ifelse(spp %in% c(15,16,17), 0.5,
                             ifelse(spp==14, 0.01638*biom, 
                                    ifelse(age<=7, 0.2,
                                           ifelse(biom<=200, 0.4,
                                                  ifelse(biom<=480, 0.95, 0.6)))))) %>%
          left_join(spp.flam, by="spp") %>% mutate(flam=wflam*flam)
        
        ## Now, add to i.land.in.neigh the indexes (positions) of fire.front cells (in case these are not already there)
        ## Further on, we'll need to know the elevation of the fire.front cells.
        i.land.in.neigh <- unique(c(i.land.in.neigh, is_inCpp(fire.front, subland$cell.id)) )
        
        ## Retrieve the orography variables for fire.front and neigbhour cells, 
        ## and already compute aspect factor
        neigh.orography <- suborography[i.land.in.neigh,] %>%
          mutate(aspc=waspc*ifelse(aspect==1, 0.1, 
                                   ifelse(aspect==3, 0.9, ifelse(aspect==4, 0.4, 0.3))))
        
        ## Get spread rate by:
        ## Joining to the neig.id data.frame the neigh.land and keep only burnable neighs 
        ## Joining to this df, the neigh.orography to get the elevation of the source cells
        ## Joining to this df, the neigh.orography to get the elevation of the neighbour cells
        ## Computing slope and wind factors
        sprd.rate <- left_join(neigh.land, neigh.id, by="cell.id") %>%
          left_join(select(neigh.orography, cell.id, elev), by=c("source.id"="cell.id")) %>%
          left_join(select(neigh.orography, cell.id, elev, aspc), by="cell.id")  %>% 
          mutate(dif.elev = elev.y-elev.x, 
                 slope = wslope * (pmax(pmin(dif.elev/dist,0.5),-0.5)+0.5), 
                 difwind = abs(windir-fire.wind),
                 # wind = wwind * ((ifelse(abs(windir-fire.wind)>180,
                 #         360-abs(windir-fire.wind), abs(windir-fire.wind)))/180)  ) %>% 
                 wind = wwind * (ifelse(difwind==180, 1, ifelse(difwind==0, 0,  # medium
                                                                ifelse(difwind==90, 0.3, ifelse(difwind==45, 0.1, 0.8))))) ) %>%
                 # wind = wwind * (ifelse(difwind==180, 1, ifelse(difwind==0, 0,
                 #                                                ifelse(difwind==90, 0.15, ifelse(difwind==45, 0.05, 0.65))))) ) %>%
          mutate(sr=slope+wind+flam+aspc, fi=sr*fuel*fi.acc, pb=1+rpb*log(sr*fuel*fi.acc)) %>%
          group_by(cell.id) %>% 
          summarize(fire.id=fire.id, spp=mean(spp), age=mean(age), fuel=max(fuel),
                    source.supp.sprd = any(source.supp.sprd[which(sr == max(sr))]),
                    source.supp.fuel = any(source.supp.fuel[which(sr == max(sr))]),
                    sr=max(sr), fi=max(fi), pb=max(pb)) %>%
          mutate(tosupp.sprd=(fi<=sprd.th), tosupp.fuel=(spp<=14 & age<=fuel.th))
        
        ## Count how many cells could be suppressed
        n.lowsprd <- n.lowsprd + sum(sprd.rate$tosupp.sprd)
        n.lowfuel <- n.lowfuel + sum(sprd.rate$tosupp.fuel)
        
        ## Now compute actual burn state (T or F) according to pb and suppress:
        supp.sprd <- (sprd.rate$tosupp.sprd & n.lowsprd>=accum.supp) | sprd.rate$source.supp.sprd
        supp.fuel <- (sprd.rate$tosupp.fuel & n.lowfuel>=accum.supp) | sprd.rate$source.supp.fuel
        sprd.rate$burn <- sprd.rate$pb >= runif(nrow(sprd.rate), pb.lower.th, pb.upper.th)
        source.supp$source.supp.sprd[source.supp$cell.id %in% sprd.rate$cell.id[sprd.rate$tosupp.sprd]] <- T
        source.supp$source.supp.fuel[source.supp$cell.id %in% sprd.rate$cell.id[sprd.rate$tosupp.fuel]] <- T
        
        ## Mark that all these neighs have been visited (before breaking in case no burn)
        visit.cells <- c(visit.cells, sprd.rate$cell.id)
        
        ## If at least there's a burn cell, continue, otherwise, stop
        if(!any(sprd.rate$burn))
          break
        
        ## Avoid fire overshooting at last iteration: Only burn cells with higher pb
        temp.burnt <- sprd.rate[sprd.rate$burn, c("cell.id", "pb")]
        if((aburnt.lowintens+aburnt.highintens+asupp.fuel+asupp.sprd+nrow(temp.burnt))>fire.size.target){
          max.burnt <- fire.size.target - (aburnt.lowintens+aburnt.highintens+asupp.fuel+asupp.sprd)
          temp.burnt <- temp.burnt[order(temp.burnt$pb, decreasing = TRUE),]
          def.burnt <- temp.burnt$cell.id[1:max.burnt]
          sprd.rate$burn <- (sprd.rate$cell.id %in% def.burnt)
        }
        
        ## Tring to really get exact fire.id!!
        mask$id[mask$cell.id %in% 
                  sprd.rate$cell.id[sprd.rate$burn & !sprd.rate$tosupp.sprd & !sprd.rate$tosupp.fuel]] <- fire.id
        mask$step[mask$cell.id %in% sprd.rate$cell.id[sprd.rate$burn]] <- step
        
        ## Mark the burnt cells, the suppressed, and the fire intensity for burnt cells
        track.burnt.cells <- rbind(track.burnt.cells, data.frame(fire.id=fire.id, 
                             cell.id=sprd.rate$cell.id[sprd.rate$burn & !sprd.rate$tosupp.sprd & !sprd.rate$tosupp.fuel], 
                             igni=F, fintensity=sprd.rate$fi[sprd.rate$burn & !sprd.rate$tosupp.sprd & !sprd.rate$tosupp.fuel]))
        
        ## Increase area burnt in either high or low intensity (Prescribed burns always burnt in low intensity)
        aburnt.lowintens <- aburnt.lowintens + sum(sprd.rate$burn & sprd.rate$fi<=ifelse(swc<4,fire.intens.th,100))
        aburnt.highintens <- aburnt.highintens + sum(sprd.rate$burn & sprd.rate$fi>ifelse(swc<4,fire.intens.th,100))
        asupp.sprd <- asupp.sprd + sum(sprd.rate$tosupp.sprd & !sprd.rate$tosupp.fuel & !sprd.rate$burn)
        asupp.fuel <- asupp.fuel + sum(sprd.rate$tosupp.fuel & !sprd.rate$burn)
        
        ## Select the new fire front
        ## First, count the number of cells burnt in the current step
        nburn <- sum(sprd.rate$burn)
        ## If any cell has burnt in the current step, stop
        if(nburn==0)
          break
        ## Otherwise, select the new fire front, a random number from 1 to n.cell.burnt 
        ## according to fire.intensity
        if(nburn==1)
          fire.front <- sprd.rate$cell.id[sprd.rate$burn]
        if(nburn>1){
          z <- scales::rescale((aburnt.lowintens+aburnt.highintens)/fire.size.target, to=c(-3,2), from=c(0,1))
          if(nx==1)
            ncell.ff <- pmax(2, round(nburn/(1+exp(z))))
          if(nx==2)
            ncell.ff <- pmin(round(nburn*0.8), pmax(2,round(nburn/(1+exp(z)))))
          if(nx==3)
            ncell.ff <- pmin(round(nburn*(1-nburn/(2*nrow(neigh.land)))), pmax(2,round(nburn/(1+exp(z)))))
          if(nx==4)
            ncell.ff <- rdunif(1, round(nburn*nff[1]), round(nburn*nff[2]))
          fire.front <- base::sample(sprd.rate$cell.id[sprd.rate$burn], ncell.ff,
                                     replace=F, prob=sprd.rate$fi[sprd.rate$burn]*runif(nburn, 0.65, 1)) 
        }
        
        ## In the case, there are no cells in the fire front, stop trying to burn.
        ## This happens when no cells have burnt in the current spreading step
        if(length(fire.front)==0)
          break
        
        ## Subset of 4+(3, or 4, ... or 7) = 6 or 7 or 8 ... or 11 default neighbours
        ## At least one neighour is not evaluated. 
        sub.default.neigh <- default.neigh[c(1:4, sample(5:12, sample(crazy,1), replace=F)),]  #12 neigbhours
        # sub.default.neigh <- default.neigh[sample(1:8, sample(6:8,1), replace=F),]  # 8 neighbours
        default.nneigh <- nrow(sub.default.neigh)
        
        track.step <- rbind(track.step, data.frame(year=t, fire.id, step, nneigh=nrow(neigh.id),
                                 nneigh.in=length(i.land.in.neigh), nburn, nff=length(fire.front)))
        
      } # while 'fire.size.target'
      
      ## Write info about this fire
      track.fire <- rbind(track.fire, data.frame(year=t, swc, clim.sever, fire.id, fst=fire.spread.type, 
                                                 wind=fire.wind, atarget=fire.size.target, aburnt.highintens, 
                                                 aburnt.lowintens, asupp.sprd, asupp.fuel))
      # cat(paste("Fire:", fire.id, "- aTarget:", fire.size.target, "- aBurnt:", aburnt.lowintens+aburnt.highintens,
      #           "- aSupp:", asupp.sprd+asupp.fuel), "\n")
      
      ## Update annual burnt area
      area.target <- area.target - (aburnt.lowintens + aburnt.highintens + asupp.sprd + asupp.fuel)
      annual.aburnt <- annual.aburnt + aburnt.lowintens + aburnt.highintens
      annual.asupp <- annual.asupp + asupp.sprd + asupp.fuel
      
    }  # while 'year'
    
    
    if(print.maps){
      ## fire.ids
      MAP[!is.na(MASK[])] <- mask$id
      writeRaster(MAP, paste0(out.path, "/lyr/FireIds_r", irun, "t", t, "swc", swc, ".tif"), format="GTiff", overwrite=T)
      ## fire.step
      MAP[!is.na(MASK[])] <- mask$step
      writeRaster(MAP, paste0(out.path, "/lyr/FireStep_r", irun, "t", t, "swc", swc, ".tif"), format="GTiff", overwrite=T)
    }
    
  }
  
  return(list(track.fire=track.fire[-1,], track.burnt.cells=track.burnt.cells[-1,], track.step=track.step[-1,]))
}

