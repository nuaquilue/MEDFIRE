######################################################################################
###  fire.regime()
###
######################################################################################

fire.regime <- function(land, coord, orography, clim, interface, all.swc, clim.sever, t, 
                        annual.burnt.area, MASK, out.path, irun, rpb){

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
  fst.sprd.weight <- read.table(paste0("inputfiles/", file.sprd.weight, ".txt"), header=T)
  
  ## MAP fire.ids and fire.step
  MAP <- MASK
  map <- data.frame(cell.id=land$cell.id, id=NA, step=NA)
  
  ## Reset TrackFires data frame each run and swc
  track.fire <- data.frame(year=NA, swc=NA, clim.sever=NA, fire.id=NA, fst=NA, wind=NA, atarget=NA, 
                           aburnt.highintens=NA, aburnt.lowintens=NA, asupp.sprd=NA, asupp.fuel=NA)
  track.burnt.cells <- data.frame(fire.id=NA, cell.id=NA, igni=NA, fintensity=NA)
  track.step <- data.frame(year=NA, fire.id=NA, step=NA, nneigh=NA, nneigh.in=NA, nburn=NA, nff=NA)
  fire.id <- 0
  visit.cells <- burnt.cells <- integer()
  annual.aburnt <- annual.asupp <- 0
  
  ## Start with the 12 neigbours of the ignition
  ## Wind direction is coded as 0-N, 45-NE, 90-E, 135-SE, 180-S, 225-SW, 270-W, 315-NE
  default.neigh <- data.frame(x=c(-1,1,2900,-2900,2899,-2901,2901,-2899),
                              windir=c(270,90,180,0,225,315,135,45),
                              dist=c(100,100,100,100,141.421,141.421,141.421,141.421))
  default.nneigh <- nrow(default.neigh)
  
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
    source.supp <- data.frame(cell.id=subland$cell.id, nsupp.sprd=0, nsupp.fuel=0)
    
    
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
    if(area.target>0 & write.maps)
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
      
      # Assign the fire spread type
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
        fire.spread.type <- ifelse(runif(1,0,1)<=1/(1+exp(-z)),2,3)
      }

      ## According to the fire spread type, look at the weights of each factor on spread rate
      facc <- fst.sprd.weight[1,fire.spread.type+1]
      wwind <- fst.sprd.weight[2,fire.spread.type+1]
      wslope <- fst.sprd.weight[3,fire.spread.type+1]
      
      ## Assign the fire suppression levels and
      ## minimum number of ha suppressed before to acivate fire-level suppression. 
      ## It applies for both types of suppression
      sprd.th <- filter(fire.supp, clim==clim.sever, fst==fire.spread.type)$sprd.th
      fuel.th <- filter(fire.supp, clim==clim.sever, fst==fire.spread.type)$fuel.th
      accum.supp <- filter(fire.supp, clim==clim.sever, fst==fire.spread.type)$accum
      
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
      
      ## Controls of the fire shape
      ## Max number of cells in the fire front
      mx.ncell.ff <- ifelse(fire.size.target<=500, 12, ifelse(fire.size.target<=1500, 20, ifelse(fire.size.target<=5000, 30, 40)))
      ## Min number of cells in the fire front, no sé si per incendis de me´s de 5000 o me´s d 10000
      mn.ncell.ff <- ifelse(fire.size.target<=5000, 8, 16)  # not sure if 16 for wind and 12 for convective
      ## wnsource --> per seguir direccionalitat vent
      wnsource <- ifelse(fire.spread.type==1, 100, 1)
      # threshodl ratio.burnt to be aplied, no sé si per incendis de me´s de 5000 o me´s d 10000
      thruky <- ifelse(fire.size.target<=5000, 0.85, 0.95)
      

      ## Initialize tracking variables
      ## Ignition always burnt, and it does in high intensity when no-PB
      fire.front <- igni.id
      cumul.source <- 1  
      aburnt.lowintens <- ifelse(swc==4, 1, 0)
      aburnt.highintens <- ifelse(swc==4, 0, 1)
      asupp.sprd <- asupp.fuel <- 0
      visit.cells <- c(visit.cells, igni.id) 
      burnt.cells <- c(burnt.cells, igni.id) 
      map$id[map$cell.id==igni.id] <- fire.id
      map$step[map$cell.id==igni.id] <- step
      track.burnt.cells <- rbind(track.burnt.cells, data.frame(fire.id=fire.id, cell.id=igni.id, igni=T, fintensity=1))
      
      # cat(paste("Fire:", fire.id, "- aTarget:", fire.size.target, "- igni:", igni.id))
      
      ## Start speading from active cells (i.e. the fire front)
      while((aburnt.lowintens+aburnt.highintens+asupp.sprd+asupp.fuel)<fire.size.target){
        
        ## Increment step
        step <- step+1
        
        ## Build a data frame with the theoretical 12 (=default.nneigh) neighbours of cells in fire.front, 
        ## and add the per definition wind direction and the distance.
        ## Filter cells that have not been visited yet.
        neigh.id <- data.frame(cell.id=as.integer(rep(fire.front, each=default.nneigh)+
                                                  rep(default.neigh$x, length(fire.front))),
                               source.id=rep(fire.front, each=default.nneigh),
                               position=rep(cumul.source, each=default.nneigh),
                               dist=rep(default.neigh$dist,length(fire.front)),
                               windir=rep(default.neigh$windir,length(fire.front)) ) %>%
                    filter(cell.id %notin% burnt.cells) %>% 
                    left_join(filter(source.supp, cell.id %in% fire.front), by=c("source.id" ="cell.id"))  # look if source cell has been suppressed
        
        ## Now find those neighbours that are currenty in Catalonia and are not burnable
        ## is_inCpp returns the position of neigh.id$cell.id in the 'subland' data.frame (not the cell.id)!
        neigh.in.land <- is_inCpp(neigh.id$cell.id, subland$cell.id)
        i.land.in.neigh <- unique(neigh.in.land[which(neigh.in.land!=-1)])
        ## If all the available neighbours are out of Catalonia, stop spreading
        if(length(i.land.in.neigh)==0)
          break
        
        ## Retrive the current neighs, and compute fuel
        neigh.land <- subland[i.land.in.neigh,] %>%
                      mutate(fuel=ifelse(spp %in% c(15,16,17), 0.1,  # grass, crop
                                   ifelse(spp==14, 0.01638*biom, # shrub
                                    ifelse(age<=7, 0.2, # saplings
                                      ifelse(biom<=200 & spp>=1 & spp<=7, 0.5, # conifer young
                                        ifelse(biom<=200 & spp>=8 & spp<=13, 0.3, # decid young
                                          ifelse(biom<=480 & spp>=1 & spp<=7, 1, # conifer mature
                                           ifelse(biom<=480 & spp>=8 & spp<=13, 0.8, 0.6)))))))) # decid mature - old
        
        ## Now, add to i.land.in.neigh the indexes (positions) of fire.front cells (in case these are not already there)
        ## Further on, we'll need to know the elevation of the fire.front cells.
        i.land.in.neigh <- unique(c(i.land.in.neigh, is_inCpp(fire.front, subland$cell.id)) )
        
        ## Retrieve the orography variables for fire.front and neigbhour cells, 
        neigh.orography <- suborography[i.land.in.neigh,] 
        
        ## Get spread rate by:
        ## Joining to the neig.id data.frame the neigh.land and keep only burnable neighs 
        ## Joining to this df, the neigh.orography to get the elevation of the source cells
        ## Joining to this df, the neigh.orography to get the elevation of the neighbour cells
        ## Computing slope and wind factors
        sprd.rate.sources <- left_join(neigh.land, neigh.id, by="cell.id") %>%
                             left_join(neigh.orography, by=c("source.id"="cell.id")) %>%
                             left_join(neigh.orography, by="cell.id")  %>% 
                             mutate(dif.elev = elev.y-elev.x, 
                                    dif.wind <- abs(windir-fire.wind),
                                    slope = pmax(pmin(dif.elev,0.5),-0.5)+0.5,  
                                    wind = ifelse(dif.wind==0, 0, ifelse(dif.wind %in% c(45,315), 0.25, 
                                           ifelse(dif.wind %in% c(90,270), 0.5, ifelse(dif.wind %in% c(135,225), 0.75, 1)))) ) %>% 
                             mutate(sr=wslope*slope+wwind*wind, fi=sr*fuel)
        sprd.rate.sources$pb <- 1-exp(-facc*sprd.rate.sources$fi) + runif(nrow(sprd.rate.sources), -rpb, rpb)   
        sprd.rate <- group_by(sprd.rate.sources, cell.id) %>% 
                     summarize(fire.id=fire.id, spp=mean(spp), age=mean(age), fi=max(fi), 
                               pb=max(pb), nsource=sum(position)) %>%
                     left_join(select(sprd.rate.sources, cell.id, pb, nsupp.sprd, nsupp.fuel), by=c("cell.id", "pb")) %>% 
                     mutate(nsupp.sprd=nsupp.sprd+(fi<=sprd.th), nsupp.fuel=nsupp.fuel+(spp<=14 & age<=fuel.th), 
                            tosupp.sprd=(nsupp.sprd>=accum.supp), tosupp.fuel=(nsupp.fuel>=accum.supp))
        
        ## Compute probability of burnt
        sprd.rate$burn <- sprd.rate$pb >= runif(nrow(sprd.rate), pb.lower.th, pb.upper.th)
        
        ## Now compute actual burn state (T or F) according to pb and suppress:
        if(nrow(sprd.rate)!=sum(source.supp$cell.id %in% sprd.rate$cell.id)){
          write.table(sprd.rate, paste0(out.path, "/ErrorSR.txt"), quote=F, row.names=F, sep="\t")
          write.table(sprd.rate.sources, paste0(out.path, "/ErrorSRsource.txt"), quote=F, row.names=F, sep="\t")
          stop("dif num cells")
        }
        source.supp$nsupp.sprd[source.supp$cell.id %in% sprd.rate$cell.id] <- sprd.rate$nsupp.sprd
        source.supp$nsupp.fuel[source.supp$cell.id %in% sprd.rate$cell.id] <- sprd.rate$nsupp.fuel
        
        ## Mark that all these neighs have been visited (before breaking in case no burn)
        visit.cells <- c(visit.cells, sprd.rate$cell.id)
        burnt.cells <- c(burnt.cells, sprd.rate$cell.id[sprd.rate$burn]) # effectively burnt or suppressed
        
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
        map$id[map$cell.id %in% sprd.rate$cell.id[sprd.rate$burn & !sprd.rate$tosupp.sprd & !sprd.rate$tosupp.fuel]] <- fire.id
        map$step[map$cell.id %in% sprd.rate$cell.id[sprd.rate$burn]] <- step
        
        ## If any cell has effectively burnt or suppressed in the current step, stop, because there is no
        ## cells from which to spread from.
        ## Otherwise, keep spreading even if everything is by suppression. that's what we want!
        nburn <- sum(sprd.rate$burn)
        if(is.na(nburn)){
          write.table(sprd.rate, paste0(out.path, "/ErrorSR.txt"), quote=F, row.names=F, sep="\t")
          write.table(sprd.rate.sources, paste0(out.path, "/ErrorSRsource.txt"), quote=F, row.names=F, sep="\t")
          stop("NA in spread.rate")
        }
        if(nburn==0)
          break   
        
        ## Mark the effectively burnt cells and the fire intensity for effectively burnt cells
        ## First condition indicates is something has burn and the second that has not been suppressed by any type of suppression
        if(sum(sprd.rate$burn & !(sprd.rate$tosupp.fuel | sprd.rate$tosupp.sprd))>0)  
          track.burnt.cells <- rbind(track.burnt.cells, data.frame(fire.id=fire.id, 
                                                                   cell.id=sprd.rate$cell.id[sprd.rate$burn & !sprd.rate$tosupp.sprd & !sprd.rate$tosupp.fuel], 
                                                                   igni=F, fintensity=sprd.rate$fi[sprd.rate$burn & !sprd.rate$tosupp.sprd & !sprd.rate$tosupp.fuel]))
        
        ## Increase area burnt in either high or low intensity (Prescribed burns always burnt in low intensity)
        aburnt.lowintens <- aburnt.lowintens + sum(sprd.rate$burn  & !sprd.rate$tosupp.sprd & !sprd.rate$tosupp.fuel & sprd.rate$fi<=ifelse(swc<4,fire.intens.th,100))
        aburnt.highintens <- aburnt.highintens + sum(sprd.rate$burn & !sprd.rate$tosupp.sprd & !sprd.rate$tosupp.fuel & sprd.rate$fi>ifelse(swc<4,fire.intens.th,100))
        asupp.sprd <- asupp.sprd + sum(sprd.rate$burn & !sprd.rate$tosupp.fuel & sprd.rate$tosupp.sprd)
        asupp.fuel <- asupp.fuel + sum(sprd.rate$burn & sprd.rate$tosupp.fuel)
        
        ## Select the new fire front
        if(nburn<=mn.ncell.ff){
          fire.front <- sprd.rate$cell.id[sprd.rate$burn]
          cumul.source <- sprd.rate$nsource[sprd.rate$burn]
        }
        else{
          ratio.burnt <- (aburnt.lowintens+aburnt.highintens+asupp.sprd+asupp.fuel)/fire.size.target
          z <- rdunif(1,mx.ncell.ff-5,mx.ncell.ff)
          ncell.ff <- min(nburn*runif(1,0.5,0.7), z, na.rm=T)
          # si el nombre cell del ff coincideix amb el màxim  
          # o bé aleatòriament cap al final de l'incendi, forço compacitat.
          if(any(is.na(sprd.rate$nsource)) | any(is.na(sprd.rate$pb))){
            write.table(sprd.rate, paste0(out.path, "/ErrorSR.txt"), quote=F, row.names=F, sep="\t")
            write.table(sprd.rate.sources, paste0(out.path, "/ErrorSRsource.txt"), quote=F, row.names=F, sep="\t")
            stop("NA in sample fire.front")
          }
          
          if(ncell.ff==z | (ratio.burnt>=thruky & runif(1,0,1)>=0.75))
            fire.front <- sort(sample(sprd.rate$cell.id[sprd.rate$burn], round(ncell.ff), replace=F,
                                      prob=sprd.rate$nsource[sprd.rate$burn]/100 ) )  
          else
            fire.front <- sort(sample(sprd.rate$cell.id[sprd.rate$burn], round(ncell.ff), replace=F, 
                                      prob=wnsource^sprd.rate$pb[sprd.rate$burn]) )
          cumul.source <- sprd.rate$nsource[sprd.rate$cell.id %in% fire.front]
        }
        
        ## In the case, there are no cells in the fire front, stop trying to burn.
        ## This happens when no cells have burnt in the current spreading step
        if(length(fire.front)==0)
          break
        
        ## Track spreading spet
        track.step <- rbind(track.step, data.frame(year=t, fire.id, step, nneigh=nrow(neigh.id),
                                 nneigh.in=length(i.land.in.neigh), nburn, nff=length(fire.front)))
        
      } # while 'fire.size.target'
      
      ## Write info about this fire
      track.fire <- rbind(track.fire, data.frame(year=t, swc, clim.sever, fire.id, fst=fire.spread.type, 
                                                 wind=fire.wind, atarget=fire.size.target, aburnt.highintens, 
                                                 aburnt.lowintens, asupp.sprd, asupp.fuel))
      # cat(paste(" - aBurnt:", aburnt.lowintens+aburnt.highintens, "- aSupp:", asupp.sprd+asupp.fuel), "\n")
      
      ## Update annual burnt area
      area.target <- area.target - (aburnt.lowintens + aburnt.highintens + asupp.sprd + asupp.fuel)
      annual.aburnt <- annual.aburnt + aburnt.lowintens + aburnt.highintens
      annual.asupp <- annual.asupp + asupp.sprd + asupp.fuel
      
      if(is.na(area.target)){
        write.table(sprd.rate, paste0(out.path, "/ErrorSR.txt"), quote=F, row.names=F, sep="\t")
        write.table(sprd.rate.sources, paste0(out.path, "/ErrorSRsource.txt"), quote=F, row.names=F, sep="\t")
        stop("NA in area.target")
      }
      
    }  # while 'area.target'
    
    if(print.maps){
      # Data frame with 
      save(map, file=paste0(out.path, "/Maps_r", irun, "t", t, "_swc", swc, ".rdata"))
      # ## fire.ids
      # MAP[!is.na(MASK[])] <- map$id
      # writeRaster(MAP, paste0(out.path, "/lyr/FireIds_r", irun, "t", t, "swc", swc, ".tif"), format="GTiff", overwrite=T)
      # ## fire.step
      # MAP[!is.na(MASK[])] <- map$step
      # writeRaster(MAP, paste0(out.path, "/lyr/FireStep_r", irun, "t", t, "swc", swc, ".tif"), format="GTiff", overwrite=T)
    }
    
  }  # 'all.swc
  
  return(list(track.fire=track.fire[-1,], track.burnt.cells=track.burnt.cells[-1,], track.step=track.step[-1,]))
}

