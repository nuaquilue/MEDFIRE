######################################################################################
###  fire.regime()
###
######################################################################################

fire.regime <- function(MASK, land, orography, pigni, coord, swc, clim.sever, t, burnt.cells, burnt.intens,
                        pb.target.area=NA, pb.convenient.area=NA, pb.mean=NA, pb.sd=NA, pb.fage.th=NA){

  print(paste("Fires in SWC:", ifelse(swc==1, "Wind", ifelse(swc==2, "Heat", 
                               ifelse(swc==3, "Regular", "Prescribed")))))
  
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
  fst.sprd.weight <- read.table("inputfiles/SprdRateWeights.txt", header=T)
  
  
  ## Restarting Tracking fires data frame each run
  track.fires <- data.frame(year=NA, swc=NA, clim.sever=NA, fire.id=NA, fire.spread.type=NA, 
                            fire.wind=NA, fire.size.target=NA, aburnt.highintens=NA, 
                            aburnt.lowintens=NA, asupp.fuel=NA, asupp.sprd=NA)
  
  
  ## Default Euclidean distance and wind direction between neigbours
  ## Wind direction is coded as 0-N, 45-NE, 90-E, 135-SE, 180-S, 225-SW, 270-W, 315-NE
  mask <- data.frame(cell.id=1:ncell(MASK), x=MASK[])
  default.dist <- c(rep(c(sqrt(2*100^2),100),2),  rep(c(100,sqrt(2*100^2)),2))
  default.windir <- data.frame(x=c(0,-1,1,2900,-2900,2899,-2901,2901,-2899),
                               windir=c(-1,270,90,180,0,225,45,135,315))
  
  
  ## Decide climatic severity and find annual target area for wildfires
  if(sum(clim.severity[clim.severity$year==t,2:4])>0){  # fixed annual burnt area
    is.aba.fix <- T
    area.target <- clim.severity[clim.severity$year==t, swc+1]
  }
  else{ # stochastic annual burnt area
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
  ## Find annual target area for prescribed burns
  if(swc==4 & !is.na(pb.target.area))
    area.target <- pb.target.area
  if(swc==4 & is.na(pb.target.area)){
    # sum
    area.target <- pb.convenient.area*7
  }
  print(paste("  annual target area:", area.target))
  
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
  while(area.target>0){
    
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
    wwind <- fst.sprd.weight[1,fire.spread.type+1]
    wslope <- fst.sprd.weight[2,fire.spread.type+1]
    wfuel <- fst.sprd.weight[3,fire.spread.type+1]
    wflam <- fst.sprd.weight[4,fire.spread.type+1]
    waspc <- fst.sprd.weight[5,fire.spread.type+1]
  
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
    print(paste("  fire.id:", fire.id, "- fire size target:", fire.size.target))
    
    ## Initialize tracking variables
    fire.front <- igni.id
    is.burnt <- vector("integer", nrow(land))
    is.burnt[which(coord$cell.id==igni.id)] <- T
    aburnt.lowintens <- 0
    aburnt.highintens <- 1  # ignition always burnt, and it does in high intensity
    if(swc==4){
      aburnt.lowintens <- 1
      aburnt.highintens <- 0} 
    asupp.sprd <- 0
    asupp.fuel <- 0
    burnt.cells <- c(burnt.cells, igni.id)
    burnt.intens <- c(burnt.intens, ifelse(swc<4,T,F))
    
    ## Start speading from active cells (i.e. the fire front)
    while(length(fire.front)>0 & (aburnt.lowintens+aburnt.highintens+asupp.fuel+asupp.sprd)<fire.size.target){
      
      ## Would be it be better to have a coord df with all cells in Catalonia¿?¿?¿?
      ## Find burnable neighbours of the cells in the fire.front that haven't burnt yet
      neighs <- nn2(coord[,-1], filter(coord, cell.id %in% fire.front)[,-1], searchtype="priority", k=9)
      
      ## Get the cell.id of all the cells in the fire.front, and remove those cells already burnt
      ## May be duplicates if spreading from front cells that are actual neighbours
      neigh.id <- data.frame(cell.id=coord$cell.id[neighs$nn.idx],
                             source.id=rep(fire.front, 9),
                             dist=as.numeric(neighs$nn.dists))
      neigh.id <- mutate(neigh.id, x=cell.id-source.id) %>% left_join(default.windir, by="x") %>% select(-x) 
      neigh.id <- filter(neigh.id, cell.id %notin% burnt.cells)
      neigh.id
      
      ## Filter 'orography' for source and neigbour cells
      neigh.orography <- filter(orography, cell.id %in% c(fire.front, neigh.id$cell.id)) %>% select(cell.id, elev, aspect)
      
      ## Compute species flammability and aspect factors for spread. rate
      flam <- filter(land, cell.id %in% neigh.id$cell.id) %>% select(cell.id, spp) %>%
              left_join(spp.flammability[,c(1,fire.spread.type+1)], by="spp") 
      flam$y <- wflam * flam[,ncol(flam)]
      aspc <- filter(neigh.orography, cell.id %in% neigh.id$cell.id) %>% select(cell.id, aspect) %>%
              mutate(z=waspc*ifelse(aspect==1, 0.1, ifelse(aspect==3, 0.9, ifelse(aspect==4, 0.4, 0.3))))
      
      ## Compute spread rate, probability of burning and actual burning state (T or F)
      sprd.rate <-  left_join(neigh.id, select(neigh.orography, cell.id, elev), by="cell.id") %>%
                    left_join(select(neigh.orography, cell.id, elev), by=c("source.id"="cell.id")) %>% 
                    mutate(dif.elev=elev.x-elev.y, 
                           front.slope=wslope * pmax(pmin(dif.elev/dist,0.5),-0.5)+0.5, 
                           front.wind=wwind * (180-ifelse(abs(windir-fire.wind)>180, 
                                                   360-abs(windir-fire.wind), abs(windir-fire.wind)))/180) %>% 
                    left_join(select(flam, cell.id, y), by="cell.id") %>% 
                    left_join(select(aspc, cell.id, z), by="cell.id") %>%
                    mutate(sr.noacc=front.slope+front.wind+y+z,
                           sr=(front.slope+front.wind+y+z)*fire.strength,
                           pb=(1-exp(-sr.noacc))^rpb) %>%
                    group_by(cell.id) %>% summarize(sr.noacc=max(sr.noacc), pb=max(pb))
      sprd.rate$burning=runif(nrow(sprd.rate),0,1) < sprd.rate$pb
      sprd.rate
        
      ## Mark the cells burnt
      is.burnt[which(coord$cell.id %in% sprd.rate$cell.id[sprd.rate$burning])] <- T
      burnt.cells <- c(burnt.cells, sprd.rate$cell.id[sprd.rate$burning])
      burnt.intens <- c(burnt.intens, sprd.rate$sr.noacc[sprd.rate$burning]>ifelse(swc<4,fire.intens.th,100))
      fire.front <- sprd.rate$cell.id[sprd.rate$burning]
      aburnt.lowintens <- aburnt.lowintens + sum(sprd.rate$burning & sprd.rate$sr.noacc<=ifelse(swc<4,fire.intens.th,100))
      aburnt.highintens <- aburnt.highintens + sum(sprd.rate$burning & sprd.rate$sr.noacc>ifelse(swc<4,fire.intens.th,100))
        # print(paste("fire.id:", fire.id, " - aburnt:", aburnt.highintens+aburnt.lowintens))
    } # while 'fire'
    
    ## escriu algo sobre aquest incendi
    track.fires <- rbind(track.fires, data.frame(year=t, swc, clim.sever, fire.id, fire.spread.type, fire.wind,
                                      fire.size.target, aburnt.highintens, aburnt.lowintens,
                                      asupp.fuel, asupp.sprd))
    
    ## Update annual burnt area
    area.target <- area.target - (aburnt.lowintens+aburnt.highintens)
    
  }  #while 'year'
  
  return(list(burnt.cells=burnt.cells, burnt.intens=burnt.intens, track.fires=track.fires[-1,]))
}

