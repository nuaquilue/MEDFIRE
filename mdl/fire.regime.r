######################################################################################
###  fire.regime()
###
######################################################################################

fire.regime <- function(land, orography, pigni, coord, swc, t, burnt.cells){
  
  ## Read and load input data
  load("inputlyrs/rdata/pfst.pwind.rdata")
  prob.hot <- read.table("inputfiles/ProbHot.txt", header=T)
  prob.conv <- read.table("inputfiles/ProbConv.txt", header=T)
  aba.dist <- read.table("inputfiles/AnnualBurntAreaDist.txt", header=T)
  fs.dist <- read.table("inputfiles/FireSizeDist.txt", header=T)
  fire.supp <- read.table(paste0("inputfiles/", file.fire.suppression, ".txt"), header=T)
  clim.severity <- read.table(paste0("inputfiles/", file.clim.severity, ".txt"), header=T)
  pctg.hot.days <- read.table(paste0("inputfiles/", file.pctg.hot.days, ".txt"), header=T)
  spp.sprd.rate <- read.table("inputfiles/SppSpreadRate.txt", header=T)
  
  ## Basics
  mask <- data.frame(cell.id=1:ncell(MASK), x=MASK[])
  default.dist <- c(rep(c(sqrt(2*100^2),100),2),  rep(c(100,sqrt(2*100^2)),2))
    ## From SELES: 45-SW, 90-W, 135-NW, 180-N, 225-NE, 270-E, 315-SE, 360-S 
  default.windir <- c(135,180,225,90,270,45,360,315) # neighbors are sorted in this order: NW, N, NE, W, E, SW, S, SE
  
  ## Decide climatic severity and generate annual target area
  if(sum(clim.severity[clim.severity$year==t,2:4])>0){  # fixed annual burnt area
    is.aba.fix <- T
    clima <- ifelse(clim.severity[clim.severity$year==t, ncol(clim.severity)]==100,1,0)
    area.target <- clim.severity[clim.severity$year==t, swc+1]
  }
  else{ # stochastic annual burnt area
    is.aba.fix <- F
    if(runif(1,0,100) <= clim.severity[clim.severity$year==7, ncol(clim.severity)]){ # not-mild
      pctg <- pctg.hot.days[pctg.hot.days$year==t, swc+1]
      prob.extrem <- 1/(1+exp(-(prob.hot$inter[swc] + prob.hot$slope[swc]*pctg)))
      if(runif(1,0,100) <= prob.extrem) # extreme
        clima <- 2
      else # severe
        clima <- 1
    }
    else # mild
      clima <- 0
    area.target <- round(min(200000, max(10, 
                                         rlnorm(1, aba.dist$meanlog[aba.dist$clim==clima & aba.dist$swc==swc],
                                                aba.dist$sdlog[aba.dist$clim==clima & aba.dist$swc==swc]))))
  }
  
  
  ## Update prob.igni according to swc
  pigni <- data.frame(cell.id=land$cell.id, p=pigni*pfst.pwind[,ifelse(swc==1,1,2)])
  pigni <- filter(pigni, !is.na(p) & p>0)
  pfst.pwind$cell.id <- land$cell.id
  
  ## Pre-select the coordinates of old Mediterranean vegetation, i.e.
  ## Pinus halepensis, Pinus nigra, and Pinus pinea of age >=30 years.
  ## to compute probability of being a convective fire
  old.forest.coord <- filter(land, spp<=3 & age>=30) %>% select(cell.id) %>% left_join(coord, by = "cell.id")

  

  
  
  ## Start burning until annual area target is not reached
  while(area.target>0){
    
    ## Select an ignition point, to then decide the fire spread type, the fire suppression level,
    ## the wind direction and the target fire size according to clim and fire spread type
    igni.id <- sample(pigni$cell.id,1,replace=F,pigni$p)
    
    ## Assign the fire spread type 
    if(swc==1 | swc==3)
      fire.spread.type <- swc
    else{
      neighs <- nn2(coord[,-1], filter(coord, cell.id==igni.id)[,-1], searchtype="standard", k=100)
      nneigh <- sum(neighs$nn.dists[,]<=500)  #sqrt(2*500^2)
      old.neighs <- nn2(old.forest.coord[,-1], filter(coord, cell.id==igni.id)[,-1], searchtype="standard", k=100)
      old.nneigh <- sum(old.neighs$nn.dists[,]<=500) #sqrt(2*500^2)
      z <- filter(prob.conv, clim==clima)$inter + filter(prob.conv, clim==clima)$slope*(old.nneigh/nneigh)*100
      1/(1+exp(-z))
      fire.spread.type <- ifelse(runif(1,0,1)<=1/(1+exp(-z)),2,3)
    }
  
    ## Assign the fire suppression levels
    sprd.th <- filter(fire.supp, clim==clima, fst==fire.spread.type)$sprd.th
    fuel.th <- filter(fire.supp, clim==clima, fst==fire.spread.type)$fuel.th
    
    ## Assign the main wind direction according to the fire spread type
    ## Wind directions inherit from SELES (to be reviewed)
    ## 45-SW, 90-W, 135-NW, 180-N, 225-NE, 270-E, 315-SE, 360-S 
    if(fire.spread.type==1)
      wind.dir <- sample(c(180,90,135), 1, replace=F, p=filter(pfst.pwind,cell.id==igni.id)[3:5])
    if(fire.spread.type==2)  
      wind.dir <- sample(c(360,45,315), 1, replace=F, p=c(80,10,10))
    if(fire.spread.type==3)  
      wind.dir <- sample(seq(45,360,45), 1, replace=F)
    
    ## Derive target fire size from a power-law according to clima and fire.spread.type 
    log.size <- seq(1.7, 5, 0.01)
    log.num <- filter(fs.dist, clim==clima, fst==fire.spread.type)$intercept +
               filter(fs.dist, clim==clima, fst==fire.spread.type)$slope * log.size
    fire.size.target <- sample(round(10^log.size), 1, replace=F, prob=10^log.num)
    fire.size.target
    
    ## Bound fire.size.target to not exceed remaining area.target
    if(fire.size.target>area.target)
      fire.size.target <- area.target
      
    ## Initialize tracking variables
    burnt.cells <- igni.id
    fire.front <- igni.id
    aburnt.lowintens <- 0
    aburnt.highintens <- 1  # ignition always burnt, and it does in high intensity
    asupp.sprd <- 0
    asupp.fuel <- 0
    
    ## MY GOSH! START SPREADING FROM FIRE FRONT!!! 
    ## fire.size <- aburnt.lowintens+aburnt.highintens+asupp.fuel+asupp.sprd
    while(length(fire.front)>0 | (aburnt.lowintens+aburnt.highintens+asupp.fuel+asupp.sprd)<fire.size.target){
      
      ## Find burnable neighbours of the cells in the fire.front that haven't burnt yet
      neighs <- nn2(coord[,-1], filter(coord, cell.id %in% fire.front)[,-1], searchtype="priority", k=9)
       
          # fire.front
      neigh.id <- coord$cell.id[t(neighs$nn.idx[,-1])]  # remove source cells
      
      
       
       
       filter(mask, cell.id %in% neigh.id) %>% mutate(source.id=rep(sort(fire.front),each=8)) %>%
         left_join(select(orography, cell.id, elev), by="cell.id") %>%
          left_join(select(orography, cell.id, elev), by=c("source.id"="cell.id")) %>% 
         mutate(dif.elev=elev.x-elev.y, dist=rep(default.dist, length(fire.front)), 
                fire.slope=pmax(pmin(dif.elev/dist,0.5),-0.5)+0.5, #kk=cell.id-source.id,
                windir=rep(default.windir, length(fire.front)),
                a =ifelse(abs(windir-wind.dir)>180, 360-abs(windir-wind.dir), abs(windir-wind.dir)),
                fire.wind=(180-a)/180) %>% filter(!is.na(x)) 
       
       
      flam <- filter(land, cell.id %in% neigh.id) %>% left_join(spp.sprd.rate[,c(1,fire.spread.type+1)], by="spp") 
      flam <- as.vector(flam[,ncol(flam)])  
      
      aspc <- filter(orography, cell.id %in% as.numeric(neigh.id)) %>% select(aspect)
      aspc <- ifelse(unlist(aspc)==1, 0.1, ifelse(unlist(aspc)==3, 0.9, ifelse(unlist(aspc)==4, 0.4, 0.3)))
      
    }
    
  }  #while
  
  
  a <- filter(coord, cell.id %in% fire.front)[,-1]
  b <- coord[t(neighs$nn.idx[,-1]),]
  names(b)[2:3] <- c("xneig","yneigh")
  
  
  b <- cbind(b, rbind(a[1,],a[1,],a[1,],a[1,],a[1,],a[1,],a[1,],a[1,],
        a[2,],a[2,],a[2,],a[2,],a[2,],a[2,],a[2,],a[2,]) )
  b$dif <- abs(b$xneig-b$x)+abs(b$yneig-b$y)
  
  
}

