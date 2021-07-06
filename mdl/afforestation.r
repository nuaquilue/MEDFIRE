######################################################################################
##
######################################################################################

afforestation <- function(land, coord, orography, clim, sdm, utm){
  
  ## Tracking
  cat("Afforestation", "\n") 
  
  ## Read species afforestation model
  afforest.mdl <- read.table("inputfiles/afforestation.mdl.txt", header=T)
  seed.pressure <- unlist(read.table("inputfiles/SppSeedPressure.txt", header=T))
  
  ## Read coefficients of site quality models
  site.quality.spp <- read.table("inputfiles/SiteQualitySpp.txt", header=T)
  site.quality.index <- read.table("inputfiles/SiteQualityIndex.txt", header=T)
  
  ## Join utm and sdm info to land
  land.utm <- land %>% select(cell.id, spp, biom, age) %>% left_join(select(clim, cell.id, sdm), by="cell.id") %>% 
              left_join(utm, by="cell.id") 
  
  ## Calculate the percentage of old forest within its climatic niche per utm cell
  utm.forest <- group_by(land.utm, utm) %>% summarise(nneigh=length(utm), old.neigh=sum(spp<=13 & age>=15 & sdm==1)) %>% 
      mutate(pct=old.neigh/nneigh)
  old.forest <- land.utm %>% select(cell.id, utm) %>% left_join(select(utm.forest, utm, pct), by="utm") %>% 
    select(-utm)
  # save(old.forest, file="inputlyrs/rdata/oldforest.rdata") #remove
  
  ## Put together all explanatory variables of the afforestation model
  dta <- select(land.utm, cell.id, spp) %>% filter(spp==14) %>% 
          left_join(select(orography, cell.id, elev, slope), by="cell.id") %>% 
          left_join(select(clim, cell.id, temp, precip), by="cell.id") %>% 
          left_join(old.forest, by="cell.id")
  
  ## Apply the afforestation model
  dta$z <- afforest.mdl$intrc + afforest.mdl$elev*dta$elev + afforest.mdl$slope*dta$slope +
            afforest.mdl$precip*dta$precip + afforest.mdl$pct*dta$pct +
            afforest.mdl$elev2*(dta$elev^2) + afforest.mdl$slope2*(dta$slope^2) + 
            afforest.mdl$precip2*(dta$precip^2) + afforest.mdl$pct2*(dta$pct^2) 
  dta$p <- 1/(1+exp(-1*dta$z))
  dta$p <- 1-(1-dta$p)^(1/30)  # 30y period of obs, from 1987 to 2017
  dta$z <- runif(nrow(dta), 0, 1) <= dta$p
  
  ## For those cells to afforestate, check if actually there are mature forest surrounding them
  dta <- dta %>% filter(z)
  
  ## Num of neighbours in a circular neighbourhood according to radius (radius is in pixels)
  ## Assume that the neighbourhood is a star, with the maximum number of pixels in the
  ## east-west or north-south direction is 2*radius + 1 (1 is the center cell).
  ## The num of pixels is sequentially: 3+1*2, 5+3*2+1*2, 7+5*2+3*2+1*2, ...
  nneigh <- seq(3,41,2) + cumsum(seq(1,40,2)*2)
  
  ## Coordinates of shruby cells and their closest neighbours (do not count for the cell itself)
  shrub.coord <- dta %>% select(cell.id, z) %>% left_join(coord, by = "cell.id") %>% select(-z)
  neigh.id <- nn2(coord[,-1], shrub.coord[,-1],  searchtype="priority", k=nneigh[colon.rad]) 
  neigh.id <- neigh.id$nn.idx  
  
  ## Identify the species of the mature forest neigbhours that are currently within their climatic niche
  neigh.spp <- matrix(land$spp[neigh.id[,-1]], nrow=nrow(neigh.id), ncol=ncol(neigh.id)-1) # spp of the neighbours
  for(i in 1:13){
    neigh.sdm <- matrix(sdm[neigh.id[,-1],i+1], nrow=nrow(neigh.id), ncol=ncol(neigh.id)-1) # sdm of the neighbours
    neigh.spp[neigh.spp==i] <- neigh.spp[neigh.spp==i] * neigh.sdm[neigh.spp==i] # mask neighbours with sdm 0
  }
  neigh.spp <- neigh.spp * matrix(land$age[neigh.id[,-1]]>=15, nrow=nrow(neigh.id), ncol=ncol(neigh.id)-1) # mask young neighbours 
  
  ## Count the presence of each species, mask its presence if the species is out the climatic niche in
  ## the target location, and give double weight to conifers
  count.neigh.spp <- t(apply(neigh.spp, 1, count.spp.narm)) * 
                     matrix(seed.pressure, nrow=nrow(neigh.id), ncol=13, byrow=T) *
                     matrix(c(sdm[neigh.id[,1],1+1], sdm[neigh.id[,1],1+2], sdm[neigh.id[,1],1+3],
                              sdm[neigh.id[,1],1+4], sdm[neigh.id[,1],1+5], sdm[neigh.id[,1],1+6],
                              sdm[neigh.id[,1],1+7], sdm[neigh.id[,1],1+8], sdm[neigh.id[,1],1+9],
                              sdm[neigh.id[,1],1+10], sdm[neigh.id[,1],1+11], sdm[neigh.id[,1],1+12],
                              sdm[neigh.id[,1],1+13]), nrow=nrow(neigh.id), ncol=13)

  ## Assign new species to those cells to afforestate, if available
  dta$spp <- apply(count.neigh.spp, 1, select.spp) 
  dta <- dta %>% filter(!is.na(spp)) 
  
  ## Join climatic and orographic variables to compute sq and then sqi
  res <- dta %>% left_join(select(orography, cell.id, aspect, slope.stand), by = "cell.id") %>%
         left_join(site.quality.spp, by = "spp") %>% left_join(site.quality.index, by = "spp") %>% 
         mutate(aux=c0+c_mnan*temp+c2_mnan*temp*temp+c_plan*precip+c2_plan*precip*precip+
                  c_aspect*ifelse(aspect!=1,0,1)+c_slope*slope.stand) %>%
             mutate(sq=1/(1+exp(-1*aux))) %>% mutate(sqi=ifelse(sq<=p50, 1, ifelse(sq<=p90, 2, 3))) %>%
             select(cell.id, spp, sqi) 
  
  return(res)
}

