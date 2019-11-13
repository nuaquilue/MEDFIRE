######################################################################################
##
######################################################################################

cohort.establish <- function(land, clim, orography, sdm, coord, spp.distrib.rad, drought.id){
  
  ## Tracking
  print("Cohort establishment")
  
  ## Read matrix of secondary species according to species - sqi
  secondary.spp <- read.table("inputfiles/SecondarySpp.txt", header=T)
  
  ## Read coefficients of site quality models
  site.quality.spp <- read.table("inputfiles/SiteQualitySpp.txt", header=T)
  site.quality.index <- read.table("inputfiles/SiteQualityIndex.txt", header=T)
  site.quality.shrub <- read.table("inputfiles/SiteQualityShrub.txt", header=T)
  
  ## Num of neighbours in a circular neighbourhood according to radius (radius is in pixels)
  ## Assume that the neighbourhood is a star, with the maximum number of pixels in the
  ## east-west or north-south direction is 2*radius + 1 (1 is the center cell).
  ## The num of pixels is sequentially: 3+1*2, 5+3*2+1*2, 7+5*2+3*2+1*2, ...
  nneigh <- seq(3,41,2) + cumsum(seq(1,40,2)*2)

  ## Coordinates of killed cells and their closest neighbours (do not count for the cell itself)
  killed.coord <- filter(land, tsdist==0, distype==drought.id) %>% select(cell.id) %>% left_join(coord)
  neigh.id <- nn2(coord[,-1], killed.coord[,-1],  searchtype="priority", k=nneigh[spp.distrib.rad])
  neigh.id <- neigh.id$nn.idx
  neigh.spp <- data.frame(cell.id=coord$cell.id[neigh.id[,1]],
               matrix(land$spp[neigh.id[,-1]], nrow=nrow(neigh.id), ncol=ncol(neigh.id)-1) )
    
  ## Count number of neighbors per spp, assume that always there's a shrub cell in the neighbourhood
  neigh.spp <- data.frame(cell.id=coord$cell.id[neigh.id[,1]],
                          t(apply(neigh.spp[,-1], 1, count.spp))>=1 )
  neigh.spp$X14 <- T
  
  ## Look up cells killed by drought, add sqi data, then add the sencondary species
  ## (according to dominant spp and sqi), then add sdm of all tree species and finally
  ## add the number of forest spp in the neighbourhood
  killed <- filter(land, tsdist==0, distype==drought.id) %>% left_join(select(clim, cell.id, sqi)) %>%
            left_join(secondary.spp) %>% left_join(sdm) %>% left_join(neigh.spp)
    
  ## Select spp among available
  new.cohort <- data.frame(cell.id=killed$cell.id,
                           spp=apply(select(killed, phalepensis:shrub) * 
                                       select(killed, sdm.phalepensis:sdm.shrub) * select(killed, X1:X14), 1, select.cohort), 
                           biom=0, sdm=1 )
  
  ## Join climatic and orographic variables to compute sq and then sqi
  new.cohort <- left_join(new.cohort, select(clim, cell.id, temp, precip)) %>% 
                left_join(select(orography, cell.id, aspect, slope)) %>%
                left_join(site.quality.spp) %>% left_join(site.quality.index) %>% 
                mutate(aux=c0+c_temp*temp+c_temp2*temp*temp+c_precip*precip+c_precip2*precip*precip+c_aspect*ifelse(aspect!=1,0,1)+c_slope*slope/10) %>%
                mutate(sq=1/(1+exp(-1*aux))) %>% mutate(sqi=ifelse(sq<=th_50, 1, ifelse(sq<=th_90, 2, 3))) %>%
                select(cell.id, spp, temp, precip,  biom, sdm, sqi)
  sqi.shrub <- filter(new.cohort, spp==14) %>% select(spp, temp, precip) %>% left_join(site.quality.shrub) %>%
               mutate(aux.brolla=c0_brolla+c_temp_brolla*temp+c_temp2_brolla*temp*temp+c_precip_brolla*precip+c_precip2_brolla*precip*precip,
                      aux.maquia=c0_maquia+c_temp_maquia*temp+c_temp2_maquia*temp*temp+c_precip_maquia*precip+c_precip2_maquia*precip*precip,
                      aux.boix=c0_boix+c_temp_boix*temp+c_temp2_boix*temp*temp+c_precip_boix*precip+c_precip2_boix*precip*precip,
                      sq.brolla=1/(1+exp(-1*aux.brolla)), sq.maquia=1/(1+exp(-1*aux.maquia)), sq.boix=1/(1+exp(-1*aux.boix)),
                      sqi=ifelse(sq.brolla>=sq.maquia & sq.brolla>=sq.maquia, 1,
                            ifelse(sq.maquia>=sq.brolla & sq.maquia>=sq.boix, 2,
                             ifelse(sq.boix>=sq.brolla & sq.boix>=sq.maquia, 3, 0))) )
  new.cohort$sqi[new.cohort$spp==14] <- sqi.shrub$sqi

  return(select(new.cohort, -temp, -precip))
}


count.spp <- function(x){
  return(c(sum(x==1), sum(x==2), sum(x==3), sum(x==4), sum(x==5), sum(x==6), sum(x==7),
           sum(x==8), sum(x==9), sum(x==10), sum(x==11), sum(x==12), sum(x==13)))
}


select.cohort <- function(x){
  return(sample(1:14, 1, replace=F, prob=x))
}
