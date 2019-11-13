######################################################################################
##
######################################################################################

forest.mgmt <- function(land, coord, orography){
  
  ## Tracking
  print("Forest Management")
  
  ## Read management rules
  mgmt.rules <- unlist(read.table("inputfiles/MgmtRules.txt", header=T))
  
  ## Num of neighbours in a circular neighbourhood according to radius (radius is in pixels)
  ## Assume that the neighbourhood is a star, with the maximum number of pixels in the
  ## east-west or north-south direction is 2*radius + 1 (1 is the center cell).
  ## The num of pixels is sequentially: 3+1*2, 5+3*2+1*2, 7+5*2+3*2+1*2, ...
  nneigh <- seq(3,41,2) + cumsum(seq(1,40,2)*2)
  
  ## find cells suitable for management: slope.pctg <=30% and dist.path <= 500m
  suit.harvest <- left_join(land, select(orography, cell.id, slope.pctg, dist.path)) %>%
                  filter(land$spp <= 13 & slope.pctg <= 30 & dist.path <= 500) %>%
                  filter(land$spp != 9) # exclude quercus suber, not managed for sawlogs neither wood
  
  
  ## Coordinates of killed cells and their closest neighbours (do not count for the cell itself)
  shrub.coord <- filter(land, tsdist>=20, spp==14) %>% select(cell.id) 
  shrub.coord <- data.frame(cell.id=shrub.coord[sample(1:nrow(shrub.coord), 10000, replace=F),1])
  shrub.coord <- left_join(shrub.coord, coord)
  neigh.id <- nn2(coord[,-1], shrub.coord[,-1],  searchtype="priority", k=nneigh[shrub.colon.rad]) 
  neigh.id <- neigh.id$nn.idx  # dim 10.000 x 61
  
}