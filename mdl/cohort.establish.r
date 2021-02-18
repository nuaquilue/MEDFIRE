######################################################################################
##
######################################################################################

cohort.establish <- function(land, coord, orography, clim, sdm){
  
  ## Tracking
  cat("Cohort establishment", "\n") 
  
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
  nneigh <- seq(3,201,2) + cumsum(seq(1,200,2)*2)

  ## Coordinates of killed cells, then find their closest neighbours 
  ## Important to sort killed.cells by cell.id, so the order of the list of neigh matches
  killed.cells <- filter(land, tsdist==0, typdist=="drght") 
  killed.cells <- left_join(killed.cells, filter(coord, cell.id %in% killed.cells$cell.id), by = "cell.id") 
  killed.cells <- killed.cells[order(killed.cells$cell.id),] 
  neighs <- nn2(coord[,-1], select(killed.cells,x,y),  searchtype="priority", k=nneigh[colon.rad])
  
  ## Retrive the species of the neighbors (first column is cell.id of killed.cells)
  aux <- matrix(land$spp[neighs$nn.idx[,-1]], nrow=nrow(killed.cells), ncol=nneigh[colon.rad]-1)*
                ifelse(matrix(neighs$nn.dists[,-1], nrow=nrow(killed.cells), ncol=nneigh[colon.rad]-1)<=colon.rad*100,1,0) 
                          
  ## Return presence (TRUE) or abscence (FALSE) of neighbor species, 
  ## assume that shrubs are always presence in no matter which neighbourhood
  killed.cells <- cbind(killed.cells, t(apply(aux, 1, count.spp))>=1 )
  names(killed.cells)[10:ncol(killed.cells)] <- paste0("X", names(killed.cells)[10:ncol(killed.cells)])
  killed.cells$X14 <- T
  
  ## Look up cells killed by drought, add sqi data, then add the sencondary species
  ## (according to dominant spp and sqi), then add sdm of all tree species and finally
  ## add the number of forest spp in the neighbourhood
  killed.cells <- left_join(killed.cells, 
                            filter(clim, cell.id %in% killed.cells$cell.id) %>% select(cell.id, sqi), by = "cell.id") %>%
                  left_join(secondary.spp, by = c("spp", "sqi")) %>% left_join(sdm, by = "cell.id") 
                  
  
  ## Select spp among available
  new.cohort <- data.frame(cell.id=killed.cells$cell.id,
                           spp=apply(select(killed.cells, phalepensis:shrub) * 
                                       select(killed.cells, sdm.phalepensis:sdm.shrub) * 
                                         select(killed.cells, X1:X14), 1, select.cohort), 
                           biom=0, sdm=1, age=1)
  
  ## Join climatic and orographic variables to compute sq and then sqi
  new.cohort <- left_join(new.cohort, filter(clim, cell.id %in% new.cohort$cell.id) %>% select(cell.id, temp, precip), by = "cell.id") %>%
                left_join(filter(orography, cell.id %in% new.cohort$cell.id) %>% select(cell.id, aspect, slope.stand), by = "cell.id") %>%
                left_join(site.quality.spp, by = "spp") %>% left_join(site.quality.index, by = "spp") %>% 
                mutate(aux=c0+c_mnan*temp+c2_mnan*temp*temp+c_plan*precip+c2_plan*precip*precip+
                         c_aspect*ifelse(aspect!=1,0,1)+c_slope*slope.stand) %>%
                mutate(sq=1/(1+exp(-1*aux))) %>% mutate(sqi=ifelse(sq<=p50, 1, ifelse(sq<=p90, 2, 3))) %>%
                select(cell.id, spp, temp, precip, biom, age, sdm, sqi)
  shrub.cells <- new.cohort$cell.id[new.cohort$spp==14]
  sqi.shrub <- filter(clim, cell.id %in% shrub.cells) %>% select(spp, temp, precip) %>% 
               mutate(aux.brolla=site.quality.shrub$c0_brolla+site.quality.shrub$c_temp_brolla*temp+site.quality.shrub$c_temp2_brolla*temp*temp+site.quality.shrub$c_precip_brolla*precip+site.quality.shrub$c_precip2_brolla*precip*precip,
                      aux.maquia=site.quality.shrub$c0_maquia+site.quality.shrub$c_temp_maquia*temp+site.quality.shrub$c_temp2_maquia*temp*temp+site.quality.shrub$c_precip_maquia*precip+site.quality.shrub$c_precip2_maquia*precip*precip,
                      aux.boix=site.quality.shrub$c0_boix+site.quality.shrub$c_temp_boix*temp+site.quality.shrub$c_temp2_boix*temp*temp+site.quality.shrub$c_precip_boix*precip+site.quality.shrub$c_precip2_boix*precip*precip,
                      sq.brolla=1/(1+exp(-1*aux.brolla)), sq.maquia=1/(1+exp(-1*aux.maquia)), sq.boix=1/(1+exp(-1*aux.boix))) # %>% 
  if(is.infinite(max(sqi.shrub$sq.brolla)) | is.infinite(max(sqi.shrub$sq.maquia))  | is.infinite(max(sqi.shrub$sq.boix)) ){
    write.table(sqi.shrub, paste0(out.path, "/ErrorSQIshrub.txt"), quote=F, row.names=F, sep="\t")
    cat("INF in sqi.shrub")  
  }
  sqi.shrub <- mutate(sqi.shrub, sqest.brolla=sq.brolla/max(sq.brolla), sqest.maquia=sq.maquia/max(sq.maquia), sqest.boix=sq.boix/max(sq.boix),
                      sqi=ifelse(sqest.brolla>=sqest.maquia & sqest.brolla>=sqest.boix, 1,
                            ifelse(sqest.maquia>=sqest.brolla & sqest.maquia>=sqest.boix, 2,
                              ifelse(sqest.boix>=sqest.brolla & sqest.boix>=sqest.maquia, 3, 0))))
  new.cohort$sqi[new.cohort$spp==14] <- sqi.shrub$sqi
  
  # toc()
  return(select(new.cohort, -temp, -precip))
}

