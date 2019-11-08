######################################################################################
##
######################################################################################

growth <- function(land, clim){
 
  ## Tracking
  print("Species growth")

  ## Read coefficients
  growth.coeff <- read.table("inputfiles/GrowthSpp.txt", header=T)
  growth.coeff.shrub <- read.table("inputfiles/GrowthShrub.txt", header=T)
  
  ## Growth of species when SDM in, species when SDM out, and shrub
  aux.spp <- filter(land, spp<=13) %>% select(cell.id, spp, biom) 
  aux.spp <- cbind(aux.spp, filter(clim, spp<=13) %>% select(sdm, sqi)) 
  aux.spp.sdmin <- filter(aux.spp, sdm==1) %>% left_join(growth.coeff) %>% 
                   mutate(increment = x*biom/10 + x2*(biom/10)^2 ) %>%
                   mutate(increment=ifelse(increment<c,c,increment)) %>% select(cell.id, increment)
  aux.spp.sdmout <- filter(aux.spp, sdm==0) %>% left_join(filter(growth.coeff, sqi==1)) %>% 
                    mutate(increment = x*biom/10 + x2*(biom/10)^2 ) %>%
                    mutate(increment=ifelse(increment<c,c,increment)) %>% select(cell.id, increment)
  aux.shrub <- filter(land, spp==14) %>% select(cell.id, spp, biom) 
  aux.shrub <- cbind(aux.shrub, filter(clim, spp==14) %>% select(sdm, sqi))         
  aux.shrub <- left_join(aux.shrub, growth.coeff.shrub)  %>%
               mutate(increment = a*log(biom/10000) + b ) %>% 
               mutate(increment = ifelse(increment<=0, b*1000, increment)) %>% select(cell.id, increment)
  
  
  ## Join increment
  all <- rbind(aux.spp.sdmin, aux.spp.sdmout, aux.shrub)
  new.biom <- left_join(land, all) %>% mutate(biom=biom+increment*10) %>% select(biom)
  
  return(new.biom$biom)
    
}
  