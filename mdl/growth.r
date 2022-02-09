######################################################################################
##
######################################################################################

growth <- function(land, clim, what){
 
  ## Tracking
  cat(paste(what, "growth", "\n"))
  
  ## Read coefficients
  growth.coeff <- read.table("inputfiles/GrowthSpp.txt", header=T)
  growth.coeff.shrub <- read.table("inputfiles/GrowthShrub.txt", header=T)
  
  ## Growth of species when SDM in, species when SDM out, and shrub
  aux.spp <- filter(land, spp<=13) %>% select(cell.id, spp, biom) %>% 
             left_join(select(clim, cell.id, sdm, sqi), by = "cell.id") 
  aux.spp.sdmin <- filter(aux.spp, sdm==1) %>% left_join(growth.coeff, by = c("spp", "sqi")) %>% 
                   mutate(increment = c + x*biom + x2*biom*biom ) %>%
                   mutate(increment=ifelse(increment<0,0,increment)) %>% select(cell.id, increment)
  aux.spp.sdmout <- filter(aux.spp, sdm==0) %>% left_join(filter(growth.coeff, sqi==1) %>% select(-sqi), by = "spp") %>% 
                    mutate(increment = c + x*biom/10 + x2*biom*biom ) %>%
                    mutate(increment=ifelse(increment<0,0,increment)) %>% select(cell.id, increment)
  aux.shrub <- filter(land, spp==14) %>% select(cell.id, spp, biom)  %>%
               left_join(select(clim, cell.id, sdm, sqi), by="cell.id") %>% 
               left_join(growth.coeff.shrub, by="sqi")  %>%
               mutate(increment = a*log(biom) + b ) %>% 
               mutate(increment = ifelse(increment<=0 | is.infinite(increment), b, increment)) %>% 
               select(cell.id, increment)

  ## Add increment
  all <- rbind(aux.spp.sdmin, aux.spp.sdmout, aux.shrub)
  new.biom <- left_join(land, all, by = "cell.id") %>% mutate(biom=biom+increment) 
  
  return(new.biom$biom)
}
  
