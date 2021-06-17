forest.areas <- function(land, harvest){
  dta <- data.frame(NA)
  # total forest 
  dta$forest <- sum(land$spp<=13)
  # harvestable species
  dta$spp.harvestable <- sum(land$spp<=13 & land$spp!=9)
  # from harvestable species, non-protected, national.park  
  aux <- filter(land, spp<=13 & spp!=9) %>% left_join(harvest, by="cell.id")
  dta$non.protect <- nrow(filter(aux, enpe==0))
  dta$national.park <- nrow(filter(aux, enpe==1))
  dta$enpe <- nrow(filter(aux, enpe>1))
  dta$no.park <- nrow(filter(aux, enpe!=1))
  
  # from harvestable species, not in national.park, slope <=30% 
  dta$slope30.nopark <- nrow(filter(aux, enpe!=1, slope.pctg<=30))
  dta$slope30.nopark.distpaht1.5 <- nrow(filter(aux, enpe!=1, slope.pctg<=30, dist.path<=1500))
  dta$slope30.nopark.distpaht2.2 <- nrow(filter(aux, enpe!=1, slope.pctg<=30, dist.path<=2200))
  
  return(dta=dta[,-1])
    
}
                             
                             
                             
                             track.areas <- data.frame(forest=NA, spp.harvestable=NA, non.protected=NA, slope50=NA, slope30=NA, distpath1.5=NA,
                          distpath2.2=NA) 