######################################################################################
###  read.sdm()
###
### L'ordre d'espècies segueix sent:
### 1"Pinus sylvestris",2"Pinus uncinata",3"Pinus pinea",4"Pinus halepensis",5"Pinus nigra",
### 6"Pinus pinaster",7"Abies alba",8"Quercus faginea",9"Quercus ilex",10"Quercus suber",
### 11"Quercus humilis",12"Fagus sylvatica",13"OTrees."
######################################################################################

read.sdm <- function(){

  for(p in c(1)){
    
    cat(paste("Threshold", p), "/n")
    
    ## Update list of SDMs
    ## Change resolution (1 km, even if the name is "_100m") and extend to match the default
    ## Build a data frame with MASK and SDMs per spp
    load(paste0(work.path, "/inputlyrs/asc/sdm/SDM_p", p, "_", clim.scn, "_", decade, "_100m.rdata"))
    sdm  <- data.frame(cell.id=1:ncell(MASK), mask=MASK[])
    for(i in order.spp.sdm){
      sdm.proj[[i]] <- disaggregate(sdm.proj[[i]], fact=c(10,10))
      sdm.proj[[i]] <- extend(sdm.proj[[i]], extCat)
      sdm <- cbind(sdm, data.frame(spp=sdm.proj[[i]][]))
    }
    names(sdm)[3:15] <- species
    # for(i in 1:13)
    #   sdm <- cbind(sdm, data.frame(spp=sdm.proj[[i]][]))
    
    ## Save SDM of all spp in a data.frame, for cells in CAT
    sdm <- sdm[!is.na(sdm$mask),]
    sdm <- select(sdm, -mask)
    names(sdm)[-1] <- paste0("sdm.", species)
    sdm$sdm.shrub <- 1
    save(sdm, file=paste0(work.path, "/inputlyrs/rdata/sdm_", p, "p_", clim.scn, "_", decade, ".rdata"))
    
  } #p
    
}


