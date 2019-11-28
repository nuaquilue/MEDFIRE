######################################################################################
###  read.climatic.vars()
###
######################################################################################

read.climatic.vars <- function(){
  
  library(raster)
  library(RANN)  # for nn2()
  library(tidyverse)
  
  ## Mask of the study area
  load("inputlyrs/rdata/mask.rdata")
  
  ## List the name of the forest species
  species <- c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster", "puncinata",
               "aalba", "qilex", "qsuber", "qfaginea", "qhumilis", "fsylvatica", "other")

  ## Default extent of raster maps of Catalonia  
  extCat <- extent(c(250000, 540000, 4480000, 4760000))
  
  for(clim.scn in c("rcp45", "rcp85")){
    for(decade in seq(10,90,10)){
      
      print(paste("Building:", clim.scn, "-", decade))
      
      ## Update annual minimum temp and annual precip
      TEMP <- raster(paste0("inputlyrs/asc/", clim.scn, "/", decade, "/mnan.asc"))
      TEMPcat <- setExtent(TEMP, extCat, keepres=T, snap=FALSE)
      TEMP100 <- disaggregate(TEMPcat, fact=c(10,10))
      PRECIP <- raster(paste0("inputlyrs/asc/", clim.scn, "/", decade, "/plan.asc"))
      PRECIPcat <- setExtent(PRECIP, extCat, keepres=T, snap=FALSE)
      PRECIP100 <- disaggregate(PRECIPcat, fact=c(10,10))
      rm(TEMP); rm(TEMPcat); rm(PRECIP); rm(PRECIPcat)
      
      ## Update list of SDMs
      load(paste0("inputlyrs/rdata/SDM_", clim.scn, "_", decade, "_100m.rdata"))
      
      ## Build a data frame with MASK, TEMP, PRECIP and SDMs per spp
      clim <- data.frame(cell.id=1:ncell(MASK), mask=MASK[], temp=TEMP[], precip=PRECIP[])
      for(spp in species){
        SDM <- raster(paste0(mdl.path, "/inputlyrs/asc/SDM_", spp, "_", decade,"_100m.asc"))
        clim <- cbind(clim, data.frame(spp=SDM[]))
        names(clim)[ncol(clim)] <- spp
      }
      
      ## Because som CAT cells have no value in TEMP layer, phacoyte
      clim <- cbind(clim, coordinates(MASK))
      # Find 8 neighbours for non-forest cells
      nastemp <- filter(clim, mask==1, is.na(temp))
      if(nrow(nastemp)>0){
        neigh.id <- nn2(select(clim,x,y), filter(clim, mask==1, is.na(temp))%>%select(x,y),
                        searchtype='priority', k=25)
        neigh.id <-  neigh.id$nn.idx 
        neigh.temp <- matrix(clim$temp[neigh.id[,-1]], nrow=nrow(neigh.id), ncol=ncol(neigh.id)-1) 
        clim$temp[!is.na(clim$mask) & is.na(clim$temp)] <- apply(neigh.temp, 1 ,mean, na.rm=T)
      }
      
      ## Save SDM of all spp in a data.frame, for cells in CAT
      sdm <- clim[!is.na(clim$mask),]
      sdm <- select(sdm, -mask, -temp, -precip, -x, -y) 
      names(sdm)[-1] <- paste0("sdm.",species)
      sdm$sdm.shrub <- 1
      save(sdm, file=paste0("inputlyrs/rdata/sdm_", clim.scn, "_", decade, ".rdata"))
      
      ## Now, again, keep only cells from CAT
      clim  <-  clim[!is.na(clim$mask),]
      clim <- select(clim, cell.id, temp, precip)
      save(clim, file=paste0("inputlyrs/rdata/climate_", clim.scn, "_", decade, ".rdata"))
      
    } # decade
  } # clim.scn
  
  
}
