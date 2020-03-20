write.track.lyrs <- function(scn.name, track.spread){
  
  library(sp)
  library(raster)
  
  print("... writing fire spread layers")
  
  load("inputlyrs/rdata/mask.rdata")
  load("inputlyrs/rdata/orography.rdata")
  
  dta <- data.frame(cell.id=1:ncell(MASK), mask=MASK[]) %>% 
    left_join(track.spread, by="cell.id") %>% left_join(orography, by="cell.id")
  
  ID <- MASK; ID[] <- dta$fire.id
  writeRaster(ID, paste0("outputs/", scn.name, "/lyr/FireId.tif"), 
              format="GTiff", overwrite=T, NAflag=-1)
  STEP <- MASK; STEP[] <- dta$step
  writeRaster(STEP, paste0("outputs/", scn.name, "/lyr/FireStep.tif"), 
              format="GTiff", overwrite=T, NAflag=-1)
  SPP <- MASK; SPP[] <- dta$spp
  writeRaster(SPP, paste0("outputs/", scn.name, "/lyr/SpeciesBurnt.tif"), 
              format="GTiff", overwrite=T, NAflag=-1)
  SLOPE <- MASK; SLOPE[] <- dta$front.slope
  writeRaster(SLOPE, paste0("outputs/", scn.name, "/lyr/FireSlope.tif"), 
              format="GTiff", overwrite=T, NAflag=-1)
  WIND <- MASK; WIND[] <- dta$front.wind
  writeRaster(WIND, paste0("outputs/", scn.name, "/lyr/FireWind.tif"), 
              format="GTiff", overwrite=T, NAflag=-1)
  INTENS <- MASK; INTENS[] <- dta$fi
  writeRaster(INTENS, paste0("outputs/", scn.name, "/lyr/FireIntens.tif"), 
              format="GTiff", overwrite=T, NAflag=-1)
  RATE <- MASK; RATE[] <- dta$sr
  writeRaster(RATE, paste0("outputs/", scn.name, "/lyr/SpreadRate.tif"), 
              format="GTiff", overwrite=T, NAflag=-1)
  PB <- MASK; PB[] <- dta$pb.fi
  writeRaster(PB, paste0("outputs/", scn.name, "/lyr/ProbBurn.tif"), 
              format="GTiff", overwrite=T, NAflag=-1)
  BURN <- MASK; BURN[] <- dta$burning.fi
  writeRaster(BURN, paste0("outputs/", scn.name, "/lyr/Burning.tif"), 
              format="GTiff", overwrite=T, NAflag=-1)
  SLOPE <- MASK; SLOPE[] <- dta$slope
  writeRaster(SLOPE, paste0("outputs/", scn.name, "/lyr/Slope.tif"), 
              format="GTiff", overwrite=T, NAflag=-1)
  
}



list.scn <- paste0("TestFire", LETTERS[1:5])
self.extinguishing <- function(list.scn){
  
  library(tidyverse)
  
  fires <- numeric()
  for(scn in list.scn){
    aux <- read.table(paste0("C:/WORK/MEDMOD/SpatialModelsR/MEDFIRE/outputs/", scn, "/Fires.txt"),
                      header=T)
    fires <- rbind(fires, data.frame(scn, aux))
    
  }
  
  # Only 5% fires reach target size
  cat(paste("Percentage of fires that reach the target size:",
      round(100*sum(fires$aburnt.highintens+fires$aburnt.lowintens>=fires$atarget) /nrow(fires)) ), "%\n")
  
  group_by(fires, scn) %>% summarise(nfire=length(atarget))
  # 1 TestFireA     5
  # 2 TestFireB     9
  # 3 TestFireC    19
  # 4 TestFireD    27
  # 5 TestFireE    75

  ## Distribution of remanent areas
  ggplot(data=filter(fires, rem>0), aes(fires$rem[fires$rem>0])) + geom_histogram()
  
}
