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

############ AUTO EXTINCTION ############
self.extinguishing <- function(list.scn){
  for(scn in list.scn){
    fires <- read.table(paste0("d:/MEDMOD/SpatialModelsR/MEDFIRE/outputs/", scn, "/Fires.txt"), header=T)
    fires$scn <- scn
    cat(paste("Percentage of fires that reach the target size:",
      round(100*sum(fires$aburnt.highintens+fires$aburnt.lowintens>=fires$atarget) /nrow(fires)) ), "%\n")
    cat(paste("aBurnt/aTarget:",
              round(100*sum(fires$aburnt.highintens+fires$aburnt.lowintens)/sum(fires$atarget))), "%\n")
    
    ## Distribution of remanent areas
    ggplot(data=filter(fires, rem>0), aes(fires$rem[fires$rem>0])) + geom_histogram(bins = 10)
  }  
}
list.scn <- c("Scn_NewCalibOnly10_1by1")
self.extinguishing(list.scn)


############ PERCENTAGE BURNT, SUPPRESS, REMANENT ############
scn <- list.scn[1]
fires <- read.table(paste0("outputs/", scn, "/Fires.txt"), header=T) %>%
         mutate(aburnt=aburnt.highintens+aburnt.lowintens, asupp=asupp.fuel+asupp.sprd)
a <- group_by(fires, year) %>% summarize(at=sum(atarget), ab=sum(aburnt), as=sum(asupp), rem=sum(rem),
                                         pb=round(ab/at*100,1), ps=round(as/at*100,1), pr=round(rem/at*100,1))
a
mean(a$pb); mean(a$ps); mean(a$pr)
