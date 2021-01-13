rm(list=ls())
library(raster)
library(landscapemetrics)
library(tidyverse)

############ AUTO EXTINCTION ############
self.extinguishing <- function(list.scn){
  result <- data.frame(scn=NA, pctg.reach=NA, ab.at=NA)
  for(scn in list.scn){
    fires <- read.table(paste0(getwd(),"/outputs/", scn, "/Fires.txt"), header=T)
    aux <- data.frame(scn=scn, 
      pctg.reach=round(100*sum(fires$aburnt.highintens+fires$aburnt.lowintens>=fires$atarget)/nrow(fires)),
      ab.at=round(100*sum(fires$aburnt.highintens+fires$aburnt.lowintens)/sum(fires$atarget)))
    result <- rbind(result, aux)
    # ggplot(data=filter(fires, rem>0), aes(fires$rem[fires$rem>0])) + geom_histogram(bins = 10)
  }
  return(result[-1,])
}


############ FIRE SHAPES - METRICS ############
fire.shapes <- function(list.scn, years, runs){
  metrics <- data.frame(scn=NA, run=NA, year=NA, shape=NA, para=NA, frac=NA, cai=NA, gyrate=NA)
  for(scn in list.scn){
    for(r in runs){
      for(y in years){
        cat(paste(scn, "r", r, "t", y),"\n")
        FIRES <- raster(paste0(getwd(), "/outputs/", scn, "/lyr/DistType_r", r, "t", y,".tif"))
        FIRES[FIRES[]!=0] <- 1
        FIRES[FIRES[]==0] <- NA
        aux <- data.frame(scn=scn,run=r, year=y,shape=mean(lsm_p_shape(FIRES, directions=8)$value),
                          para=mean(lsm_p_para(FIRES, directions=8)$value),
                          frac=mean(lsm_p_frac(FIRES, directions=8)$value),
                          cai=mean(lsm_p_cai(FIRES, directions=8)$value),
                          gyrate=mean(lsm_p_gyrate(FIRES, directions=8)$value))
        metrics <- rbind(metrics, aux)
      }  
    }
  }
  return(metrics)
}


# execute validation functions
 list.scn <-  #c(paste0("Scn_crazy05_r85_nff", c("01", "02", "03", "04", "05", "06", "07", "08", "09")),
              paste0("Scn_crazy04_r85_nff", c("01", "02", "03", "04", "05", "06", "07", "08", "09"), "_windH") #,
              #paste0("Scn_crazy03_r85_nff", c("01", "02", "03", "04", "05", "06", "07", "08", "09")))
list.scn <- c(paste0("Scn12_crazy", c( "08", "09", "10", "11"), "_r65_nff01_topo"),
              paste0("Scn12_crazy", c( "09",  "11"), "_r65_nff11_topo") )
list.scn <- c(paste0("Scn12_crazy", c( "08", "09", "10", "11"), "_r65_n3_topo"),
              paste0("Scn12_crazy", c( "08", "09", "10", "11"), "_r65_n3_conv"),
              paste0("Scn12_crazy", c( "08", "09", "10", "11"), "_r65_n3_wind"))
extinct <- self.extinguishing(list.scn);extinct
metric <- fire.shapes(list.scn, 1, 1:3)
write.table(metric, "rscripts/outs/metricsCrazy04_r85_nff01-09.txt", row.names=F, quote=F, sep="\t")

metric
resum <- filter(metric, !is.na(scn)) %>% group_by(scn) %>% summarize(shape=round(mean(shape),2), 
          para=round(mean(para*100),2), frac=round(mean(frac),3), cai=round(mean(cai),2),
          gyrate=round(mean(gyrate),2))
resum
a <- group_by(metric, scn, year) %>% summarize(shape=round(mean(shape),2), para=round(mean(para*100),2), 
        frac=round(mean(frac),3),cai=round(mean(cai),2), gyrate=round(mean(gyrate),2))
filter(a, year==1)

# write.table(resum, "d:/MEDMOD/SpatialModelsR/MEDFIRE/rscripts/outs/metrics.txt", row.names=F, quote=F, sep="\t")


############ PERCENTAGE BURNT, SUPPRESS, REMANENT ############
scn <- list.scn[1]
fires <- read.table(paste0("outputs/", scn, "/Fires.txt"), header=T) %>%
         mutate(aburnt=aburnt.highintens+aburnt.lowintens, asupp=asupp.fuel+asupp.sprd)
a <- group_by(fires, year) %>% summarize(at=sum(atarget), ab=sum(aburnt), as=sum(asupp), rem=sum(rem),
                                         pb=round(ab/at*100,1), ps=round(as/at*100,1), pr=round(rem/at*100,1))
a
mean(a$pb); mean(a$ps); mean(a$pr)




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
