library(sp)
library(raster)
load("inputlyrs/rdata/mask.rdata")
load("inputlyrs/rdata/orography.rdata")

# track.spread <- data.frame(cell.id=igni.id, fire.step, spp=land$spp[land$cell.id==igni.id],
#                            front.slope=0.5, front.wind=0.5, flam=0.5, fi=0.5, sr=1, 
#                            pb.sr=1, pb.fi=1, burning.sr=1, burning.fi=1)

dta <- data.frame(cell.id=1:ncell(MASK), mask=MASK[]) %>% 
       left_join(track.spread, by="cell.id") %>% left_join(orography, by="cell.id")

STEP <- MASK; STEP[] <- dta$step
writeRaster(STEP, "D:/MEDMOD/SpatialModelsR/MEDFIRE/rscripts/outs/FireStep.asc", 
            format="ascii", overwrite=T, NAflag=-1)
SPP <- MASK; SPP[] <- dta$spp
writeRaster(SPP, "D:/MEDMOD/SpatialModelsR/MEDFIRE/rscripts/outs/SpeciesBurnt.asc", 
            format="ascii", overwrite=T, NAflag=-1)
SLOPE <- MASK; SLOPE[] <- dta$front.slope
writeRaster(SLOPE, "D:/MEDMOD/SpatialModelsR/MEDFIRE/rscripts/outs/FireSlope.asc", 
            format="ascii", overwrite=T, NAflag=-1)
WIND <- MASK; WIND[] <- dta$front.wind
writeRaster(WIND, "D:/MEDMOD/SpatialModelsR/MEDFIRE/rscripts/outs/FireWind.asc", 
            format="ascii", overwrite=T, NAflag=-1)
INTENS <- MASK; INTENS[] <- dta$fi
writeRaster(INTENS, "D:/MEDMOD/SpatialModelsR/MEDFIRE/rscripts/outs/FireIntens.asc", 
            format="ascii", overwrite=T, NAflag=-1)
RATE <- MASK; RATE[] <- dta$sr
writeRaster(RATE, "D:/MEDMOD/SpatialModelsR/MEDFIRE/rscripts/outs/SpreadRate.asc", 
            format="ascii", overwrite=T, NAflag=-1)
PB <- MASK; PB[] <- dta$pb.fi
writeRaster(PB, "D:/MEDMOD/SpatialModelsR/MEDFIRE/rscripts/outs/ProbBurn.asc", 
            format="ascii", overwrite=T, NAflag=-1)
BURN <- MASK; BURN[] <- dta$burning.fi
writeRaster(BURN, "D:/MEDMOD/SpatialModelsR/MEDFIRE/rscripts/outs/Burning.asc", 
            format="ascii", overwrite=T, NAflag=-1)
SLOPE <- MASK; SLOPE[] <- dta$slope
writeRaster(SLOPE, "D:/MEDMOD/SpatialModelsR/MEDFIRE/rscripts/outs/Slope.asc", 
            format="ascii", overwrite=T, NAflag=-1)
ELEV <- MASK; ELEV[] <- dta$elev
writeRaster(ELEV, "D:/MEDMOD/SpatialModelsR/MEDFIRE/rscripts/outs/Elev.asc", 
            format="ascii", overwrite=T, NAflag=-1)


