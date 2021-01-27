rm(list=ls())

suppressPackageStartupMessages({
  library(sp)
  library(raster)  
  library(RANN)  # for nn2()
  library(tidyverse)
})
source("mdl/land.cover.change.r")

# Name scn and output folder
scn.name <- "TestLChg"
out.path <- paste0("outputs/", scn.name)
dir.create(file.path(getwd(), out.path), showWarnings = F) 
dir.create(file.path(getwd(), out.path, "/lyr"), showWarnings = F) 

## Input data from land.dyn.mdl.r
load("inputlyrs/rdata/mask.rdata")
load("inputlyrs/rdata/land.rdata")
load("inputlyrs/rdata/coordinates.rdata")
load("inputlyrs/rdata/interface.rdata")

# Input data
lchg.id <- 2
t <- 1
lc.trans <- 1
file.dmnd.lchg <- "DemandLChg"
file.pattern.lchg  <- "PatternLChg"
# urban
visit.cells <- numeric()
chg.cells <- land.cover.change(land, coord, interface, lc.trans=1, t, visit.cells)
land$distype[land$cell.id %in% chg.cells] <- 1
# agri.conv
lc.trans <- 2
visit.cells <- chg.cells
chg.cells <- land.cover.change(land, coord, interface, lc.trans=2, t, visit.cells)
land$distype[land$cell.id %in% chg.cells] <- 2
# raband
lc.trans <- 3
visit.cells <- c(visit.cells, chg.cells)
chg.cells <- land.cover.change(land, coord, interface, 3, t, visit.cells)
land$distype[land$cell.id %in% chg.cells] <- 3
# Track land-cover chg
visit.cells <- c(visit.cells, chg.cells)
land$tsdist[land$cell.id %in% visit.cells] <- 0
MAP <- MASK
MAP[!is.na(MASK[])] <- land$distype*(land$tsdist==0)
writeRaster(MAP, paste0(out.path, "/lyr/DistType_t", t, ".tif"), format="GTiff", overwrite=T, NAflag=-1)
writeRaster(MAP, paste0(out.path, "/lyr/DistType_t", t, ".asc"), format="ascii", overwrite=T, NAflag=-1)


## 
land <- read.table("C:/WORK/MEDMOD/SpatialModelsR/MEDFIRE/outputs/landcover/Land.txt", header=T)
