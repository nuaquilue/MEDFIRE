scn <- "CC"
aff <- read.table(paste0("C:/WORK/MEDMOD/SpatialModelsR/MEDFIRE/outputs/Scn_",scn, "/Afforestation.txt"), header=T)
land <- read.table(paste0("C:/WORK/MEDMOD/SpatialModelsR/MEDFIRE/outputs/Scn_", scn,"/Land.txt"), header=T)
forest <- land %>% filter(spp<=13, year>0) %>% group_by(year, run) %>% summarise(area=sum(area))
shrub <- land %>% filter(spp==14, year>0) %>% group_by(year, run) %>% summarise(area=sum(area)) 
a <- group_by(aff, year, run) %>% summarise(ha=sum(ha)) 
mean(a$ha)
mean(a$ha/forest$area*100)
mean(a$ha/shrub$area*100)


library(foreign)
usos <- read.dbf("C:/WORK/TEACHING/Joel/DataIn/Usos sol Catalunya  1956 IFN/Export_Output.dbf")
table(usos$Usos_56_Si)
round(100*table(usos$Usos_56_Si)/nrow(usos))


library(raster)
library(tidyverse)
class <- read.table("C:/WORK/CARTO-DATA/UsosCobertes/Tesaurus/reclass_covers.txt", header=T)
USOS87 <- raster("C:/WORK/CARTO-DATA/UsosCobertes/RastersCoherents_Multibanda/UsSol_1987_100m.asc")
USOS17 <- raster("C:/WORK/CARTO-DATA/UsosCobertes/RastersCoherents_Multibanda/UsSol_2017_100m.asc")
# USOS17 <- raster("C:/WORK/CARTO-DATA/UsosCobertes/RastersCoherents_Multibanda/UsSol_2012_v2.asc")
dta <- data.frame(cell.id=1:ncell(USOS87), usos87=USOS87[], usos17=USOS17[])
dta <- filter(dta, !is.na(usos87)) %>% filter(!is.na(usos17))
dta.cover <- dta %>% left_join(class, by=c("usos87"="codi")) %>% mutate(coberta87=coberta) %>% select(-coberta, -usos87)  %>% 
  left_join(class, by=c("usos17"="codi")) %>% mutate(coberta17=coberta) %>% select(-coberta, -usos17) %>% 
  mutate(chg=ifelse(coberta17==3 & coberta87==4, 1, ifelse(coberta17==4 & coberta87==4, 0, NA)))
count87 <- table(dta.cover$coberta87)
count17 <- table(dta.cover$coberta17)
count17-count87
table(dta.cover$chg)
table(dta.cover$coberta87, dta.cover$coberta17)

load("inputlyrs/rdata/orography.rdata")
load("inputlyrs/rdata/land.rdata")
load("inputlyrs/rdata/coordinates.rdata")
clim.mdl <- "SMHI-RCA4_MOHC-HadGEM2-ES"
load(paste0("inputlyrs/rdata/clim_hist_", clim.mdl, ".rdata"))
clim <- select(clim, cell.id, temp, precip)

dta.aff <- dta.cover %>% filter(!is.na(chg)) %>% left_join(orography, by="cell.id") %>% 
  left_join(clim, by="cell.id") %>% filter(!is.na(elev))


############ do sth like:
## Coordinates of killed cells and their closest neighbours (do not count for the cell itself)
shrub.coord <- filter(land, tsdist>=20, spp==14) %>% select(cell.id) 
shrub.coord <- data.frame(cell.id=shrub.coord[sample(1:nrow(shrub.coord), 10000, replace=F),1])
shrub.coord <- left_join(shrub.coord, coord, by = "cell.id")
neigh.id <- nn2(coord[,-1], shrub.coord[,-1],  searchtype="priority", k=nneigh[colon.rad]) 
neigh.id <- neigh.id$nn.idx  # dim 10.000 x 61

## Count number of forest mature neigbhours within their climatic range
neigh.spp <- matrix(land$spp[neigh.id[,-1]], nrow=nrow(neigh.id), ncol=ncol(neigh.id)-1) 
for(i in 1:13){
  neigh.sdm <- matrix(sdm[neigh.id[,-1],i+1], nrow=nrow(neigh.id), ncol=ncol(neigh.id)-1) 
  neigh.spp[neigh.spp==i] <- neigh.spp[neigh.spp==i] * neigh.sdm[neigh.spp==i]
}
neigh.spp <- neigh.spp * matrix(land$age[neigh.id[,-1]]>=30, nrow=nrow(neigh.id), ncol=ncol(neigh.id)-1) 
rm(neigh.sdm); gc()



