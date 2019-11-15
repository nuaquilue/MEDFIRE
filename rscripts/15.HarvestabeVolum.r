library(raster)
library(tidyverse)
rm(list=ls())


############################ land.cover, dist.path, slope, and forest type
LAND0 <-  raster("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputlyrs/asc/LCFspp10_100m.asc")
BIOM0 <-  raster("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputlyrs/asc/Biomass10_res10_100m.asc")
AGE0 <- raster("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputlyrs/asc/VegetAge10_100m.asc")
SQI <- raster("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputlyrs/asc/SQI10_100m.asc")
DIST.PATH <- raster("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputlyrs/asc/DistPath_100m.asc")
SLOPE <- raster("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputlyrs/asc/SlopePctg_100m.asc")
COUNTY <- raster("//Serverprocess/nu/MEDMOD/SpatialModels/MEDFIRE_II/inputlyrs/asc/COUNTIES_100m.asc")
## Limits of counties
LIMITS <- shapefile("//SERVERPROCESS/Nu/MEDMOD/InputLAYERS_MEDFIRELUC/AdminRegion/Comarques.shp")
## Species
forest.type <- data.frame(spp=1:13, ftype=c(1,1,1,1,1,1,1,2,2,2,2,2,2))
species <- data.frame(id=1:14, spp=c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster", "puncianta", "aalba", "qilex", "qsuber", "qfaginea", "qhumilis", "fsylvatica", "other", "shrub"),
                      group=c("conif", "conif", "conif", "conif", "conif", "conif", "conif", "decid", "decid", "decid", "decid", "decid", "decid", "shrub"))
## Management rules
rules <- read.table("//SERVERPROCESS/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputfiles/MgmtRules.txt", header=T)
rules <- rules[,2:4]
names(rules) <- c("spp", "sqi", "year.cut")

dta <- data.frame(spp=LAND0[], biom=BIOM0[], age=AGE0[], sqi=SQI[], slope=SLOPE[], dist=DIST.PATH[], county=COUNTY[])
dta <- left_join(dta, forest.type)
eq.ba.vol <- read.table("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputfiles/BasalAreaVol.txt", header=T)
eq.ba.volbark <- read.table("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputfiles/BasalAreaVolBark.txt", header=T)
eq.ba.carbon <- read.table("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputfiles/BasalAreaCarbon.txt", header=T)
  
  
# bosc
spp.comer <- 13
forest <- filter(dta, !is.na(spp) & spp>0 & spp<=spp.comer) %>%
          left_join(eq.ba.vol) %>%  left_join(eq.ba.volbark) %>%  left_join(eq.ba.carbon) %>%
          mutate(vol=c*biom/10+c2*(biom/10)^2, volbark=cbark*biom/10+cbark2*(biom/10)^2, carbon=c1*biom/10) %>%
          select(-c, -c2, -c1, -cbark, -cbark2) %>%
          left_join(rules)
voltot <- group_by(forest, spp) %>% summarise(volbark=sum(volbark))

# bosc aprofitable
for(th.dist in c(200,400,500)){
  forest.aprof <- filter(forest, dist<=th.dist)
  print( round(100* nrow(forest.aprof) / nrow(forest) ) )# PGPF is 75%
}
vol.acces <- group_by(forest.aprof, spp) %>% summarise(vol=sum(volbark))

# bosc mecanizable
forest.mecan <- filter(forest.aprof, slope<=30)
nrow(forest.mecan)/ nrow(forest)  # PGPF is 40%
vol.mecan <- group_by(forest.mecan, spp) %>% summarise(vol=sum(volbark))  
  
# bosc extraible 
forest.extract <- filter(forest.aprof.mecan, age>=year.cut)
vol.extract <- group_by(forest.extract, spp) %>% summarise(vol=sum(volbark))  
  
# Percentatges
voltot$pctg.acces <- vol.acces$vol/voltot$volbark*100  
voltot$pctg.mecan <- vol.mecan$vol/voltot$volbark*100
voltot <- left_join(voltot, vol.extract) %>% mutate(pctg.extract = vol/volbark*100) %>% select(-vol) 
names(voltot)[1] <- "id"
voltot <- left_join(voltot, species)

write.table(voltot, "//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/outs/PctgCurrentVolExtract.txt", quote=F)













## Example to understand the ratio of (extract.decid/exist.decid) / (extract.conif/exist.conif)
  ## Percentatge volum extret / vol.bosc
  vol.extret <- c(456879, 97787)
  vol.extret <- c(671308, 105496)
  vol.extret/unlist(vol.ftype$vol)*100
  vol.extret/unlist(vol.ftype.aprof$vol)*100
  vol.extret/unlist(vol.ftype.aprof.mecan$vol)*100
  (vol.extret[2]/vol.ftype.aprof.mecan$vol[2])  /  (vol.extret[1]/vol.ftype.aprof.mecan$vol[1])
  y <- 671308 + 105496
  y <- 456879 + 97787
  r.new <- 0.33
  r.old <- 0.53
  xc <-  vol.ftype.aprof.mecan$vol[1]
  xd <-  vol.ftype.aprof.mecan$vol[2]
  yc <- y/(1+r.new*xd/xc)
  yd <- y-yc
  yc;yd
  
  
  ## Decay function (negative exponential) for forest accessible to be harvesed
  x <- seq(0,500,1)
  lambda <- 1/200
  plot(x, exp(-lambda*x), type="l")
  x <- 200;  y <- exp(-lambda*x); y
  x <- 400;  y <- exp(-lambda*x); y
  x <- 500;  y <- exp(-lambda*x); y
  
  
  ## Current volume per spp
  ## 1. harvestable
  ## 2. harvestable & accessible 
  ## 3. harvestable, accessible, & mecanizable
  