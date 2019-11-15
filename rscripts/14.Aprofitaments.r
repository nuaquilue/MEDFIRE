library(tidyverse)
library(raster)
rm(list=ls())

######################## Aprofitaments de fusta i llenya en forest pública de Catalunya ########################
aprof <- read.table("//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/ins/AprofitamentsPublics_2015-2017.txt", header=T)
aprof$Ratio <- aprof$Quantity / aprof$Area
filter(aprof, Product=="FUSTA_EN_PEU", Province=="TARRAGONA")

## Histogrames per producte i variable
# for(province in c("BARCELONA", "LLEIDA", "GIRONA", "TARRAGONA")){
  # aprof.province <- filter(aprof, Province==province)
  aprof.province <- aprof
  # tiff(paste0("//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/outs/Aprofitaments", province, ".tiff"), width=600, height=1000)
  tiff("//serverprocess/Nu/MEDMOD/R.MEDFIRE_II/outs/Aprofitaments.tiff", width=1000, height=1000)
  par(mfrow=c(3,3))
  # APROFITAMENT FUSTA m3
  fusta.peu <- filter(aprof.province, Product=="FUSTA_EN_PEU")
  volum <- fusta.peu$Quantity
  q <- quantile(volum, p=c(0,0.05,0.25,0.50,0.75,0.95,1))
  volum <- volum[volum>=q[2] & volum<=q[6]]
  hist(volum, main="Volum FUSTA EN PEU", xlab="m3" )
  area <- fusta.peu$Area
  q <- quantile(area, p=c(0,0.05,0.25,0.50,0.75,0.95,1))
  area <- area[area>=q[2] & area<=q[6]]
  hist(area, main="Area FUSTA EN PEU", xlab="ha")
  ratio <- fusta.peu$Ratio
  q <- quantile(ratio, p=c(0,0.05,0.25,0.50,0.75,0.95,1))
  ratio <- ratio[ratio>=q[2] & ratio<=q[6]]
  hist(ratio, main="Ratio FUSTA EN PEU", xlab="m3/ha")
  # APROFITAMENT FUSTA PER PRODUCTES t
  fusta.prod <- filter(aprof.province, Product=="FUSTA_PER_PRODUCTES")
  volum <- fusta.prod$Quantity
  q <- quantile(volum, p=c(0,0.05,0.25,0.50,0.75,0.95,1))
  volum <- volum[volum>=q[2] & volum<=q[6]]
  hist(volum,main="Massa FUSTA PER PRODUCTES", xlab="t" )
  area <- fusta.prod$Area
  q <- quantile(area, p=c(0,0.05,0.25,0.50,0.75,0.95,1))
  area <- area[area>=q[2] & area<=q[6]]
  hist(area, main="Area FUSTA PER PRODUCTES", xlab="ha")
  ratio <- fusta.prod$Ratio
  q <- quantile(ratio, p=c(0,0.05,0.25,0.50,0.75,0.95,1))
  ratio <- ratio[ratio>=q[2] & ratio<=q[6]]
  hist(ratio, main="Ratio FUSTA PER PRODUCTES", xlab="t/ha")
  # APROFITAMENT LLENYA t
  llenya <- filter(aprof.province, Product=="LLENYA")
  volum <- llenya$Quantity
  q <- quantile(volum, p=c(0,0.05,0.25,0.50,0.75,0.95,1))
  volum <- volum[volum>=q[2] & volum<=q[6]]
  hist(volum,main="Massa LLENYA", xlab="t" )
  area <- llenya$Area
  q <- quantile(area, p=c(0,0.05,0.25,0.50,0.75,0.95,1))
  area <- area[area>=q[2] & area<=q[6]]
  hist(area, main="Area LLENYA", xlab="ha")
  ratio <- fusta.prod$Ratio
  q <- quantile(ratio, p=c(0,0.05,0.25,0.50,0.75,0.95,1))
  ratio <- ratio[ratio>=q[2] & ratio<=q[6]]
  hist(ratio, main="Ratio LLENYA", xlab="t/ha")
  dev.off()  
# }
  
  
###########################  

  # land.cover, dist.path, slope, and forest type
  LAND0 <-  raster("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputlyrs/asc/LCFspp10_100m.asc")
  BIOM0 <-  raster("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputlyrs/asc/Biomass10_res10_100m.asc")
  AGE0 <- raster("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputlyrs/asc/VegetAge10_100m.asc")
  SQI <- raster("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputlyrs/asc/SQI10_100m.asc")
  DIST.PATH <- raster("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputlyrs/asc/DistPath_100m.asc")
  SLOPE <- raster("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputlyrs/asc/SlopePctg_100m.asc")
  forest.type <- data.frame(spp=1:13, ftype=c(1,1,1,1,1,1,1,2,2,2,2,2,2))
  dta <- data.frame(spp=LAND0[], biom=BIOM0[], age=AGE0[], sqi=SQI[], slope=SLOPE[], dist=DIST.PATH[])
  dta <- left_join(dta, forest.type)
  eq.ba.vol <- read.table("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputfiles/BasalAreaVol.txt", header=T)
  eq.ba.volbark <- read.table("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputfiles/BasalAreaVolBark.txt", header=T)
  eq.ba.carbon <- read.table("//serverprocess/Nu/MEDMOD/SpatialModels/MEDFIRE_II/inputfiles/BasalAreaCarbon.txt", header=T)
  
  
  # bosc
  spp.comer <- 13
  forest <- filter(dta, !is.na(spp) & spp>0 & spp<=spp.comer) %>%
    left_join(eq.ba.vol) %>%  left_join(eq.ba.volbark) %>%  left_join(eq.ba.carbon) %>%
    mutate(vol=c*biom/10+c2*(biom/10)^2, volbark=cbark*biom/10+cbark2*(biom/10)^2, carbon=c1*biom/10) 
  # bosc aprofitable
  for(th.dist in c(200,400,500)){
    forest.aprof <- filter(forest, dist<=th.dist)
    print( round(100* nrow(forest.aprof) / nrow(forest) ) )# PGPF is 75%
  }
  # bosc mecanizable
  forest.aprof.mecan <- filter(forest.aprof, slope<=30)
  nrow(forest.aprof.mecan)/ nrow(forest)  # PGPF is 40%
  
  # bosc per forest type
  table(forest.aprof$ftype)/nrow(forest.aprof)
  table(forest.aprof.mecan$ftype)/nrow(forest.aprof.mecan)
  # volum bosc per forest type
  vol.ftype <- group_by(forest, ftype) %>% summarise(vol=sum(volbark))
  vol.ftype.aprof <- group_by(forest.aprof, ftype) %>% summarise(vol=sum(volbark))
  vol.ftype.aprof.mecan <- group_by(forest.aprof.mecan, ftype) %>% summarise(vol=sum(volbark))
  
  ## 
  vol.ftype.aprof.mecan$vol[1]/sum(  vol.ftype.aprof.mecan$vol)
  
  ## Percentatge volum extret / vol.bosc
  vol.extret <- c(456879, 97787)
  vol.extret <- c(671308, 105496)
  vol.extret/unlist(vol.ftype$vol)*100
  vol.extret/unlist(vol.ftype.aprof$vol)*100
  vol.extret/unlist(vol.ftype.aprof.mecan$vol)*100
  
  (vol.extret[2]/vol.ftype.aprof.mecan$vol[2])  /  (vol.extret[1]/vol.ftype.aprof.mecan$vol[1])
  
  
  ## Example to understand the ratio of (extract.decid/exist.decid) / (extract.conif/exist.conif)
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
  