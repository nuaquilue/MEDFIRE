## Observed
a <- read.table("D:/MEDMOD/FutureBioEcon/HarvestingStats/ProduccióComarcal/AprofitamentAnnual_2002-2016.txt", header = T)
a$pconif <- round(100*a$Conifer/(a$Conifer+a$Deciduous))
a$pdecid <- round(100*a$Deciduous/(a$Conifer+a$Deciduous))
a
mean(a$Conifer); mean(a$Deciduous)
mean(a$Conifer + a$Deciduous)
mean(a$Wood)


## Existences
## Compute the volume that can be extracted in locations where harvesting is sustainable
volume <- select(sustain, cell.id, spp, biom, pctgextract) %>% 
  left_join(eq.ba.vol, by="spp") %>% 
  mutate(vol=cx*biom/10+cx2*biom*biom/100) %>% select(-cx, -cx2) %>% 
  mutate(vol.extract = vol*pctgextract/100) %>% 
  mutate(vol.fake=ifelse(spp<=7, vol.extract*5, vol.extract))
allvol.extract <- sum(volume$vol.extract)
allvol.fake <- sum(volume$vol.fake)
volspp <- group_by(volume, spp) %>% summarise(vol.extract=sum(vol.extract), vol.fake=sum(vol.fake)) %>% 
  mutate(pext=vol.extract/allvol.extract, pfake=vol.fake/allvol.fake)
round(100*sum(volspp$pext[volspp$spp<=7]),1); round(100*sum(volspp$pext[volspp$spp>7]),1)
round(100*sum(volspp$pfake[volspp$spp<=7]),1); round(100*sum(volspp$pfake[volspp$spp>7]),1)


## com ha anat això??
a <- filter(cut.out, !is.na(cut.id)) %>% group_by(cut.id) %>% 
  summarise(vol=sum(vol.sawlog+vol.wood), vol.sawlog=sum(vol.sawlog), vol.wood=sum(vol.wood))
sum(a$vol); sum(a$vol.sawlog); sum(a$vol.wood)
b <- group_by(cut.out, spp) %>% summarise(vol.sawlog=sum(vol.sawlog), vol.wood=sum(vol.wood))
# Vol sawlog
sum(b$vol.sawlog[b$spp<=7]); round(100*sum(b$vol.sawlog[b$spp<=7])/sum(b$vol.sawlog))
sum(b$vol.sawlog[b$spp>7]); round(100*sum(b$vol.sawlog[b$spp>7])/sum(b$vol.sawlog))
# Vol wood
sum(b$vol.wood[b$spp<=7]); round(100*sum(b$vol.wood[b$spp<=7])/sum(b$vol.wood))
sum(b$vol.wood[b$spp>7 & b$spp<=11]); round(100*sum(b$vol.wood[b$spp>7 & b$spp<=11])/sum(b$vol.wood))
sum(b$vol.wood[b$spp>=12]); round(100*sum(b$vol.wood[b$spp>=12])/sum(b$vol.wood))
# Vol sawlog conif / vol all conif
conif <- filter(b, spp<=7);
sum(conif$vol.sawlog); round(100*sum(conif$vol.sawlog)/(sum(conif$vol.sawlog + conif$vol.wood)))
sum(conif$vol.wood); round(100*sum(conif$vol.wood)/(sum(conif$vol.sawlog + conif$vol.wood)))
# Vol sawlog conif / vol all conif
decid <- filter(b, spp>7);
sum(decid$vol.sawlog); round(100*sum(decid$vol.sawlog)/(sum(decid$vol.sawlog + decid$vol.wood)))
sum(decid$vol.wood); round(100*sum(decid$vol.wood)/(sum(decid$vol.sawlog + decid$vol.wood)))
