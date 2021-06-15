source("mdl/suit.mgmt.r")
cut.out <- suit.mgmt(land, harvest, clim, t)
sustain <- cut.out$sustain
extracted.sawlog <- cut.out$extracted.sawlog
extracted.wood <- cut.out$extracted.wood
aux <- rbind(group_by(extracted.sawlog,spp) %>% summarize(vol.sawlog=sum(vol.extract.sawlog), vol.wood=sum(vol.extract.wood)),
             group_by(extracted.wood,spp) %>% summarize(vol.sawlog=0, vol.wood=sum(vol.extract.sawlog+vol.extract.wood)))
a <- group_by(aux, spp) %>% summarize(vol.sawlog=round(sum(vol.sawlog),1), vol.wood=round(sum(vol.wood),1))         ;a


# pctg forest type harvested for sawlog:  79% conif - 21% decid (obs); 84.5 - 15.5 (sim)
round(100*sum(a$vol.sawlog[a$spp<=7])/sum(a$vol.sawlog),1)
round(100*sum(a$vol.sawlog[a$spp>7])/sum(a$vol.sawlog),1)
# pctg forest type harvested for wood:  20% conif - 80% decid (obs); 19.4 - 80.6 (sim)
round(100*sum(a$vol.wood[a$spp<=7])/sum(a$vol.wood),1)
round(100*sum(a$vol.wood[a$spp>7])/sum(a$vol.wood),1)
# pctg conif to each product: 92% sawlog - 8% wood (obs); 92.3 - 7.7 (sim)
round(100*sum(a$vol.sawlog[a$spp<=7])/sum(a$vol.sawlog[a$spp<=7]+a$vol.wood[a$spp<=7]),1)
round(100*sum(a$vol.wood[a$spp<=7])/sum(a$vol.sawlog[a$spp<=7]+a$vol.wood[a$spp<=7]),1)
# pctg decid to each product: 49% sawlog - 51% wood (obs); 34.6 - 65.4 (sim)
round(100*sum(a$vol.sawlog[a$spp>7])/sum(a$vol.sawlog[a$spp>7]+a$vol.wood[a$spp>7]),1)
round(100*sum(a$vol.wood[a$spp>7])/sum(a$vol.sawlog[a$spp>7]+a$vol.wood[a$spp>7]),1)


## Map
load("inputlyrs/rdata/mask.rdata")
scn.name <- "harvesting10"
load(paste0("outputs/", scn.name, "/rdata/land_r1t6.rdata"))
## Find patches using a 8-neigbour rule, to have an idea of the size of patches that can be harvested
MASK[!is.na(MASK[])] <- ifelse(!is.na(land$tscut) & land$tscut==1,1,0) * ifelse(land$spp<=7,1,2)
table(MASK[])
tiff(paste0("outputs/", scn.name, "/interventions_r1t1.tiff"))
plot(MASK, col=c("grey90", "black","red"), legend=F); dev.off()

load("inputlyrs/rdata/mask.rdata")
MASK[!is.na(MASK[])] <- ifelse(land$tscut==1,1,0)
CLUSTER <- clump(MASK)
plot(CLUSTER, col="black", legend=F) #plasma(max(CLUSTER[], na.rm=T)))
## Build a data frame with cell coordinates and cluster id
df <- data.frame(cell.id=1:ncell(CLUSTER), coordinates(CLUSTER), clust=getValues(CLUSTER))
ptch <- filter(df, !is.na(clust)) %>% group_by(clust) %>% summarise(xm=mean(x), ym=mean(y), size=length(clust))
head(ptch)



################ Early june 2021 ################
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
###################################################################

## Observed
a <- read.table("D:/MEDMOD/FutureBioEcon/HarvestingStats/ProduccióComarcal/AprofitamentAnnual_2002-2016.txt", header = T)
a$pconif <- round(100*a$Conifer/(a$Conifer+a$Deciduous))
a$pdecid <- round(100*a$Deciduous/(a$Conifer+a$Deciduous))
a
mean(a$Conifer); mean(a$Deciduous)
mean(a$Conifer + a$Deciduous)
mean(a$Wood)
