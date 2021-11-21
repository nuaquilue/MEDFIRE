scn <- "CC"
aff <- read.table(paste0("outputs/Scn_",scn, "/Afforestation.txt"), header=T)
land <- read.table(paste0("outputs/Scn_", scn,"/Land.txt"), header=T)
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
class <- read.table("C:/WORK/onedrive - ctfc.cat/CARTO-DATA/UsosCobertes/Tesaurus/reclass_covers.txt", header=T)
USOS87 <- raster("C:/WORK/onedrive - ctfc.cat/CARTO-DATA/UsosCobertes/RastersCoherents_Multibanda/UsSol_1987_100m.asc")
USOS17 <- raster("C:/WORK/onedrive - ctfc.cat/CARTO-DATA/UsosCobertes/RastersCoherents_Multibanda/UsSol_2017_100m.asc")
dta <- data.frame(cell.id=1:ncell(USOS87), usos87=USOS87[], usos17=USOS17[])
dta <- filter(dta, !is.na(usos87)) %>% filter(!is.na(usos17))
dta.cover <- dta %>% left_join(class, by=c("usos87"="codi")) %>% mutate(coberta87=coberta) %>% select(-coberta, -usos87)  %>% 
  left_join(class, by=c("usos17"="codi")) %>% mutate(coberta17=coberta) %>% select(-coberta, -usos17) %>% 
  mutate(chg=ifelse(coberta17==3 & coberta87==4, 1, ifelse(coberta17==4 & coberta87==4, 0, NA)))
save(dta.cover, file="inputlyrs/rdata/dta.cover.rdata")
count87 <- table(dta.cover$coberta87)
count17 <- table(dta.cover$coberta17)
count17-count87
table(dta.cover$chg)
table(dta.cover$coberta87, dta.cover$coberta17)
rm(USOS87); rm(USOS17); gc()

# data
load("inputlyrs/rdata/dta.cover.rdata")
load("inputlyrs/rdata/fire.recurrence_87-17.rdata")
load("inputlyrs/rdata/orography.rdata")
load("inputlyrs/rdata/land.rdata")

# climate standarized as in medfire 
clim.mdl <- "SMHI-RCA4_MOHC-HadGEM2-ES"
load(paste0("inputlyrs/rdata/clim_hist_", clim.mdl, ".rdata"))
load(paste0("inputlyrs/rdata/sdm_base_hist_", clim.mdl, ".rdata"))

# old forest
load("inputlyrs/rdata/utm.rdata")
## Join utm and sdm info to land
land.utm <- land %>% select(cell.id, spp, biom, age) %>% 
  left_join(select(clim, cell.id, sdm), by="cell.id") %>% 
  left_join(utm, by="cell.id") 
## Calculate the percentage of old forest within its climatic niche per utm cell
utm.forest <- group_by(land.utm, utm) %>% summarise(nneigh=length(utm), old.neigh=sum(spp<=13 & age>=15 & sdm==1)) %>% 
  mutate(pct=old.neigh/nneigh)
old.forest <- land.utm %>% select(cell.id, utm) %>% left_join(select(utm.forest, utm, pct), by="utm") %>% 
  select(-utm)



# all variables
dta.aff <- dta.cover %>% filter(!is.na(chg)) %>% left_join(recurrent, by="cell.id") %>% 
  filter(times.burnt==0) %>% left_join(orography, by="cell.id") %>% 
  left_join(clim, by="cell.id") %>% left_join(old.forest, by="cell.id") %>% filter(!is.na(elev)) 
table(dta.aff$chg)
summary(dta.aff)
rm(clim); rm(sdm); rm(old.forest); rm(recurrent); rm(dta.cover); 
rm(land); rm(land.utm); rm(orography); rm(utm); rm(utm.forest); gc()


# explanatory variables: histogram 
ggplot(dta.aff, aes(x=elev)) + 
  geom_histogram(binwidth=100, color="black", fill="white") +facet_grid(.~chg)
ggplot(dta.aff, aes(x=slope)) + 
  geom_histogram(binwidth=1, color="black", fill="white") +facet_grid(.~chg)
ggplot(dta.aff, aes(x=tempmin)) + 
  geom_histogram(binwidth=1, color="black", fill="white") +facet_grid(.~chg)
ggplot(dta.aff, aes(x=tempmax)) + 
  geom_histogram(binwidth=1, color="black", fill="white") +facet_grid(.~chg)
ggplot(dta.aff, aes(x=precip)) + 
  geom_histogram(binwidth=100, color="black", fill="white") +facet_grid(.~chg)
ggplot(dta.aff, aes(x=pct)) + 
  geom_histogram(binwidth=0.1, color="black", fill="white") +facet_grid(.~chg)

# explanatory variables: correlation matix
res <- cor(select(dta.aff, elev, slope, tmin, tmax, precip, pct))
round(res, 2)

# logistic regression
dta.aff$rand <- runif(nrow(dta.aff),0,1)
dta.fit <- filter(dta.aff, rand<=0.4)
# all variables, linear terms
mylogit <- glm(chg ~ elev + slope + precip + tmin + tmax + pct, data = dta.fit, family = "binomial")
summary(mylogit)
# all variables with quadratic terms
full.model <- glm(chg ~ elev + slope + precip + pct + tmin + tmax +
                    I(elev^2) + I(slope^2) + I(precip^2) + I(pct^2) +I(tmin^2) + I(tmax^2) , 
                  data = dta.fit, family = "binomial",  na.action = "na.fail")  
summary(full.model)
# all variables with interactions
inter.model <- glm(chg ~ elev + slope + precip + pct + tmin + # tmax + 
                      elev*pct +  slope*pct + precip*pct + tmin*pct, # + tmax*pct, 
                   data = dta.fit, family = "binomial")
summary(inter.model)
# 
pupurri.model <- glm(chg ~ elev + slope + precip + pct + 
                        I(elev^2) + I(slope^2) + I(precip^2) + I(pct^2) +
                       elev*slope + elev*pct + slope*precip + slope*pct + precip*pct, 
                     data = dta.fit, family = "binomial")
summary(pupurri.model)
 

library(BMA)
# si poso tmax, la precip no surt significativa
# prefereixo tenir una temp i una precip, que no les 2 temperatures
# amb elev, slope, precip, tmin, pct i les interaccions pct amb la resta (4), 
# totes les variables surten significatives en el millor model
output <- bic.glm(chg ~ elev + slope + precip + tmin + pct +  # tmax*pct +  tmax +
                     elev*pct + slope*pct + precip*pct + tmin*pct,  #elev*slope +  slope*precip 
                  data = dta.fit, glm.family = "binomial", maxCol=14)
summary(output)
                    
                    
# variable selection with step AIC 
step.model <- pupurri.model %>% MASS::stepAIC(trace = FALSE)
summary(step.model)
# variable selection with dredge.
results.affor <- MuMIn::dredge(full.model)
subset(results.affor , delta <10)
subset(results.affor, delta ==0) #millor model
  #   Model selection table 
  # (Intrc)      elev     elev^2    pct pct^2    precp    precp^2   slope   slope^2 df    logLik     AICc delta weight
  # 256   -1.33 0.0005573 -7.392e-07 0.6058 2.316 0.2619 -0.01144 0.0149 0.0001734  9 -229774.9 459567.7     0      1
  # Models ranked by AICc(x)
MuMIn::importance(results.affor)
  # elev I(elev^2) pct I(pct^2) precip I(precip^2) slope I(slope^2)
  # Sum of weights:        1    1         1   1        1      1           1     1       
  # N containing models: 128  128       128 128      128    128         128   128   



