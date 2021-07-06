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
class <- read.table("C:/WORK/CARTO-DATA/UsosCobertes/Tesaurus/reclass_covers.txt", header=T)
USOS87 <- raster("C:/WORK/CARTO-DATA/UsosCobertes/RastersCoherents_Multibanda/UsSol_1987_100m.asc")
USOS17 <- raster("C:/WORK/CARTO-DATA/UsosCobertes/RastersCoherents_Multibanda/UsSol_2017_100m.asc")
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

load("inputlyrs/rdata/dta.cover.rdata")
load("inputlyrs/rdata/fire.recurrence_87-17.rdata")
load("inputlyrs/rdata/orography.rdata")
load("inputlyrs/rdata/land.rdata")
load("inputlyrs/rdata/oldforest.rdata")
# climate standarized as in medfire
clim.mdl <- "SMHI-RCA4_MOHC-HadGEM2-ES"
load(paste0("inputlyrs/rdata/climate_hist_", clim.mdl, ".rdata"))

# all variables
dta.aff <- dta.cover %>% filter(!is.na(chg)) %>% left_join(recurrent, by="cell.id") %>% 
  filter(times.burnt==0) %>% left_join(orography, by="cell.id") %>% 
  left_join(clim, by="cell.id") %>% left_join(oldforest, by="cell.id") %>% filter(!is.na(elev)) 
table(dta.aff$chg)
summary(dta.aff)

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
res <- cor(select(dta.aff, elev, slope, temp, precip, pct))
round(res, 2)

# logistic regression
dta.aff$rand <- runif(nrow(dta.aff),0,1)
dta.fit <- filter(dta.aff, rand<=0.4)
# all variables, linear terms
mylogit <- glm(chg ~ elev + slope + precip + temp + pct, data = dta.fit, family = "binomial")
summary(mylogit)
# all variables with quadratic terms
full.model <- glm(chg ~ elev + slope + precip + pct + 
                    I(elev^2) + I(slope^2) + I(precip^2) + I(pct^2), 
                  data = dta.fit, family = "binomial",  na.action = "na.fail")  
summary(full.model)

# variable selection with step AIC 
step.model <- full.model %>% MASS::stepAIC(trace = FALSE)
summary(step.model)
# variable selection with dredge.
results.affor <- MuMIn::dredge(full.model)
subset(results.affor , delta <10)
subset(results.affor, delta ==0) #millor model
  #   Model selection table 
  # (Intrc)      elev     elev^2    pct pct^2    precp    precp^2   slope   slope^2 df    logLik     AICc delta weight
  # 256  -2.278 0.0005294 -7.225e-07 0.7493 2.182 0.001565 -2.538e-07 0.01614 0.0001455  9 -229946.6 459911.2     0      1
  # Models ranked by AICc(x)
MuMIn::importance(results.affor)
  # elev I(elev^2) pct I(pct^2) precip I(precip^2) slope I(slope^2)
  # Sum of weights:        1    1         1   1        1      1           1     1       
  # N containing models: 128  128       128 128      128    128         128   128   



