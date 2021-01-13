library(sf)
library(rgdal)
library(RSQLite)
library(tidyverse)

# Explore the layers available 
ogrListLayers("c:/work/carto-data/cobertes-sol-v1r0-2018.gpkg")
package <- src_sqlite("c:/work/carto-data/cobertes-sol-v1r0-2018.gpkg") 
dta <- as.data.frame(tbl(package, "cobertes_sol_categories"));dta
dta$lcfm <- c(16,17,17,17,17,16,1,1,1,14,1,1,1,15,1,1,1,18,18,19,20,20,20,
              20,20,20,20,20,20,20,20,20,20,20,20,19,19,19,19,19,19)



library(raster)
mcsc <- raster("c:/work/carto-data/MCSC18.tif")
plot(mcsc, col=rainbow(41))
table(mcsc[])

mcsc_7cat <- reclassify(mcsc, dta[,c(3,5)])
abund <- table(mcsc_7cat[])
100*abund/sum(abund)
round(100*abund/sum(abund),1)
plot(mcsc_7cat)
