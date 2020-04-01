# /*********
#   Compute Neighborhood factor for each Land Cover Type following Verburg et al. 2004
# 
# F.i.k.d = (n.k.d.i / n.d.i) / (N.k / N)
# 
# F.i.k.d characterizes the enrichment of neighbourhood d of location i with land use type k. 
# n.k.d.i is the number of cells of land use type k in the neighbourhood d of cell i
# n.d.i the total number of cells in the neighbourhood 
# N.k is the number of cells with land use type k in the whole raster  
# N all cells in the raster
# 
# Two alternatives: squared neighbourhood versus a circle-based
# 
# OVER REGION CENTRED(0, rad, EUCLIDEAN)  --> includes center cell
# OVER REGION CENTRED(1, rad)  --> doesn't inclued it
# 	OVER REGION CENTRED(0, rad, CARDINAL)  --> the distance between two cells is the minimum number
# 									              of up - down - left - right steps
# 	OVER REGION RECT(bottomlim, leftlim, toplim, rightlim)  --> it includes central cell
# 			// the 4 arguments define the limits of the rectangular region
# 
# *********/


neigh.factor <- function(land, interface){
  
  cat("Deriving neigh factor for urban, agricultural land and natural land")
  
  ## Land-cover types are 1 - urban, 2 - crop+arableland, 3 - forest+shurb+grass, 4 - water+bare
  thesaurus <- data.frame(spp=1:20, x=c(rep(3,16),2,2,4,1))
  land.cover <- left_join(land, thesaurus, by="spp")
  
  ## Nk
  Nk.cover <- group_by(land.cover, x) %>% summarize(n=length(x))
  Nk.inter <- group_by(interface, x) %>% summarize(n=length(x))
  
  ##
  neighs <- nn2(select(coord, x, y), coord[123456, c(2:3)], searchtype="priority", k=705)
  neigh.id <- neighs$nn.idx
  neigh.dist <- neighs$nn.dists
  max(neigh.dist)
  sum(neigh.dist==1500)
  neigh.id0 <- neigh.id-123456
  hist(neigh.id0)
  hist(neigh.dist)
  
  m <- matrix(neigh.id0[-1], nrow=nrow(coord), ncol=704, byrow=T) 
  m <- m + matrix(coord$cell.id, nrow=nrow(coord), ncol=704, byrow=F)
  
  
}

work.path <- "c:/work/MEDMOD/spatialmodelsr/Medfire"
load("inputlyrs/rdata/mask.rdata")
load("inputlyrs/rdata/land.rdata")

URB <- MASK
URB[!is.na(URB[])] <- land$spp
URB[URB[]!=20] <- NA
plot(URB)
sum(is.na(URB[]))
# group_by(land, spp) %>% summarize(x=length(spp))
crs(URB) <- CRS("+init=epsg:25831")

system.time(DIST.URB <- distance(URB))
plot(DIST.URB)  
