######################################################################################
##
######################################################################################

drought <- function(land, clim, decade, t){
  
  ## Tracking
  cat("Drought", "\n") 
  
  ## Count how many ha to kill, totally and this time step
  ## Those forest species out of its climatic range and not burnt the current time step
  to.kill <- left_join(land, select(clim, -spp), by="cell.id") %>% filter(spp<=13, sdm==0, tsdist>0)
  nkill <- round(table(to.kill$spp) / (10 - (t-1 - (decade-10)) ) )
  
  ## Kill randomly as many cells per spp
  killed.cells <- integer()
  for(i in names(nkill)){
    if(nkill[i]>0)
      killed.cells <- c(killed.cells, sample(to.kill$cell.id[to.kill$spp==i], nkill[i], replace=F))
  }
  
  return(killed.cells)
}
