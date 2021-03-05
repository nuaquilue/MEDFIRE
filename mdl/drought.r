######################################################################################
##
######################################################################################

drought <- function(land, clim, decade, t){
  
  ## Count how many ha to kill, totally and this time step
  ## Those forest species out of its climatic range and not burnt the current time step
  to.kill <- left_join(land, select(clim, -spp), by="cell.id") %>% filter(spp<=13, sdm==0, tsdist>0)
  nkill <- round(table(to.kill$spp) / (10 - (t-1 - (decade-10)) ) )
  
  ## Tracking
  if(sum(nkill)>0)
    cat("Drought", "\n") 
  
  ## Kill randomly as many cells per spp
  killed.cells <- integer()
  for(i in names(nkill)){
    if(nkill[i]>1)
      killed.cells <- c(killed.cells, sample(to.kill$cell.id[to.kill$spp==i], nkill[i], replace=F))
    if(nkill[i]==1)
      killed.cells <- c(killed.cells, to.kill$cell.id[to.kill$spp==i])
  }
  # sample(x,1) returns a number in range [1, x], instead of x itself.
  
  return(killed.cells)
}
