prob.igni <- function(land, orography, clim, interface){
  plan.st <- c(663.2897, 212.5369)
  ## Read ignition probability model
  pigni.mdl <- read.table("inputfiles/ProbIgniMdl.txt", header=T)
  ## Compute regression  
  z = pigni.mdl$intercept + pigni.mdl$elev*orography$elev + pigni.mdl$slope*orography$slope +
      pigni.mdl$precip*(clim$precip*plan.st[2]+plan.st[1]) +   ## des-estandaritzar precipitació
      pigni.mdl$nat*(interface$x == 3) + pigni.mdl$urbnat*(interface$x == 6)+ pigni.mdl$crpnat*(interface$x == 7) + 
      pigni.mdl$road*orography$road/100  ## road/100 it's ok
  ## Assign NA to non-burnable covers
  z[land$spp>17] <- NA
  ## Return prob.igni (logistic regression)
  dta <- data.frame(cell.id=land$cell.id, p=(1/(1+exp(-1*z)))*100)
  return(dta)
}