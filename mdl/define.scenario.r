######################################################################################
###  define.scenario()
###
###  Description >  Initialize the scenario parameters and the global variables of 
###   MEDFIRE model
###
###  Arguments >  
###   scn.name : identificative name of the scenario (string)
###
###  Details > By default, the output directory is ..\outputs\scn.name\ and all the
###   objects are saved in the file scn.def.r.
###
###  Value >  An R script.
######################################################################################

define.scenario <- function(scn.name){

  ## Output directory (do not never change that, please!)
  out.path <- paste0("outputs/", scn.name)
  
  ## Time lenght (in years) of a model simulation, from 2010 to 2099
  time.horizon <- 90
  
  ## Number of runs (i.e. replicas)
  nrun <- 1
  
  ## Flags to write spatial and tabular output data
  write.maps <- F
  write.freq <- 10
  spin.up <- T
  
  ## Processes of the model included (TRUE or FLASE)
  is.climate.change <- F
  is.land.cover.change <- F
  is.harvest <- F
  is.wildfire <- F
  is.prescribed.burn <- F
  is.drought <- T
  is.postfire <- T
  is.cohort.establish <- T
  is.afforestation <- T
  is.encroachment <- F
  is.growth <- T
  
  ## Processes frequency (in years) 
  clim.step <- 10
  time.step <- 1
  
  ## Global model's parametres
  # Radius of the neighborhood (in pixels) to find out if a species is present in a region
  spp.distrib.rad <- 25 	# i.e. 2500 m -> 2.5 km, a ~circular (in fact it's a star) of radius 2.5 km (circle of 5 km diameter)
  # Radius of the neighborhood (in pixels) to look for mature tree species ready to colonize shrubs
  # or locations killed by drought
  colon.rad <- 5 		# i.e. 500 m

  ## Fire parameters (should not change to much): Spread rate, burn probability, prescribed burns
  rpb <- 0.2
  pb.upper.th <- 0.8
  pb.lower.th <- -1
  fire.intens.th <- 0.19  # high vs. low intensity fire, sr <= fire.intens.th
  pb.target.area <- NA  # if NA, then burnt as 7*pb.convenient.area, otherwise annually burnt pb.fix.area
  pb.convenient.area <- 400 ## should be 15.000 ha
  accum.burnt.area <- rep(pb.convenient.area,7)
  pb.mean <- 1.974
  pb.sd <- 0.683
  pb.fage.th <- 30 ## minimum forest age to apply prescribed burns
  
  ## Scenario parameters
  clim.scn <- NA #"rcp45", "rcp85"
  clim.mdl <- "SMHI-RCA4_MOHC-HadGEM2-ES"
  file.dmnd.lchg <- "DemandLChg"
  file.pattern.lchg  <- "PatternLChg"
  file.dmnd.harvest <- "DemandHarvest"
  file.clim.severity <- paste0("ClimaticSeverity_", ifelse(is.na(clim.scn), "noCC", clim.scn))
  file.pctg.hot.days <- paste0("PctgHotDays_", ifelse(is.na(clim.scn), "noCC", clim.scn)) 
  file.fire.suppression <- "FireSuppress"
  file.sprd.weight <- "WeightSprdFactors"
  
  ## Save all the variables in .r file to be further loaded by landscape.dyn.r
  if(!file.exists(out.path))
    dir.create(file.path(getwd(), out.path), showWarnings = T) 
  dump(ls(), paste0(out.path, "/scn.def.r"))
  
}