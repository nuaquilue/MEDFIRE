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

  print("Initializing parameters")

  ## Output directory (do not never change that, please!)
  out.path <- paste0("outputs/", scn.name)
  
  ## Flags to write spatial and tabular output data
  write.sp.outputs <- TRUE
  write.tbl.outputs <- TRUE
  
  ## Id for distrubance types
  lchg <- 1
  cut <- 2
  thin <- 3
  hfire <- 4
  lfire <- 5
  pb <- 6
  drought <- 7
  afforest <- 8
    
  ## Processes of the model included (IN our OUT) and recurrence (in years) of each process
  ## 1. Climate change, 2. Land-cover changes, 3. Forest management
  ## 4. Wildfires, 5. Prescribed burns, 6. Drought, 7. Post-fire regeneration,
  ## 8. Cohort establihsment, 9. Afforestation, 10. Growth
  processes <- c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  clim.step <- 10
  lchg.step <- 5
  fmgmt.step <- 1
  fire.step <- 1
  pb.step <- 1
  drought.step <- 1
  post.fire.step <- 1
  cohort.step <- 1
  afforest.step <- 1
  growth.step <- 1
  
  ## Time lenght (in years) of a model simulation, from 2010 to 2100
  time.horizon <-  6
  
  ## Number of runs (i.e. replicas)
  nrun <- 1
  
  ## Initialize model global parameters (equal for all scn)
  spp.distrib.rad <- 20 	# neighborhood radius to determine which species belong to that region (in pixels)
  shrub.colon.rad <- 5 		#
  
  ## Characteristics specific of this scenario
  clim.scn <- "rcp85"
  psdm <- 5
  file.dmnd.harvest <- "DemandHarvest_Bioenergy"
  file.clim.severity <- "ClimaticSeverity_fixABA"
  file.pctg.hot.days <- "PctgHotDays_rcp45"
  file.fire.suppression <- "FireSuppression_CurrExtrem"
    
  ## Spread rate, burn probability parameters, prescribed burns
  fire.strength <- 1
  rpb <- 0.6
  stochastic.spread <- 0.75
  pb.th <- 1	
  fire.intens.th <- 0.35  # high vs. low intensity fire, SR_noAcc <= fire.intens.th
  pb.target.area <- NA  # if NA, then burnt as 7*pb.convenient.area, otherwise annually burnt pb.fix.area
  pb.convenient.area <- 15000
  pb.mean <- 1.974
  pb.sd <- 0.683
  pb.fage.th <- 30 ## minimum forest age to apply prescribed burns
  
  ## Save all the variables in .r file to be further loaded by landscape.dyn.r
  if(!file.exists(out.path))
    dir.create(file.path(getwd(), out.path), showWarnings = T) 
  dump(ls(), paste0(out.path, "/scn.def.r"))
  
}