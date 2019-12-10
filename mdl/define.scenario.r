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
  
  library(raster)
  
  print("Initializing parameters")

  ## Output directory (do not never change that, please!)
  out.path <- paste0("outputs/", scn.name)
  
  ## Flags to write spatial and tabular output data
  write.sp.outputs <- TRUE
  write.tbl.outputs <- TRUE

  ## Number of runs (i.e. replicas)
  nrun <- 1
  
  ## Processes of the model included (IN our OUT)
  ## 1. Climate change, 2. Interfaces, 3. Forest management
  ## 4. Wildfires, 5. Prescribed burns, 6. Drought, 7. Post-fire regeneration,
  ## 8. Cohort establihsment, 9. Afforestation, 10. Growth
  processes <- c(TRUE, TRUE, F, F, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  
  ## Fix the recurrence (in years) of each process 
  ## 1. Climate change
  clim.step <- 10
  ## 2. Interfaces
  interface.step <- 0  ## to not update it, use only the initial interface layer
  ## 3. Forest management
  fmgmt.step <- 1
  ## 4. Wildfires
  fire.step <- 1
  ## 5. Prescribed burns
  pb.step <- 1
  ## 6. Drought
  drought.step <- 1
  ## 7. Post-fire regeneration
  post.fire.step <- 1
  ## 8. Cohort establihsment
  cohort.step <- 1
  ## 9. Afforestation
  afforest.step <- 1
  ## 10. Growth
  growth.step <- 1
  
  ## The minimum time step among processes
  # min.time.step <- pmin(clim.step, interface.step, pigni.step, fmgmt.step,
  #                       fire.step, pb.step, drought.step, post.fire.step,
  #                       cohort.step, afforest.step, growth.step)
  
  ## Time lenght (in years) of a model simulation, from 2010 to 2100
  time.horizon <-  91
  
  ## Characteristics specific of this scenario
  clim.scn <- "rcp45"
  psdm <- 10
  file.dmnd.harvest <- "DemandHarvest_Bioenergy"
  file.clim.severity <- "ClimaticSeverity_rcp45_fixABA"
  file.pctg.hot.days <- "PctgHotDays_rcp45"
    
  ## " Save all the variables in .r file to be further loaded by landscape.dyn.r"
  if(!file.exists(out.path))
    dir.create(file.path(getwd(), out.path), showWarnings = T) 
  dump(ls(), paste0(out.path, "/scn.def.r"))
  
}