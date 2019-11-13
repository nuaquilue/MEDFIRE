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
  # plot.fires <- FALSE

  ## Number of runs (i.e. replicas)
  nrun <- 1
  
  ## Processes of the model included (IN our OUT)
  ## 1. Climate change, 2. Interfaces, 3. Forest management
  ## 4. Wildfires, 5. Prescribed burns, 6. Drought, 7. Post-fire regeneration,
  ## 8. Cohort establihsment, 9. Afforestation, 10. Growth
  processes <- c(TRUE, TRUE, TRUE, F, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  
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
  time.horizon <- 91
  
  ## Characteristics specific of this scenario
  clim.scn <- "RCP45"
  harvest.dmnd <- "DemandHarvest_Bioenergy.txt"
    
  # ## FIRE disturbance definitions: distributions of fire regime per FRZone
  # fire.regime.file <- "inputfiles/NumFires.txt"
  # fire.sizes.file <- "inputfiles/FireSizesEmpiric.txt" 
  # NFdistrib <- read.table(fire.regime.file, header = T)
  # FSdistrib <- read.table(fire.sizes.file, header = T)
  # fire.rate.increase <- 0 # increase in fire frequency over the planning horizon (climate change)
  # 
  # ## SPRUCE BUDWORM disturbance definitions: probability of tree mortality following SBW defoliation
  # # sbw.mortality <- read.table("inputfiles/SBWmortality.txt", header=T)
  # 
  # ## WIND disturbance definitions
  # # wind.cycle <- read.table("inputfiles/WindCycle.txt", header=T)
  # # wind.cycle$target.land <- wind.step/wind.cycle$cycle
  # 
  # ## FOREST MANAGEMENT disturbance definitions
  # age.mat <- 80         # default number of years for a stand to reach maturity (harvestable)
  # age.mat.pc <- 30      # default number of years after a partial cut for a stand to become harvestable again
  # target.old.pct <- 0    # default target proportion of old (>= mature) forests to maintain inside management units
  # diff.prematurite <- 0 # number of years of pre-mature spcies before recuperation
  # salvage.rate.event <- 1  # maximal proportion of burnt mature forests that can be salvaged realistically in a given fire event [0,1]
  # salvage.rate.FMU <- 1  # maximum proportion of salvaged burnt wood allowed in the timber supply in each FMU [0,1]
  # ecocrisis <- FALSE  # presence of economic crises during simulations
  # ecocrisis.freq <- 0.1  # proportion of years affected by an economic crisis
  # hor.plan <- 16 # time horizon for forest management planification (years)
  # 
  # ## VEGETATION DYNAMICS: post-disturbance regeneration and forest succession
  # ## Species are PET - Trembling aspen, BOJ - Yellow birch, ERS - Sugar maple, SAB - Balsam fir, EPN - Black spruce
  # radius.buff <-  c(75000, 60000, 50000, 50000, 50000) # estimated maximum colonization distance (in m)
  # nb.buff <- c(1,1,1,1,1)     # maximum number of neighbours on the buffer
  # persist <- c(1,1,1,1,1)     # it indicates whether we allow the transition probability to remain high locally
  #                             # (cell level) when the species is outside its optimal climatic condition (1=yes, 0=no)
  # age.seed <- 50     # below this stand age, seed production is very low, and regeneration failures are more likely
  # p.failure <- 0     # probability of regeneration failure in young (< 50 years) burned stands
  # post.fire.reg <- read.table("inputfiles/PostFireReg.txt", header=T)
  # # post.epid.reg <- read.table("inputfiles/PostEpidReg.txt", header=T)
  # # post.wind.reg <- read.table("inputfiles/PostWindReg.txt", header=T)
  # post.harvest.reg <- read.table("inputfiles/PostHarvestReg.txt", header=T)
  # forest.succ <- read.table("inputfiles/ForestSucc.txt", header=T)
  # ThMeanTemp <- read.table("inputfiles/ThMeanTemp.txt", header=T)  
  # ThAnnualPrecip <- read.table("inputfiles/ThAnnualPrecip.txt", header=T)  
  # ThSoil <- read.table("inputfiles/ThSoil.txt", header=T)  
  # 
  ## " Save all the variables in .r file to be further loaded by landscape.dyn.r"
  if(!file.exists(out.path))
    dir.create(file.path(getwd(), out.path), showWarnings = T) 
  dump(ls(), paste0(out.path, "/scn.def.r"))
  
}