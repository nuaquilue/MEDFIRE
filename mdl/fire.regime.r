######################################################################################
###  fire.regime()
###
######################################################################################

fire.regime <- function(land, orography, pigni, swc, t, burnt.cells){
  
  ## Read and load input data
  load("inputlyrs/rdata/igni.fst.rdata")
  prob.hot <- read.table("inputfiles/ProbHot.txt", header=T)
  prob.conv <- read.table("inputfiles/ProbConv.txt", header=T)
  aba.dist <- read.table("inputfiles/AnnualBurntAreaDist.txt", header=T)
  fs.dist <- read.table("inputfiles/FireSizeDist.txt", header=T)
  clim.severity <- read.table(paste0("inputfiles/", file.clim.severity, ".txt"), header=T)
  pctg.hot.days <- read.table(paste0("inputfiles/", file.pctg.hot.days, ".txt"), header=T)
  
  
  ## Decide climatic severity and generate annual target area
  if(sum(clim.severity[clim.severity$year==t,2:4])>0){  # fixed annual burnt area
    is.aba.fix <- T
    clim <- ifelse(clim.severity[clim.severity$year==t, ncol(clim.severity)]==100,1,0)
    area.target <- clim.severity[clim.severity$year==t, swc+1]
  }
  else{ # stochastic annual burnt area
    is.aba.fix <- F
    if(runif(1,0,100) <= clim.severity[clim.severity$year==7, ncol(clim.severity)]){ # not-mild
      pctg <- pctg.hot.days[pctg.hot.days$year==t, swc+1]
      prob.extrem <- 1/(1+exp(-(prob.hot$inter[swc] + prob.hot$slope[swc]*pctg)))
      if(runif(1,0,100) <= prob.extrem) # extreme
        clim <- 2
      else # severe
        clim <- 1
    }
    else # mild
      clim <- 0
    area.target <- round(min(200000, max(10, 
                         rlnorm(1, aba.dist$meanlog[aba.dist$clim==clim & aba.dist$swc==swc],
                                   aba.dist$sdlog[aba.dist$clim==clim & aba.dist$swc==swc]))))
  }
  
  
  ## Update prob.igni according to swc
  pigni <- pigni*igni.fst[,ifelse(swc==1,1,2)]

  
  ## Decide the 
}

