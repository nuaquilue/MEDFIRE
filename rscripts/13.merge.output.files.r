rm(list=ls())
merge.output.files <- function(scn, nset){
  dir.create(file.path(getwd(), "outputs", scn), showWarnings = F) 
  files <- list.files(paste0("outputs/", scn, "_set1"), pattern = "*.txt")
  files <- files[files!="ErrorSR.txt"]
  files <- files[files!="ErrorSRsource.txt"]
  txt <- files[1]
  for(txt in files){
    for(i in 1:nset){
      aux <- read.table(paste0("outputs/", scn, "_set", i, "/", txt), header=T)
      if(exists("dta")){
        aux$run <- aux$run+nrun
        dta <- rbind(dta,aux)
        nrun <- max(dta$run) 
      }
      else{
        dta <- aux
        nrun <- max(aux$run) 
      }
    } 
    table(dta$run)
    write.table(dta, paste0("outputs/", scn, "/", txt), quote=F, row.names=F, sep="\t")
    rm(dta)
  }
}
merge.output.files("Scn_CC_FM_WF", 2)
