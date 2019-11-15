play <- function(){
  rm(list=ls())  
  setwd("c:/work/MEDMOD/SpatialModelsR/MEDFIRE")  #NúLaptop
  setwd("d:/MEDMOD/SpatialModelsR/MEDFIRE")   #CTFC
  
  scn.name <- "Test02"
  plot.drought(scn.name)
  
}

species.name.color <- function(){
  species <- data.frame(spp=1:13, species=c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster",
                                            "puncinata", "aalba", "qilex", "qsuber", "qfaginea", "qhumilis",
                                            "fsylvatica", "other"),
                        color=c("chartreuse3", "darkolivegreen1", "darkseagreen", "forestgreen",
                                "olivedrab4", "darkslategrey", "blue4", "gold", "saddlebrown",
                                "sienna2", "palegoldenrod", "red3", "purple3"),
                        color.sort=c("royalblue4","red4", "magenta3", "chartreuse2", "darkolivegreen1",
                                     "darkslategrey",  "seagreen1", "forestgreen", "grey10",   #
                                     "darkorange2", "palegoldenrod", "gold", "saddlebrown"))
  save(species, file="rscripts/ins/species.rdata")
}


plot.drought <- function(scn.name){
  
  library(tidyverse)
  load("rscripts/ins/species.rdata")  
  
  dta.land <- read.table(paste0("outputs/", scn.name, "/Land.txt"), header=T)
  dta.land <- mutate(dta.land, decade= ((dta.land$year-1) %/% 10)*10+2010) %>%
              mutate(decade=ifelse(decade==2100,2090, decade)) %>%
              group_by(decade, spp, run) %>% summarise(area=sum(vol))%>%
              group_by(decade, spp) %>% summarise(area=mean(area)) 
  
  dta.drought <- read.table(paste0("outputs/", scn.name, "/Drought.txt"), header=T)
  dta.drought <- mutate(dta.drought, decade=((year-1) %/% 10)*10+2010) %>%
                 mutate(decade=ifelse(decade==2100,2090, decade)) %>%
                 group_by(decade, spp, run) %>% summarise(ha=sum(ha))%>%
                 group_by(decade, spp) %>% summarise(ha=mean(ha)) %>% 
                 left_join(dta.land, by=c("decade", "spp")) %>% 
                 left_join(select(species, spp, species), by="spp") %>%
                 mutate(pctg.kill=100*ha/area)
  
  
  ggplot(data=dta.drought, aes(x=as.factor(decade), y=ha/100, fill=species)) + geom_bar(stat="identity") +
    scale_fill_manual(values=as.character(species$color.sort)) + 
    ggtitle("Area killed by drought per decade") + ylab("km2") + xlab("period") +
    theme(axis.text.x = element_text(angle = 90))
  
  ggplot(data=dta.drought, aes(x=as.factor(decade), y=pctg.kill, fill=species)) + geom_bar(stat="identity") +
    scale_fill_manual(values=as.character(species$color.sort)) + 
    ggtitle("Pctg of actual area killed by drought per decade") + ylab("%") + xlab("period") +
    theme(axis.text.x = element_text(angle = 90))
  

}

