rm(list = ls())
library(tidyverse)
load("inputlyrs/rdata/land.rdata")
head(land)

forest <- filter(land, spp==4, age>7) %>% 
          mutate(class=ifelse(biom<200,"young", ifelse(biom<480, "mature", "old")))  
ggplot(data=forest, aes(age)) + geom_histogram(bins=16) + 
      aes(fill=as.factor(class)) + facet_wrap(class ~ .) + scale_fill_brewer(palette = "Set1") +
      theme(legend.position = "none") + labs(x="age") 
shrub <- filter(land, spp==14, age<400)
ggplot(data=shrub, aes(age, biom)) + geom_line() # + 
  aes(fill=as.factor(class)) + facet_wrap(class ~ .) + scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none") + labs(x="age") 
lmshrub <- lm(biom ~age+I(age^2), shrub)
tope <- lmshrub$coefficients[1] + lmshrub$coefficients[2]*24 +lmshrub$coefficients[3]*24*24
aix <- data.frame(age=1:24)
aix$fuel <- (lmshrub$coefficients[1] + lmshrub$coefficients[2]*aix$age +
  lmshrub$coefficients[3]*aix$age*aix$age)/tope
aix$fuel[aix$fuel<0] <- 0.01
aix

forest <- filter(land, spp<14, age>7) %>% 
  mutate(class=ifelse(biom<200,"young", ifelse(biom<480, "mature", "old")))  
a <-group_by(forest, spp, class) %>% 
  summarise(q05=quantile(age, p=0.005), q25=quantile(age, p=0.25), q5=quantile(age, p=0.5), 
          q75=quantile(age, p=0.75), q95=quantile(age, p=0.95))
write.table(a, "rscripts/outs/AgeBiomClass.txt", row.names = F, quote=F, sep="\t")
