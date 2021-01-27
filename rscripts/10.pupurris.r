library(tidyverse)

elevx <- 100
elevy <- seq(0,200,10)
dif.elev <- elevy-elevx
dist <- 100
slope <- pmax(pmin(dif.elev/dist,0.5),-0.5)+0.5
plot(elevy, slope, type="l")
abline(v=100, col="red")


fire.wind <- 0 # vent dominant
windir <- seq(0,315,45)
((ifelse(abs(windir-fire.wind)>180, 360-abs(windir-fire.wind), abs(windir-fire.wind)))/180)  


rm(list=ls())
sr <- seq(0,1,0.1)
a <- data.frame(fuel="low", sr=sr, fi=0.2*sr)
b <- data.frame(fuel="med", sr=sr, fi=0.4*sr)
c <- data.frame(fuel="high", sr=sr, fi=0.6*sr)
d <- data.frame(fuel="extr", sr=sr, fi=0.95*sr)
kk <- rbind(a,b,c,d)
kk$fi.acc <- "small"
fi.acc <- 5
a <- data.frame(fuel="low", sr=sr, fi=0.2*sr*fi.acc)
b <- data.frame(fuel="med", sr=sr, fi=0.4*sr*fi.acc)
c <- data.frame(fuel="high", sr=sr, fi=0.6*sr*fi.acc)
d <- data.frame(fuel="extr", sr=sr, fi=0.95*sr*fi.acc)
kk2 <- rbind(a,b,c,d)
kk2$fi.acc <- "large"
kk <- rbind(kk, kk2)
ggplot(data=kk, aes(x=sr, y=fi, colour=fuel)) + geom_line(size=1.2) +  facet_wrap(~fi.acc) + 
  theme_classic() +  theme(text = element_text(size=16)) + scale_color_viridis_d()

kk$pb <- 1+0.4*log(kk$fi)
ggplot(data=kk, aes(x=sr, y=pb, colour=fuel)) + geom_line(size=1.2) +  facet_wrap(~fi.acc) + 
    theme(text = element_text(size=16)) + scale_color_viridis_d()




kk$rpb <- "rpb0.1"
kk$pb <- 1+0.1*log(kk$fi)
head(kk)
aux <- kk
aux$rpb <- "rpb0.3"
aux$pb <- 1+0.3*log(aux$fi)
head(aux)
rr <- rbind(kk,aux)
rr$what <- paste0(rr$fi.acc, "-", rr$rpb)
table(rr$what)
ggplot(data=rr, aes(x=sr, y=pb, colour=fuel)) + geom_line(size=1.2) +  facet_wrap(~what) + 
  theme_classic() +  theme(text = element_text(size=16)) + scale_color_viridis_d()


atarget <- 386
aburnt <- c(9,27,51,79,117,170,228,299,366,386)
z <- scales::rescale(aburnt/atarget, to=c(-3,2), from=c(0,1))
plot(aburnt/atarget,z, type="b")


