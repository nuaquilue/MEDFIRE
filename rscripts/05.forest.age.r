################################################################################
## Derivation of Forest Age from Forest Height for each tree species
## For each species one formula / parameters
## Arguments are the Land-cover Forest-species map (LCFspp), and the Heigth (Hm) map
################################################################################
forest.age <- function(LCFspp, Hm){
  ## Count occupations
  t.LCF <- table(LCFspp[])
  
  ## A single data frame with these two variables and the response variable Age
  dta <- data.frame(LCF=LCFspp[], H=Hm[])
  dta$Age <- NA
  
  
  ########################################
  # Pinus halepensis (Montero et al. 2001)
  # LT = 150
  # Mat = 15
  ########################################
  spp <- 1
  max.age <- 150
  hfun <-function(t) {
    a = 15.21453
    b = 0.0203954
    c = 1.046295
    return(a*(1-exp(-b*(t-c)))^(1/c))
  }
  ages <- hfun(1:max.age)   
  th <- function(x)  which(ages>x)[1]
  dta$Age[!is.na(dta$LCF) & dta$LCF==spp] <- 
    apply(as.data.frame(dta$H[!is.na(dta$LCF) & dta$LCF==spp]), 1, th )
  dta$Age[is.na(dta$Age) & dta$LCF==spp] <- max.age
  
  
  ########################################
  # Pinus nigra (Palahi and Grau 2003)
  # LT = 400
  # Mat = 15
  ########################################
  spp <- 2
  max.age <- 400
  hfun<-function(t2) {
    a=16.884
    b=0.033
    t1=60
    H1=14
    H2=(t2^2)/(a+t2*((t1/H1)-b*t1-(a/t1)+b*t2))
    return(H2)
  }
  ages <- hfun(1:max.age)   
  th <- function(x)  which(ages>x)[1]
  dta$Age[!is.na(dta$LCF) & dta$LCF==spp] <- 
    apply(as.data.frame(dta$H[!is.na(dta$LCF) & dta$LCF==spp]), 1, th )
  dta$Age[is.na(dta$Age) & dta$LCF==spp] <- max.age
  
  
  ########################################
  # Pinus pinea (Pique 2003)
  # LT = 150
  # Mat = 15
  ########################################
  spp <- 3
  max.age <- 150
  hfun<-function(t2) {
    t1=100
    H1=15
    H2=exp((5.5618)+(log(H1)-5.5618)*((t2/t1)^(-0.184601)))
    return(H2)
  }
  ages <- hfun(1:max.age)   
  th <- function(x)  which(ages>x)[1]
  dta$Age[!is.na(dta$LCF) & dta$LCF==spp] <- 
    apply(as.data.frame(dta$H[!is.na(dta$LCF) & dta$LCF==spp]), 1, th )
  dta$Age[is.na(dta$Age) & dta$LCF==spp] <- max.age
  
  
  ########################################
  # Pinus sylvestris (Palahi et al 2004)
  # LT = 400
  # Mat = 15
  ########################################
  spp <- 4
  max.age <- 400
  hfun<-function(t2) {
    t1=100
    H1=18.5
    H2=(t2^2)/(18.6269+(t2*((t1/H1)-(0.03119*t1)-(18.6269/t1)+(0.03119*t2))))
    return(H2)
  }
  ages <- hfun(1:max.age)   
  th <- function(x)  which(ages>x)[1]
  dta$Age[!is.na(dta$LCF) & dta$LCF==spp] <- 
    apply(as.data.frame(dta$H[!is.na(dta$LCF) & dta$LCF==spp]), 1, th )
  dta$Age[is.na(dta$Age) & dta$LCF==spp] <- max.age
  
  
  ########################################
  # Pinus pinaster from Garc?a-Abej?n & G?mez-Loranca 1989
  # LT = 150
  # Mat = 15
  ########################################
  spp <- 5
  max.age <- 150
  inv.age = 1/seq(30,120, by=10)
  log.h = log(c(10.5,13.9,16.7,19.2,21.4,23.3,24.9,26.3,27.6,28.7))
  m = lm(log.h~inv.age)
  hfun <- function(t) {
    lh = m$coefficients[1] - m$coefficients[2]*(1/t)
    return(exp(lh))
  }
  ages <- hfun(1:max.age)   
  th <- function(x)  which(ages>x)[1]
  dta$Age[!is.na(dta$LCF) & dta$LCF==spp] <- 
    apply(as.data.frame(dta$H[!is.na(dta$LCF) & dta$LCF==spp]), 1, th )
  dta$Age[is.na(dta$Age) & dta$LCF==spp] <- max.age
  
  
  ########################################
  # Pinus uncinata (Calama 2004)
  # LT = 400
  # Mat = 15
  ########################################
  spp <- 6
  max.age <- 400
  hfun<-function(t2) {
    t1=100
    H1=14
    H2=(24.4295)/(1-((1-24.4295/H1)*((t1/t2)^1.5464)))
    return(H2)
  }
  ages <- hfun(1:max.age)   
  th <- function(x)  which(ages>x)[1]
  dta$Age[!is.na(dta$LCF) & dta$LCF==spp] <- 
    apply(as.data.frame(dta$H[!is.na(dta$LCF) & dta$LCF==spp]), 1, th )
  dta$Age[is.na(dta$Age) & dta$LCF==spp] <- max.age
  
  
  ########################################
  # Abies alba (ONF 1999)
  # LT = 400
  # Mat = 20
  ########################################
  spp <- 7
  max.age <- 400
  hfun<-function(t2) {
    H2=-0.0013*t2^2+0.3863*t2+1.7257
    return(H2)
  }
  ages <- hfun(1:max.age)   
  th <- function(x)  which(ages>x)[1]
  dta$Age[!is.na(dta$LCF) & dta$LCF==spp] <- 
    apply(as.data.frame(dta$H[!is.na(dta$LCF) & dta$LCF==spp]), 1, th )
  dta$Age[is.na(dta$Age) & dta$LCF==spp] <- max.age
  
  
  ########################################
  # Quercus ilex and Quercus suber 
  # LT = 400
  # Mat = 20
  ########################################
  max.age <- 400
  hfun<-function(t2) {
    H1=10
    t1=80
    H2=(20.7216)/(1-((1-20.7216/H1)*((t1/t2)^1.4486)))
    return(H2)
  }
  ages <- hfun(1:max.age)   
  th <- function(x)  which(ages>x)[1]
  # Q. ilex
  spp <- 8
  dta$Age[!is.na(dta$LCF) & dta$LCF==spp] <- 
    apply(as.data.frame(dta$H[!is.na(dta$LCF) & dta$LCF==spp]), 1, th )
  dta$Age[is.na(dta$Age) & dta$LCF==spp] <- max.age
  # Q. suber
  spp <- 9
  dta$Age[!is.na(dta$LCF) & dta$LCF==spp] <- 
    apply(as.data.frame(dta$H[!is.na(dta$LCF) & dta$LCF==spp]), 1, th )
  dta$Age[is.na(dta$Age) & dta$LCF==spp] <- max.age
  
  
  ########################################
  # Quercus faginea and Quercus humilis
  # Lopez-Senespleda et al. (2007)
  # LT = 400
  # Mat = 20
  ########################################
  max.age <- 400
  hfun<-function(t2) {
    H1=7
    t1=50
    H2=exp((3.094)+(log(H1)-3.094)*((t2/t1)^(-0.562)))
    return(H2)
  }
  ages <- hfun(1:max.age)   
  th <- function(x)  which(ages>x)[1]
  # Q. faginea
  spp <- 10
  dta$Age[!is.na(dta$LCF) & dta$LCF==spp] <- 
    apply(as.data.frame(dta$H[!is.na(dta$LCF) & dta$LCF==spp]), 1, th )
  dta$Age[is.na(dta$Age) & dta$LCF==spp] <- max.age
  # Q. humilis
  spp <- 11
  dta$Age[!is.na(dta$LCF) & dta$LCF==spp] <- 
    apply(as.data.frame(dta$H[!is.na(dta$LCF) & dta$LCF==spp]), 1, th )
  dta$Age[is.na(dta$Age) & dta$LCF==spp] <- max.age
  
  
  ########################################
  # Fagus sylvatica (ONF 1997)
  # ONF. 1997. Guide des sylvicultures du h?tre dans les pyren?es. Office National des For?ts. 73 pp
  # LT = 200
  # Mat = 20
  ########################################
  spp <- 12
  max.age <- 200
  hfun<-function(t) {
    b=-7.27032
    return(exp(5.4813+(b*(1/t)^0.25)))
  }
  ages <- hfun(1:max.age)   
  th <- function(x)  which(ages>x)[1]
  dta$Age[!is.na(dta$LCF) & dta$LCF==spp] <- 
    apply(as.data.frame(dta$H[!is.na(dta$LCF) & dta$LCF==spp]), 1, th )
  dta$Age[is.na(dta$Age) & dta$LCF==spp] <- max.age
  
  
  ########################################
  # Other trees (=Castanea sativa) 
  # Cabrera (1997), modified by Beltran et al. (2013)
  # LT = 200
  # Mat = 20
  ########################################
  spp <- 13
  max.age <- 200
  hfun<-function(t2) {
    H2=10*(2.1602*(1-exp(-0.519*(t2/10))^(1/0.988)))
    return(H2)
  }
  ages <- hfun(1:max.age)   
  th <- function(x)  which(ages>x)[1]
  dta$Age[!is.na(dta$LCF) & dta$LCF==spp] <- 
    apply(as.data.frame(dta$H[!is.na(dta$LCF) & dta$LCF==spp]), 1, th )
  dta$Age[is.na(dta$Age) & dta$LCF==spp] <- max.age
  
  
  ## Build the Forest Age raster
  AGE <- raster(vals=dta$Age, ext=extent(LCFspp), res=res(LCFspp), crs=crs(LCFspp))

  return(AGE)  
}


