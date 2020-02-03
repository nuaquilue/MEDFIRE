Tmax <- c(14.8,15.6,17.4,19.1,22.5,26.1,28.6,29,26,22.5,17.9,15.1)
Tmin <- c(8.8,9.3,10.9,12.5,16.1,19.8,22.7,23.1,20,16.5,11.9,9.5)
Tmean <- (Tmin+Tmax)/2
J <- c(15,45,75,105,135,165,195,225,255,285,315,345)
cloud <- 1-c(0.59,0.6,0.6,0.61,0.59,0.71,0.88,0.81,0.66,0.56,0.58,0.57)
delta <- 0.4102 * sin(2*pi*(J-80)/365)
phi <- 41.38879 * pi /180
Ra <- 118*acos(-tan(delta)*tan(phi))*sin(phi)*sin(delta)+cos(phi)*cos(delta)*sin(acos(-tan(delta)*tan(phi)))
Rs <- 0.16*Ra*sqrt(Tmax-Tmin)
epsilon <- (0.72+0.005*Tmean)*(1-0.0084*cloud)+0.0084*cloud
Rl <- (4.903*10^(-9))*(epsilon-0.97)*(Tmean+273)^4
Rnet <- 0.77*Rs-Rl
G <- 0.07*(Tmean[c(2:12,1)]-Tmean[c(12,1:11)])
ratio <- (-0.0008*Tmean^2)+(0.0139*Tmean)+0.4235
PET <- 0.408*1.26*ratio*(Rnet-G)
