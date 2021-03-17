rm(list=ls())

n = 10000
## Generate values for the three distributions


#install.packages("msm")
library(msm) ## rtnorm - get upper and lower limit of shape distribution

#install.packages("GeneralizedHyperbolic")
library(GeneralizedHyperbolic) ## normal-inverse Gaussian

##++++++++++++++++++++++++++++++++++++++++
## SIZE DISTRIBUTION
##++++++++++++++++++++++++++++++++++++++++

xmin = 1 #UM
alpha = 1.6

X.func <- function (X){
  success <- FALSE
  while (!success){
    U = runif(1, 0, 1)
    X = xmin*(1-U)^(1/(1-alpha))
    success <- X < 5000} ##should be smaller than 5000 um 
  return(X)
}

Data <- data.frame(Size = numeric(0));

for(i in 1:n){
  X <- X.func()
  Data <- rbind(Data, X)
}

colnames(Data) <- c("Size")

min(Data$Size) ##20 um
max(Data$Size) ##5000 um

##++++++++++++++++++++++++++++++++++++++++
## SHAPE DISTRIBUTION
##++++++++++++++++++++++++++++++++++++++++

mu1 <- 0.08
mu2 <- 0.44
sd1 <- 0.03
sd2 <- 0.19
lambda1 <- 0.06
lambda2 <- 0.94


#Sample N random uniforms U
U =runif(n)

#Sampling from the mixture
for(i in 1:n){
  if(U[i]<lambda1){
    Data$Shape[i] = rtnorm(1,mu1,sd1, lower = 0, upper = 1)
  }else{
    Data$Shape[i] = rtnorm(1,mu2,sd2, lower = 0, upper = 1)
  }
}

min(Data$Shape) ##0
max(Data$Shape) ##1

##++++++++++++++++++++++++++++++++++++++++
## DENSITY DISTRIBUTION
##++++++++++++++++++++++++++++++++++++++++

d.alpha = 73.8 #tail heaviness
d.beta = 69.9  #asymmetry
d.mu = 0.840   #location
d.delta = 0.0972 #scale

D.func <- function (D){
  success <- FALSE
  while (!success){
    D = rnig(1, mu = d.mu, alpha = d.alpha, beta = d.beta, delta = d.delta)
    success <- D < 2.63} ## include upper limit of 2.63, the max. 
  return(D)
}

Dens <- data.frame(Density = numeric(0));

for(i in 1:n){
  X <- D.func()
  Dens <- rbind(Dens, X)
}

colnames(Dens) <- c("Density")

Data <- cbind(Data, Dens)

##++++++++++++++++++++++++++++++++++++++++
## Quick check results
##++++++++++++++++++++++++++++++++++++++++

par(mfrow = c(1,3))
hist(Data$Size, breaks = 50)
hist(Data$Shape, breaks = 50)
hist(Data$Density, breaks = 50)
