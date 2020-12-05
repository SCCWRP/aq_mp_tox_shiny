## Simplifying Microplastics Via Continuous Probability Distributions ##
# Scott Coffin
# 12-1-2020
# Resources: https://pubs.acs.org/doi/suppl/10.1021/acs.est.0c02982/suppl_file/es0c02982_si_001.pdf
# https://pubs.acs.org/doi/suppl/10.1021/acs.estlett.9b00379/suppl_file/ez9b00379_si_001.pdf
# https://pubs.acs.org/doi/10.1021/acs.estlett.9b00379

# Bimodal universal shape distribution for environmental microplastic from Kooi and Koelmans (2019)
library(truncnorm)
library(tidyverse)

#average parameters from distribution
f1 = 0.06
f2 = 0.94
delta1 = 0.03
delta2 = 0.19
mu1 = 0.08
mu2 = 0.44
x = runif(n=100)


#define list from above
param_list = list("f1" = f1, "f2" = f2, "delta1" = delta1, "delta2" = delta2, "mu1" = mu1, "mu2" = mu2, "x" = x)

#permanently define euler's number
e <- exp(1)
lockBinding("e", globalenv())

#starting conditions for x
x = runif(n = 100) #particle size in micrometers

#equation 4
y = f1 * (1/sqrt(2 * pi * delta1 ^ 2)) * (e^-(x - mu1)^2 /2 *delta1 ^ 2)+ f2 * (1/sqrt(2 * pi * delta2 ^ 2)) * (e^-(x - mu2)^2 /2 *delta2 ^ 2)


# Monte Carlo Simulation 
library(MonteCarlo)
#define equation 4
binom <- function(x, f1, f2, delta1, delta2, mu1, mu2){
  #generate sample
  #x = runif(n=1000)
  #calculate CSF
  y <- f1 * (1/sqrt(2 * pi * delta1 ^ 2)) * (e^-(x - mu1)^2 /2 *delta1 ^ 2)+ f2 * (1/sqrt(2 * pi * delta2 ^ 2)) * (e^-(x - mu2)^2 /2 *delta2 ^ 2)
  #return result
  return(list("y" = y))
}

MC_result <- MonteCarlo(func=binom, nrep=1, param_list=param_list)

df<-MakeFrame(MC_result)
head(df)
library(dplyr)
library(ggplot2)
tbl <- tibble(df)

ggplot(tbl) + 
  geom_density(aes(y))
