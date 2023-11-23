# The change point problem
# Revisiting Linear Regression
# We'll consider the flow of the river nile for this example

# Load dependencies
library(tidyverse)
library(MCMCglmm)
# Load Data
data("Nile")

nile <- data.frame(year = c(time(Nile)),
                   flow = c(Nile))

nile %>% ggplot(aes(year,flow))+geom_line(size=1.5)


# Intercept only model

m0 <- MCMCglmm(flow~1, data=nile, nitt=1500, burnin=500, thin=1,
               prior = list(B=list(mu=0,V=10^10)),
               verbose = F)

# Flow as a function of year

m1 <- MCMCglmm(flow~year, data=nile, nitt=1500, burnin=500, thin=1,
               prior = list(B=list(mu=c(0,0),V=diag(2)*10^10)),
               verbose = F)
# What is the posterior probability that the trend in flow is negative?
mean(m1$Sol[,2]<0)

# Modelling Change Points
# Evaluate a dummy variable
nile$after <- (nile$year>=1898)*1

# Fit a linear regression model using the dummy var as a covariate
m2 <- MCMCglmm(flow~after, data = nile,
               nitt=1500, burnin=500, thin = 1,
               prior=list(B=list(mu=c(0,0), V=diag(2)*10^10)),
               verbose=F)
summary(m2)$solutions

m0$DIC; m1$DIC; m2$DIC
# After evaluating the dummy variable, we see that the average flow after 1898
# exhibited a decrease of 245.10*10^8*m^3 and the model exhibits a significantly 
# lower DIC than the first two models
