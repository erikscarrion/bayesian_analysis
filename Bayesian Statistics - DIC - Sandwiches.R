# Bayesian Statistics - The Beta Binomial Model
library(tidyverse)
library(glue)

# Suppose we have 50 seeds, of which 35 germinated
# What is the distribution of p?
# Suppose we have a B(1,1) prior for p
n1 = 50
y1 = 35
n2 = 80
y2 = 63


y.new = y1+y2
n.new = n1+n2
a = 1
b = 1

a.new = a+y.new
b.new = b+n.new-y.new

a.new; b.new

# What is the expected value of p with these new parameters?
p.exp <- a.new/(a.new+b.new)
p.exp

# What is the posterior probability that the germination probability  is above 80?
options(digits=4)
1 - pbeta(.80,a.new,b.new)

# Produce the posterior predictive distribution for the number of seeds germinating
# out of a batch of n = 100

n.samp <- 10^4
N <- 100
preds <- numeric(n.samp)
preds1 <- numeric(n.samp)
ps <- rbeta(n.samp, a.new, b.new)
ys <- sapply(ps, rbinom, n = n.samp, size = N)
ys[1:5]

for(i in 1:n.samp){
  p <- rbeta(1, a.new, b.new)
  preds1[i] <- rbinom(1, N, p)
}

# calculating DIC - Sandwich Model Revisted

demand = c(50, 65,72,63, 70)

# Use the informal Gamma(.01,.01) prior to estimate day-specific demand and then the common
# model (where demand remains constant during the week)
# compare each model using DIC
options(digits = 5)
# Set up priors
a <- b <- .001

# Number of Samples
n.samp <- 10^4
days = c("monday", "tuesday", "wednesday", "thursday", "friday")

mu1 <- rgamma(n.samp, a+demand[1], b+1)
mu2 <- rgamma(n.samp, a+demand[2], b+1)
mu3 <- rgamma(n.samp, a+demand[3], b+1)
mu4 <- rgamma(n.samp, a+demand[4], b+1)
mu5 <- rgamma(n.samp, a+demand[5], b+1)

LL <- dpois(demand[1], mu1, log = T)*(-2) +
      dpois(demand[2], mu2, log = T)*(-2) +
      dpois(demand[3], mu3, log = T)*(-2) +
      dpois(demand[4], mu4, log = T)*(-2) +
      dpois(demand[5], mu5, log = T)*(-2)

mean.D <- mean(LL)
mean.D
D.mean <- dpois(demand[1], (a+demand[1])/(b+1), log = T)*(-2) +
          dpois(demand[2], (a+demand[2])/(b+1), log = T)*(-2) +
          dpois(demand[3], (a+demand[3])/(b+1), log = T)*(-2) +
          dpois(demand[4], (a+demand[4])/(b+1), log = T)*(-2) +
          dpois(demand[5], (a+demand[5])/(b+1), log = T)*(-2)


DIC.daily = 2*mean.D - D.mean
DIC.daily # 40.01



likelihoods <- data.frame(mu = numeric(),
                          ll = numeric())
dmean = numeric(5)
# can we achieve an equivalent result using a loop?
for(i in 1:5){
  observed_data <- demand[i]
  
  # sample mu from its posterior distribution
  a.new = a + observed_data
  b.new = b + 1
  post.mean <- a.new/b.new
  
  mu.sample <- rgamma(n.samp, a.new, b.new)
  
  # Calculate the likelihood of the observed data for each of sampled mu's
  LL <- dpois(observed_data, mu.sample, log = T)*(-2)
  
  # Store mu.sample and LL as a df and append to likelihoods
  df <- data.frame(mu = mu.sample, ll = LL)
  likelihoods <- rbind(likelihoods, df)
  
  # Calculate the likelihood of the observed data given the posterior mean
  dmean[i] <- dpois(observed_data, post.mean, log=T)*(-2)
  
  }
