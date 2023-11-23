# Sandwich data - DIC calculations

demand = c(50,65,72,63,70)

# Set priors for the gamma distribution
a.prior = 0.001
b.prior = 0.001

# Number of Samples to take
n.samp = 10^3

# Sample mu from the gamma for each of the realized observations
set.seed(12345, sample.kind = "Rounding")
mu1 <- rgamma(n.samp, (a.prior + demand[1]), (b.prior+1))
mu2 <- rgamma(n.samp, (a.prior + demand[2]), (b.prior+1))
mu3 <- rgamma(n.samp, (a.prior + demand[3]), (b.prior+1))
mu4 <- rgamma(n.samp, (a.prior + demand[4]), (b.prior+1))
mu5 <- rgamma(n.samp, (a.prior + demand[5]), (b.prior+1))

# Determine the likelihood of the observed data for each of the given sampled mu's

LL <- (dpois(demand[1], mu1, log = T)+
         dpois(demand[2], mu2, log = T)+
         dpois(demand[3], mu3, log = T)+
         dpois(demand[4], mu4, log = T)+
         dpois(demand[5], mu5, log = T)
)

# Calculate D.Mean
mean.D <- mean(-2*LL)
mean.D

# Determine the likelihood of the observed data given the posterior mean
post.mean <- c((a.prior + demand[1])/(b.prior+1),
               (a.prior + demand[2])/(b.prior+1),
               (a.prior + demand[3])/(b.prior+1),
               (a.prior + demand[4])/(b.prior+1),
               (a.prior + demand[5])/(b.prior+1)
)

dmean <- (dpois(demand[1], post.mean[1], log = T) +
            dpois(demand[2], post.mean[2], log = T) +
            dpois(demand[3], post.mean[3], log = T) +
            dpois(demand[4], post.mean[4], log = T) +
            dpois(demand[5], post.mean[5], log = T)
) 

# Calculate mean.D
D.mean <-  (-2)*dmean
D.mean
# Calculate DIC DIC = pd + D(theta.bar) = 2*D(theta).bar - D(theta.bar) => 40.009

pd <- mean.D - D.mean
DIC.daily <- pd + mean.D
DIC.daily



LL <- c()
d.mean <- numeric(5)

# Now, let's attempt to do the same thing, except this time programmatically.
for(i in 1:5){
  # Get the observed data
  observed_data <- demand[i]
  
  # Evaluation of mean.D
  # calculate parameters of the posterior distribution of mu
  a.new = a.prior + observed_data
  b.new = b.prior+1
  
  # calculate the posterior expected value of mu
  post.mean <- a.new/b.new
  
  # sample mu from the gamma with a = a.new, b = b.new
  mu <- rgamma(n.samp, a.new, b.new)
  
  # Assign likelihood
  LL <-  append(LL, dpois(observed_data, mu, log = T))
  
  # Evaluate d.mean using the posterior mean
  d.mean[i] <- dpois(observed_data, post.mean, log = T)
}

## My attempts at doing this programmatically just did not work ##
## Need to Determine why ##

# Model with constant average
mu.avg <- round(mean(demand))

a.new <- a.prior + sum(demand)
b.new <- b.prior + length(demand)
post.mean1 <- a.new/b.new
post.mean1


mu <- rgamma(n.samp, a.new, b.new)

LL1 <- (
  -2*dpois(demand[1], mu, log = T) +
    -2*dpois(demand[2], mu, log = T) +
    -2*dpois(demand[3], mu, log = T) +
    -2*dpois(demand[4], mu, log = T) +
    -2*dpois(demand[5], mu, log = T)
)
mean.d1 <- mean(LL1)
mean.d1
# calculate d(theta_bar)

d.theta.bar <- (
  -2*dpois(demand[1], post.mean1, log = T) +
    -2*dpois(demand[2], post.mean1, log = T) +
    -2*dpois(demand[3], post.mean1, log = T) +
    -2*dpois(demand[4], post.mean1, log = T) +
    -2*dpois(demand[5], post.mean1, log = T)
)

d.mean1 <- d.theta.bar

pd1 <- mean.d1 - d.mean1
dic.daily <- pd1+mean.d1

dic.daily

# SOLUTION #
# parameters for the posterior distribution
dtf$a.post <- 0.01+dtf$x
dtf$b.post <- 0.01+1

# producing a sample from each of the posterior distributions
# results in 1000 x 5 array:
post.samples <- sapply(dtf$a.post,rgamma,n=10^3,rate=1.01)

# evaluating poisson likelihood for each case:
pois.lik <- array(dim=c(1000,5))
for(j in 1:5){
  pois.lik[,j] <- dpois(dtf$x[j],post.samples[,j],log=T)
}

# averaging to obtain mean.D
mean.D <- -2*mean(apply(pois.lik,1,sum))


# evaluating poisson likelihood conditional on posterior means of lambda:

dtf$lambda.post.mean <- dtf$a.post/dtf$b.post
pois.lik.mean <- sum(dpois(dtf$x,dtf$lambda.post.mean,log=T))

D.mean <- -2*pois.lik.mean

p.D <- mean.D - D.mean
DIC.5days <- p.D + mean.D

# Model with no daily fluctuations
# parameters for the posterior distribution
a.post <- 0.01+sum(dtf$x)
b.post <- 0.01+5

# producing a sample from each of the posterior distributions
# results in 1000 x 5 array:
post.samples <- rgamma(10^3,a.post,b.post)

# evaluating poisson likelihood for each case:
pois.lik <- array(dim=c(1000,5))
for(j in 1:5){
  pois.lik[,j] <- dpois(dtf$x[j],post.samples,log=T)
}

# averaging to obtain mean.D
mean.D <- -2*mean(apply(pois.lik,1,sum))


# evaluating poisson likelihood conditional on posterior means of lambda:

lambda.post.mean <- a.post/b.post
pois.lik.mean <- sum(dpois(dtf$x,lambda.post.mean,log=T))

D.mean <- -2*pois.lik.mean

p.D <- mean.D - D.mean
DIC.common <- p.D + mean.D
