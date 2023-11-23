# Metropolis-Hastings and Gibbs
# 

# Metropolis-Hasting Step for the Normal Mean:
# set seed
set.seed(12345,sample.kind="Rounding")

# Number of samples to take
n <- 100

# sample from the normal
# with mean 2 and sd 2
y <- rnorm(n,2,2)

# Vague priors for models
mu0 <- 0; tau0 <- 10^{-4}
a <- .01 ; b <- .01

# Set number of iterations
iter <- 10^3

# Set up storage for our params
R.storage <- mu.storage <- tau.storage <- numeric(iter)

# Initialize Mu and Tau
mu <- mean(y)
tau <- sd(y)

# Decide on a width of the proposal distribution
delta <- .3

for(iter in 1:iter){
  # Propose a new value:
  mu.new <- rnorm(1, mu, sd=delta)
  
  # Evaluate the log of the acceptance ratio
  logR <- sum(dnorm(y, mu.new, 1/sqrt(tau), log = T)) -
          sum(dnorm(y, mu,     1/sqrt(tau), log = T)) +
          dnorm(mu.new, mu0, 1/sqrt(tau0), log = T) - 
          dnorm(mu,     mu0, 1/sqrt(tau0), log = T)
  
  # Accept with probability min(R,1)
  # draw U from unif(0,1) and accept if logU < logR
  
  logU <- log(runif(1,0,1))
  if(logU < logR){mu <- mu.new}
  
  # Step for Tau
  tau <- rgamma(1, a + n/2, b + .5*sum((y-mu)^2))
  
  # update the monitors
  mu.storage[iter] <- mu
  tau.storage[iter] <- tau
  R.storage[iter] <- logR
}
# par(mfrow=c(2,1))
# plot(mu.storage, ty="l",ylab=expression(mu))
# plot(1/sqrt(tau.storage), ty='l', ylab='sd')

plot(exp(R.storage), type = "l")