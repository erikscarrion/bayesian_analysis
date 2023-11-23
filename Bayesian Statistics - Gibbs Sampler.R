library(tidyverse)
library(glue)

# Implementing a gibbs sampler

# Define sample size
n = 100

# Generate some data and assign priors
y <- rnorm(n)
nybar <- sum(y)

# Sample initial param values from vague priors
tau.zero <- 1/(100^2)
mu.zero <- 0
mu.prior <- rnorm(1, mu.zero, tau.zero)

a.init <- .001
b.init <- .001
tau.prior <- rgamma(1, a.init, b.init)

# Set number of iterations
n.iter <- 10^3

# Set up storage vectors
mu.sample <- tau.sample <- numeric(n.iter)

# Assign starting values
mu.star <- mean(y)
tau.star <- 1/var(y)

# Run the iterations
for(i in 1:n.iter){
  # Given t=t* sample mu* and store
  mu.star <- rnorm(1, (nybar*tau.star + tau.zero*mu.zero)/(n*tau.star + tau.zero),
                   1/sqrt(n*tau.star + tau.zero))
  mu.sample[i] <- mu.star
  
  # Given mu.star sample tau.star and store
  b.star <- b.init + (1/2)*sum((y-mu.sample[i])^2)
  tau.star <- rgamma(1, a.init+(n/2), b.star)
  tau.sample[i] <- tau.star
  
}
hist(tau.sample)
1/sqrt(mean(tau.sample))


data.frame(mu = mu.sample, tau = tau.sample) %>%  ggplot(aes(mu)) +  geom_density()
data.frame(mu = mu.sample, tau = tau.sample) %>%  ggplot(aes(tau)) +  geom_density()

# checking convergence
data.frame(mu = mu.sample, tau = tau.sample) %>%  ggplot(aes(1:n.iter, tau)) +  geom_line()
data.frame(mu = mu.sample, tau = tau.sample) %>%  ggplot(aes(1:n.iter, mu)) +  geom_line()


# Assessment Using the Howell1 data
howell <- read.csv("howell1.csv", sep=";")
str(howell)
summary(howell)
vars <- c("height", "weight", "age")

# Histograms
howell %>% ggplot(aes(height)) + geom_histogram() + ggtitle("hist(height)")
howell %>% ggplot(aes(weight)) + geom_histogram() + ggtitle("hist(weight)")
howell %>% ggplot(aes(age)) + geom_histogram() + ggtitle("hist(age)")

# Densities Colored by Sex
howell %>% ggplot(aes(height)) + 
  geom_density(aes(group = male, fill = male), alpha = 0.8) + 
  ggtitle("density(height)")

howell %>% ggplot(aes(weight, fill = male)) + 
  geom_density(aes(group = male), alpha = 0.75) + 
  ggtitle("density(weight)")

howell %>% ggplot(aes(age)) + 
  geom_density(aes(group = male, fill = male), alpha = 0.8) + 
  ggtitle("density(age)")

adults <- howell %>% filter(age >= 18)
adults %>%  ggplot(aes(height)) + 
  geom_density(aes(group=male, fill=male), alpha=0.75) +
  labs(title = "Distribution of Height",
       subtitle = "Subset: People 18 and older")

# Let's check for normality
par(mfrow = c(1,2))
qqnorm(adults$height[adults$male==1], pch=16, main="QQ Plot Height - Men")
qqline(adults$height[adults$male==1], col = "red", lwd=2)
qqnorm(adults$height[adults$male==0], pch=16, main="QQ Plot Height - Women")
qqline(adults$height[adults$male==0], col = "red", lwd=2)

par(mfrow = c(1,2))
qqnorm(adults$weight[adults$male==1], pch=16, main="QQ Plot Weight - Men")
qqline(adults$weight[adults$male==1], col = "red", lwd=2)
qqnorm(adults$weight[adults$male==0], pch=16, main="QQ Plot Weight - Women")
qqline(adults$weight[adults$male==0], col = "red", lwd=2)

par(mfrow = c(1,2))
qqnorm(adults$age[adults$male==1], pch=16, main="QQ Plot Age - Men")
qqline(adults$age[adults$male==1], col = "red", lwd=2)
qqnorm(adults$age[adults$male==0], pch=16, main="QQ Plot Age - Women")
qqline(adults$age[adults$male==0], col = "red", lwd=2)

################################
### Gibbs Sampler for Height ###

# Set seed
set.seed(12345, sample.kind="Rounding")

# Get data and define sample sizes
height.m <- adults$height[adults$male==1]
n.m <- length(height.m)
nybar.m <- sum(height.m)

height.f <- adults$height[adults$male==0]
n.f <- length(height.f)
nybar.f <- sum(height.f)

# Set priors for mu and sample
mu.zero <- 175
tau.zero <- 1/(5^2)
mu.prior.m <- mu.prior.f <- rnorm(1, mu.zero, tau.zero)

# Set vague priors for tau and sample
a.init = .001
b.init = .001
tau.prior.m <- tau.prior.f <- rgamma(1, a.init, b.init)

# Set number of iterations
n.iter = 10^3

# Set up storage Vectors
mu.sample.m <- mu.sample.f <- tau.sample.m <- tau.sample.f <- numeric(n.iter)

# Assign Starting Values
mu.star.m <- mean(height.m)
tau.star.m <- 1/var(height.m)

mu.star.f <- mean(height.f)
tau.star.f <- 1/var(height.f)

# Run Simulations
for(i in 1:n.iter){
  # Start with men
  # Given t=t* sample mu* and store
  mu.star.m <- mu.sample.m[i] <- rnorm(1,
                                       (nybar.m*tau.star.m + tau.zero*mu.zero)/(n.m*tau.star.m + tau.zero),
                                       1/sqrt(n.m*tau.star.m + tau.zero))
  
  # Sample tau.star given mu.star and store
  a.star.m <- a.init + n.m/2
  b.star.m <- b.init + (0.5)*(sum((height.m - mu.sample.m[i])^2))
  tau.star.m <- tau.sample.m[i] <- rgamma(1, a.star.m, b.star.m)
  
  # Women
  # Given t=t* sample mu* and store
  mu.star.f <- mu.sample.f[i] <- rnorm(1,
                                       (nybar.f*tau.star.f + tau.zero*mu.zero)/(n.f*tau.star.f + tau.zero),
                                       1/sqrt(n.f*tau.star.f + tau.zero))
  
  # Sample tau.star given mu.star and store
  a.star.f <- a.init + n.f/2
  b.star.f <- b.init+(0.5)*(sum((height.f-mu.sample.f[i])^2))
  tau.star.f <- tau.sample.f[i] <- rgamma(1, a.star.f, b.star.f)
 
}

gibbs <- data.frame(iter = seq(1,n.iter),
                    tau_men = tau.sample.m,
                    tau_fem = tau.sample.f,
                    mu_men = mu.sample.m,
                    mu_fem = mu.sample.f)

gibbs %>%  ggplot(aes(iter, tau_men)) +geom_line() + ggtitle("Convergence of Tau - Men")
gibbs %>%  ggplot(aes(iter, tau_fem)) +geom_line() + ggtitle("Convergence of Tau - Females")

gibbs %>%  ggplot(aes(iter, mu_men)) + geom_line() + ggtitle("Convergence of Mu - Men")
gibbs %>%  ggplot(aes(iter, mu_fem)) + geom_line() + ggtitle("Convergence of Mu - Females")


gibbs %>%  ggplot() + 
  geom_density(aes(mu_men, fill ="Male"), alpha = 0.8) + 
  geom_density(aes(mu_fem, fill = "Female"), alpha = 0.8) +
  labs(title = "Gibbs Sampling Distribution of Height",
       subtitle = "Data: Howell dataset") +
  scale_fill_manual(name = "Sex",
                     breaks = c("Male", "Female"),
                     values = c("Male" = "cyan", "Female"="purple"))

gibbs %>%  ggplot() + 
  geom_density(aes(tau_men, fill = "Male"), alpha = 0.8) + 
  geom_density(aes(tau_fem, fill = "Female"), alpha = 0.8) +
  labs(title = "Gibbs Sampling Distribution of Tau",
       subtitle = "Howell dataset") +
  scale_fill_manual(name = "Sex",
                    breaks = c("Male", "Female"),
                    values = c("Male" = "cyan", "Female"="purple"))

options(digits = 5)
gibbs %>%  summarize(mu.men = mean(mu_men),
                     tau.men = mean(tau_men),
                     mu.fem = mean(mu_fem),
                     tau.fem = mean(tau_fem))
quantile(mu.sample.m, c(.025,.975))
quantile(mu.sample.f, c(.025,.975))
quantile(tau.sample.m, c(.025,.975))
quantile(tau.sample.f, c(.025,.975))

# What is the posterior probability that men are on average
# taller than women? P(mu.m > mu.f|data)

1 - pnorm(mean(height.f), mean(mu.sample.m), 1/sqrt(mean(tau.sample.m)))

#What is the posterior probability that a random man is taller than a random woman?
# we must use the posterior predictive

pred.m <- rnorm(n.iter, mean = mean(mu.sample.m), sd = 1/sqrt(mean(tau.sample.m)))
pred.f <- rnorm(n.iter, mean = mean(mu.sample.f), sd = 1/sqrt(mean(tau.sample.f)))
mean(pred.m > pred.f)

d.pred <- data.frame(men = pred.m, female = pred.f)
d.pred %>%  ggplot() + 
  geom_density(aes(x = men), fill ="cyan", alpha =0.8) + 
  geom_density(aes(x = female), fill="purple", alpha = 0.8) +
  labs(title = "Posterior predictive distribution of height",
       subtitle = "Howell Data Set")
