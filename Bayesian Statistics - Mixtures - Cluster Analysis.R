# Bayesian Statistics - Cluster Analysis
# Cinderella Data

data = read.csv("cinderella.txt")
head(data)

data %>% ggplot(aes(x)) + geom_histogram(fill = "plum", color="black")

# Cluster Analysis with mixAK and Fixed K
library(mixAK)
m1 <- NMixMCMC(y0 = data$x,
               nMCMC = c(burn = 1000, keep=1000, thin = 1, info = 100),
               prior = list(priorK="fixed", Kmax=3))
# Inspect the marginal plot of m1

pdens1 <- NMixPredDensMarg(m1[[1]], lgrid=50)
plot(pdens1)

# Inspect estimates for the components
print("Posterior Means for Mu")
print(c(m1[[1]]$poster.mean.mu*sd(data$x)+mean(data$x)))

print("Posterior means for standard deviations:")
print(sqrt(c(unlist(m1[[1]]$poster.mean.Sigma)))*sd(d$x))

# Estimated Frequency for each class
print(round(m1[[1]]$poster.mean.w,2))
