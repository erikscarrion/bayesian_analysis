###################################
# Monte Carlo Sampling Assessment #
###################################
# Load Dependencies
library(tidyverse)

# consider the function (x^5)*(1-x)^5

# Perform simple monte carlo integration to estimate the area under the curve

# Set the function up
beta.func <- function(x){(x^5)*(1-x)^5}

# Generate data on [0,1]
n.samples <- 10^4

set.seed(12345, sample.kind="Rounding")
X <- runif(n.samples,0,1)

# Plot the Curve
base.df <- data.frame(X = X, y = beta.func(X), group=factor("1"))

beta.curve <- base.df %>% ggplot(aes(X,y,fill=group)) +
  geom_ribbon(aes(x=X,ymax=y),
              ymin = 0,alpha = 0.8) +
  geom_line() +
  ylim(c(0,.001)) +
  scale_color_manual(name="", values = c("1"="green4"), labels=NULL) + 
  labs(title = "Distribution of X")+
  theme(legend.position = "none")
beta.curve

# Generate random data to visualize and determine the number of hits and misses

x1 <- runif(n.samples,0,1)
y1 <- runif(n.samples,0,.001)
f.of.x1 <-beta.func(x1)
under <- (y1 < f.of.x1)*1

hits.df <- data.frame(X = x1, y = y1, under = factor(under))


hit.points <- geom_point(data = hits.df,
                         aes(X,y, color = under),
                         inherit.aes = F)

beta.curve+hit.points


area.estimate = mean(under)*1*.001
area.estimate
mean(under)

# Sample directly from a Beta(5,6) distribution to determine the mean of h(x)=1260X
# 
x1.beta.samp <- rbeta(10000,5,6)/1260
mean(x1.beta.samp)

# Using Importance sampling and the uniform, repeat for the mean of h(x)=1260x

# mean of h(x) = 1260x = mean(accept)
mean.accept <- replicate(1000,{
  x <- runif(1000)
  mean(beta.func(x))
})

