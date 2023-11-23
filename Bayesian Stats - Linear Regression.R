# Linear Regression
library(tidyverse)
library(MCMCglmm)


data <- read.csv("howell1.csv", sep =";")
adults <- data %>% filter(age >= 18)

m1 <- MCMCglmm(weight ~ height, data = adults)
summary(m1)

# Set up Priors
priors <- list(B = list(mu = c(0,0), V=c(100^2, 100^2)*diag(2)))
priors

m2 <- MCMCglmm(weight~height, data = adults,
               nitt = 11000, burnin = 1000,
               thin = 10, prior = priors, verbose=F)

summary(m2)

mean(m2$Sol[,2]>0)
plot(m2)

# Posterior Predictive Distribution

new.data <- data.frame(height = seq(130,185,.1),
                       weight = 200)

wgt.mean <- predict(m2, newdata = new.data,
                    type = "response", interval = "confidence")

wgt.ind <- predict(m2, newdata = new.data,
                   type = "response", interval = "prediction")

new.data <- new.data %>%  mutate(wgt_mean.mn = wgt.mean[,1],
                                 wgt_mean.lo = wgt.mean[,2],
                                 wgt_mean.hi = wgt.mean[,3],
                                 wgt_ind.mn = wgt.ind[,1],
                                 wgt_ind.lo = wgt.ind[,2],
                                 wgt_ind.hi = wgt.ind[,3],)

par(mfrow = c(2,2))
new.data %>%  ggplot(aes(x=height)) + 
  # 95% credible confidence bands for the individual responses
  geom_ribbon(aes(ymin = wgt_ind.lo, ymax = wgt_ind.hi),
              fill = "cyan", alpha = 0.8) +
  # 95% credible confidence interval for the mean
  geom_ribbon(aes(ymin = wgt_mean.lo, ymax = wgt_mean.hi),
              fill = "salmon", alpha = 0.8) +
  geom_line(aes(y = wgt_mean.mn), col = "red", size = 1.5)+
  geom_point(data = adults, aes(x=height, y = weight))


# Assessment
# Fit the model weight ~ age 

# default model, no params specified
m.age <- MCMCglmm(weight~age, data = adults)
summary(m.age)
  
# model of weight~height  
m.height <- MCMCglmm(weight~height, data = adults, verbose = F)
summary(m.height)

# compare the 2 models using DIC. Which is better?
dic.age <- m.age$DIC
dic.height <- m.height$DIC
dic.diff <- dic.height-dic.age
dic.diff

dic.age; dic.height; dic.diff

# Plot the credible intervals for the posterior and the mean
height.lo = round(min(adults$height))-7
height.hi = round(max(adults$height))+11

# inspect range (137,179)
height.lo; height.hi

# Generate new data
new.data = data.frame(height = seq(height.lo, height.hi, length.out=100),
                      weight = 200)

# Generate Predictions
preds.mean_height <- predict(m.height,
                         newdata = new.data,
                         type = "response",
                         interval ="confidence",
                         verbose = F)
preds.ind_height <- predict(m.height,
                           newdata = new.data,
                           type = "response",
                           interval ="prediction",
                           verbose = F)

new.data <- new.data %>%  mutate(mean.mean = preds.mean_height[,1],
                                 mean.lo = preds.mean_height[,2],
                                 mean.hi = preds.mean_height[,3],
                                 ind.mean = preds.ind_height[,1],
                                 ind.lo = preds.ind_height[,2],
                                 ind.hi = preds.ind_height[,3])

new.data %>%  ggplot(aes(x = height)) + 
  geom_ribbon(aes(ymin = mean.lo, ymax = mean.hi), fill = "#40B0A6") +
  geom_ribbon(aes(ymin = ind.lo, ymax = ind.hi), fill = "#DC3220", alpha = 0.4) +
  geom_line(aes(y = mean.mean), col = "black", size = 1.25) +
  geom_point(data=adults, aes(height, weight)) + 
  labs(title = "MCMC Model: Weight ~ Height",
       subtitle = "Howell Dataset")
