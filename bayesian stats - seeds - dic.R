library(tidyverse)

set.seed(20215, sample.kind = "Rounding")

data <- data.frame(day = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                   sales = c(50,65,72,63,70)) %>% 
  mutate(a.post = .001+sales, b.post = .001+1, post.mean = a.post/b.post)
data

# Number of samples
n.samp <- 10^4

# Sample the mean
mu.daily <- LL.daily <- array(dim=c(n.samp, 5))

for(i in 1:5){
  mu.daily[,i] <- rgamma(n.samp, data[i,"a.post"], data[i,"b.post"])
  LL.daily[,i] <- dpois(data[i,"sales"], mu.daily[,i], log = T)
}


# Evaluate the likelihood
mean.d.daily <- mean(-2*apply(LL.daily, 1, sum))
mean.d.daily

# Evaluate D.mean
data <- data %>%  mutate(post.prob = dpois(sales, post.mean, log = T))
d.mean.daily <- -2*sum(data$post.prob)



# Evaluate DIC
DIC.daily <- 2*mean.d.daily - d.mean.daily

# Model with no daily variation

data.new <- data.frame(day = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                       sales = c(50,65,72,63,70)) %>%  
  mutate(a.post = .001 + sum(sales), 
         b.post = .001+5, 
         post.mean = a.post/b.post, 
         post.prob = dpois(sales, post.mean, log = T))
data.new
d.mean.constant <- -2*sum(data.new$post.prob)

# evaluate mean.d
a.constant = .001 + sum(data.new$sales)
b.constant = .001 + 5

mu.constant <- LL.constant <- array(dim = c(n.samp, 5))
for(i in 1:5){
  mu.constant[,i] <- rgamma(n.samp, a.constant, b.constant)
  LL.constant[,i] <- dpois(data.new[i,"sales"], mu.constant[,i], log = T)
}
mean.d.constant <- mean(-2*apply(LL.daily,1,sum))
mean.d.constant

DIC.constant <- 2*mean.d.constant - d.mean.constant

DIC.daily; DIC.constant; DIC.daily-DIC.constant
