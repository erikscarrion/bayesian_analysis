# Bayesian Stats - posterior predictive distribution

# Suppose prior = gamma(1020,15)
# suppose generating = poisson(mu)

# Set Seed
set.seed(12345, sample.kind="Rounding")
# Step 1 simulate mu
B = 10^4

mu <- rgamma(B, 1020,15)
# Step 2 Simulate Data Generating Process
x.tilde = rpois(B, mu)
x.df <- data.frame(x = x.tilde)
x.df %>%  ggplot(aes(x)) + geom_histogram(color = "black",fill = 'plum')
mean(x.tilde)
quantile(x.tilde,c(.025,.975))

# Treating each day as different

dtf <- data.frame(Day = factor(c('Monday','Tuesday','Wednesday','Thursday','Friday'),
                               levels=c('Monday','Tuesday','Wednesday','Thursday','Friday')),
                  x = c(50,65,72,63,70)) %>% 
  mutate(post.a = 700+x,
         post.b = 10+1,
         post.mean = round(post.a/post.b, 1),
         lower_ci = round(qgamma(.025, post.a, post.b), 1),
         upper_ci = round(qgamma(.975, post.a, post.b), 1))


dtf %>%  ggplot(aes(Day, post.mean)) + 
  geom_segment(aes(x = Day, xend =Day, y = lower_ci, yend = upper_ci)) +
  labs(title = "Mean Demand") +geom_point()


mu.monday <- rgamma(B, 750, 11)
mu.tuesday <- rgamma(B, 765, 11)
mean(mu.monday<mu.tuesday)


# Repeat above checking if Friday > Thursday
set.seed(12345, sample.kind="Rounding")
mu.friday <- rgamma(B, 770,11)
mu.thursday <- rgamma(B, 763,11)
mean(mu.friday>mu.thursday)

# Suppose we want to know which day has the greatest possible demand
set.seed(12345, sample.kind="Rounding")
mu.post <- array(dim = c(B, 5))
for(j in 1:5){
  mu.post[,j] <- rgamma(B, dtf$post.a[j], dtf$post.b[j])
}
head(mu.post)

# Raw Counts
max.mu <- apply(mu.post, 1, which.max)
prop.table(table(max.mu))
head(max.mu)
for(i in 1:5){
  print(mean(max.mu==i))
}
monday.probs <- array(dim = c(4,1))
for(i in 2:5){
  monday.probs[i-1,1] <- mean(mu.post[,1] < mu.post[,i])
}

# What day is expected to have the lowest demands
min.mu <- apply(mu.post, 1, which.min)
prop.table(table(min.mu))
