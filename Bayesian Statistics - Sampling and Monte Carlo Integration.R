# Monte Carlo Integration
# Suppose we have a function f(x) = 20*x*(1-x)^3 which models the beta
# 
my.f <- function(x){20*x*(1-x)^3}

f.df <- data.frame(x = seq(0,1, length.out = 150)) %>% mutate(y = my.f(x), group = as.factor("1"))

beta.curve <- f.df %>% ggplot(aes(x, y, group = group, fill=group, shape=group)) + 
  ylim(c(0,3))+
  geom_line(size = 1.2)+
  geom_ribbon(data = subset(f.df, x>0 & x<1),aes(x=x, ymax=y), ymin=0 ,alpha=0.3) +
  scale_fill_manual(name="", values = c("1" = "green4"))
beta.curve
# Now we randomly simulate x and y and ask which of our points "hit" the green area
x <- runif(1000,0,1)
y <- runif(1000,0,3)

# create df and add logical vector to the df indicating if under or not
sim.data <- data.frame(x = x, y = y) %>% mutate(is.under = 1*(y<my.f(x)))

# Plot the points 
beta.curve + geom_point(data = sim.data, aes(x,y, color=factor(is.under)),
                        inherit.aes = F)

# what proportion of the points are under?
prop.under <- mean(sim.data$is.under)
graph.area <- 1*3
AUC.ex <- prop.under * graph.area
AUC.ex

# Assessment
# Using N = 10,100,500,1000 on uniform [0,1]x[0,3]
# And then on [0,1]x[0,6]
N <- c(10,100,500,1000)

area.under.curve <- numeric(length(N))

for(i in 1:length(N)){
  x1 <- runif(N[i], 0, 1)
  y1 <- runif(N[i], 0, 3)
  is.under <- (y1<my.f(x1))*1
  area.under.curve[i] <- mean(is.under)*1*3
}
area.under.curve


# on [0,1]x[0,6]
area.under.curve1 <- numeric(length(N))
for(i in 1:length(N)){
  x2 <- runif(N[i], 0, 1)
  y2 <- runif(N[i], 0, 6)
  is.under2 <- (y2<my.f(x2))*1
  area.under.curve1[i] <- mean(is.under2)*1*6
}
area.under.curve1

auc.df <- data.frame(n = N, 
                     auc1 = area.under.curve, 
                     auc2 = area.under.curve1) %>% 
  mutate(dist.auc1 = abs(auc1-1),
         dist.auc2 = abs(auc2-1))
auc.df %>% ggplot(aes(N,auc1)) + geom_line() + geom_line(aes(y=auc2))


# Monte Carlo Integration Cont'd
# 
area.estimates <- numeric(1000)
for(i in 1:1000){
  g <- runif(1000,0,1)
  f <- runif(1000,0,3)
  is.under <- (f<my.f(g))*1
  area.estimates[i] <- mean(is.under)*1*3
}                  
data.frame(est = area.estimates) %>% ggplot(aes(est)) + geom_histogram(fill="plum", col="black")

estimates.mean <- mean(area.estimates)
estimates.sd <- sd(area.estimates)
estimates.mean; estimates.sd

# we see that our esimated SD is very very close to the actual expected SD
A.true.sd <- sqrt(1*(3-1)/1000)
A.true.sd
A.true.sd - estimates.sd


# Inverse Sampling

# Inverse sampling
lambda <- 2
N <- 1000
u <- runif(N,0,1)

x <- -log(1-u)/lambda

hist(x)

# sample from the actual pdf
x.true <- rexp(1000, 2)
hist(x.true)

# Rejection sampling
# Strongest when the cdf is not easily invertible

# Suppose f(x)=2x for 0<=x<=1 and 0 elsewhere
# Suppose g(x) = 1 for 0<=x<=1 and 0 elsewhere
# P(y==x) = 2x/1 <= 2 = 2x/2*1 = x

# sample u
B <- 10^4
set.seed(12345,  sample.kind="Rounding")


X <- runif(B)

p.accept <- X

u <- runif(B); reject <- (u < p.accept)

# sample
x <- X[reject]

data.frame(x)%>%ggplot(aes(x))+geom_histogram(fill="plum",col="black")

# assessment
# suppose 6x(1-x)/1 <= 3/2
# p(y=x) = 4*x*(1-x)
# 
set.seed(12345,  sample.kind="Rounding")
X <- runif(B)
p.acc <- 4*X*(1-X)
u <- runif(B) ; accept.vec <- (u<p.acc)
sample.x <- X[accept.vec]
y<-X[p.acc <=u]
data.frame(x=sample.x)%>%ggplot(aes(x))+geom_histogram(fill="plum",col="black")
data.frame(x=y)%>%ggplot(aes(y))+geom_histogram(fill="plum",col="black")

# Assessment

my.func <- function(x){
  (x^5)*(1-x)^5
  }
curve.df <- data.frame(x = runif(B)) %>% mutate(y = my.func(x),
                                                group = factor("1"))

beta.curve <- f.df %>% ggplot(aes(x, y, group = group, fill=group, shape=group)) + 
  ylim(c(0,.001))+
  geom_line(size = 1.2)+
  geom_ribbon(data = subset(f.df, x>0 & x<1),aes(x=x, ymax=y), ymin=0 ,alpha=0.3) +
  scale_fill_manual(name="", values = c("1" = "green4"))
beta.curve


# Now generate random data and see how many of the points hit
set.seed(12345,sample.kind = "Rounding")
x.rand <- runif(B)
y.rand <- runif(B)
fx.rand <- my.func(x.rand)
is.under <- (y.rand < fx.rand)*1
hits.df <- data.frame(x=x.rand,y=y.rand,f=fx.rand,under=is.under)

hit.points <- geom_point(data = hits.df,
                         aes(x,y,color=factor(under)),
                         inherit.aes = F)

beta.curve + hit.points


