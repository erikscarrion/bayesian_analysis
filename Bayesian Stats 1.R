# Bayesian Statistics - Part 1

# King's Dagger Problem
set.seed(20202020, sample.kind="Rounding")
B = 10^5
boxes <- sample(c("GG", "SS", "GS"),
                size = B, 
                replace = T,
                prob = c(1/3, 1/3, 1/3))
table(boxes)/B

box_fact <- as.factor(boxes)
levels(box_fact) <- c("GG", "SS", "GS")
as.numeric(box_fact[1:5])
box.fact.n <- as.numeric(box_fact)

dagger <- rbinom(B, 1, c(1,0,0.5))[box.fact.n]

# King's Gift - Inverse Probability
# Once the king has presented the box to the guest, and the guest viewed one of the two daggers 
# kept in it, they were invited to guess whether the remaining unseen dagger was silver or gold.

# What would you have guessed if you have opened the box and seen the gold dagger?

dtf <- data.frame(boxes = boxes, dagger = dagger)
head(dtf)


the.other.dagger <- function(box,dagger){
  1*(box=='GG')+0*(box=='SS')+(1-dagger)*(box=='GS')
}

dtf$other <- the.other.dagger(dtf$boxes, dtf$dagger)

prop.table(table(dtf$other))

# Q1
p_barista <- 5/7
p_trainee <- 2/7
barista_probs <- c(excellent = 0.80,
                 good = 0.15,
                 soso = 0.05)
trainee_probs <- c(excellent = 0.20,
             good = 0.50,
             soso = 0.30)

# What is the probability a random visitor will get a good coffee?
p_good = barista_probs["good"]*p_barista + trainee_probs["good"]*p_trainee
p_good

# What is the probability the barista is on duty, given your coffee is excellent
p_excellent = barista_probs["excellent"]*p_barista + trainee_probs["excellent"]*p_trainee
barista_probs["excellent"]*p_barista / p_excellent
trainee_probs["excellent"]*p_trainee / p_excellent

# What is the probabilityt that the barista made your coffee given it is so-so

p_soso <- barista_probs["soso"]*p_barista + trainee_probs["soso"]*p_trainee
p_bs <- barista_probs["soso"]*p_barista / p_soso
p_bs

# Repeat your calculations if the second cup of coffee was Excellent. What is the probability 
# that an experienced barista is on duty today now?
p.b.sosoexc <- barista_probs["soso"]*barista_probs["excellent"]*p_barista
p.t.sosoexc <- trainee_probs["soso"]*trainee_probs["excellent"]*p_trainee

p_sosoexc <- p.b.sosoexc+p.t.sosoexc

p_b_sosoexc <- p.b.sosoexc / p_sosoexc

p_b_sosoexc


# Sandwich Shop Example
sales <- c(monday = 50,
           tuesday = 65, 
           wednesday = 72,
           thursday = 63,
           friday = 70)
a = 3
b = 0.02

a.star <- a+sum(sales)
b.star <- b+length(sales)
#posterior mean, upper and lower bounds
a.star/b.star
qgamma(.025, a.star, b.star)
qgamma(.975, a.star,b.star)

# repeat for a= .01, b =.01

a1 = .01
b1 = .01
a1.star = a1+sum(sales)
b1.star = b1+length(sales)
a1.star/b1.star
qgamma(.025, a1.star, b1.star)
qgamma(.975, a1.star,b1.star)

