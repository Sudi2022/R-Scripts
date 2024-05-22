# Assignment 4 Soudabeh Rafieisakhaei
# Problem 6

Class_Size <- c(36,28,31,33,34,42,24,23,28,
                28,33,33,35,30,28,27,31,24)

t.test(Class_Size, alternative = "less", mu = 32)

# Problem 7

Sample_of_Households <- read.table("/Users/soudabeh/Desktop/Question7.csv",
                                   header=FALSE, sep=",")
Sample_of_Households
options(scipen = 999)
t.test(Sample_of_Households, alternative = "less", mu = 3817)
options(scipen = 0)

# Problem 8
# A company that stocks shelves in supermarkets is considering expanding the supply that it delivers.
# Items that are not sold must be discarded at the end of the day, so it only wants to schedule additional deliveries if stores regularly sell out.
# A break-even analysis indicates that an additional delivery cycle will be profitable if items are selling out in more than 63% of markets.
# A survey during the last week in 42 markets found the shelves bare in 31.
# Complete parts a-c.
#A.
#H0: p =< 0.63 vs. Ha: p >0.63
# (b) Describe a Type I error and a Type II error in this context.
# Choose the correct answer below.
# Choosing to schedule additional deliveries when less than 63% of markets regularly sell out is a Type I error.
# Choosing not to schedule additional deliveries when more than 63% of markets regularly sell out is a Type II error.
# c) Find the​ p-value of the test. Do the data reject the null hypothesis if the ​-level is ​?
n    <- 42
phat <- 31/42

p_0  <- 0.63    # hypothesized proportion

z <- (phat - p_0)/sqrt(p_0 * (1-p_0)/n)
z
value <- 1-pnorm(z)
round(value,digits = 4)
# Fail to rejecy H0.
# There is not sufficient evidence that more than 63% of the markets regularly
#sell out.


# Problem 9
#A research center claims that 29% of adults in a certain country would travel into space on a commercial flight if they could afford it.
#In a random sample of 1100  adults in that​ country, 31% say that they would travel into space on a commercial flight if they could afford it.
# At alpha 0.05, is there enough evidence to reject the research​ center's claim?
#Complete parts​ (a) through​ (d) below.
#H0 p= 0.29
# HaNot equal to 0.29


n    <- 1100
phat <- 0.31

p_0  <- 0.29    # hypothesized proportion

z <- (phat - p_0)/sqrt(p_0 * (1-p_0)/n)
z <- round(z,digits = 2)
z
# p-value
value <- 1- pnorm(z)
round(value*2,digits=3)
# Fail to reject the null hypothesis. There is not enough evidence to  reject the research​ center's claim.
