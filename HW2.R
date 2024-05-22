# Assignment_2 Soudabeh Rafieisakhaei
# Problem 3
## Create a vector of the values of the random variable, x
x <- c(0,35000)

## Create a vector of the probabilities, P(x)
prob <- c(5/6, 1/6)

## Expected Value
exp_value <- sum(x*prob)
formatted_exp_value <- format(round(exp_value, 2), nsmall = 2)

## Display the values
exp_value
formatted_exp_value

## Variance using the shortcut formula 
x_sq <- x^2
var <- sum(x_sq*prob) - exp_value^2
round(var, 2)

sprintf("The mean is %s and the variance is %s.", formatted_exp_value, round(var, 2))

std_dev<-sqrt (var)
std_dev

# 32% of U.S. adults say they are more likely to make purchases during a sales tax holiday. You randomly select 10 adults.
# Find the probability that the number of adults who say they are more likely to make purchases during a sales tax holiday is​
# (a) exactly​ two, (b) more than​ two, and​ (c) between two and​ five, inclusive.
#problem6
round(dbinom(2, size = 10,prob = 0.32),digits=3)
round(pbinom(2, size = 10, prob = 0.32, lower.tail = FALSE),digits = 3)
round(sum(dbinom(2:5, size=10, prob = 0.32)),digits=3)


# A small hotel in a popular resort area has 30 rooms. The hotel manager estimates that 16% of all confirmed reservations are "no-shows." 
#Consequently, the hotel accepts confirmed reservations for as many as 35 rooms. If more confirmed reservations arrive than there are​ rooms,
#the overbooked guests are sent to another hotel and given a complementary dinner. Complete parts a through c below assuming that the hotel
#currently has 35 confirmed reservations.
#problem7
# Find the probability that no customers will be sent to another hotel.
round(pbinom(4, size = 35, prob = 0.16, lower.tail = FALSE),digits = 4)
# Find the probability that exactly 1guest  will be sent to another hotel.
round(dbinom(4, size = 35,prob = 0.16),digits=4)
# Find the probability that 2 or more guests will be sent to another hotel.
round(pbinom(3, size = 35, prob = 0.16, lower.tail = TRUE),digits = 4)

#problem8
round(ppois(0,lambda = 2.73),digits=4)
round(dpois(1,lambda = 2.73),digits=4)
round(ppois(1, lambda = 2.73, lower.tail = FALSE),digits = 4)
round(ppois(2, lambda = 2.73, lower.tail = TRUE),digits = 4)


#problem 9
#a
round(pnorm(24, mean = 23, sd = 3, lower.tail = FALSE),digits = 4)
#b
round(pnorm(21, mean = 23, sd = 3, lower.tail = TRUE) -
  pnorm(15, mean = 23, sd = 3, lower.tail = TRUE),digits = 4)



#problem10
(a) Find the value at risk​ (VaR) for an investment of​ $100,000 at ​%. ​(That is, find out how low the value of this investment could be if the worst ​% of outcomes are ruled​ out.) The investment is expected to grow during the year by ​% with SD ​%. Assume a normal model for the change in value.
​(b) To reduce the VaR to ​$​, how much more expected growth would be​ necessary? Assume that the SD 20% of the growth remains ​%.

mean <- 0.08
sd <- 0.20

## Part a
x <- qnorm(0.05, mean = mean, sd = sd, lower.tail = TRUE)
value <- floor(x*100000)
loss <- format(value, big.mark = ",", scientific = F)

sprintf("The VaR for an investment of $100,000 at 5%% is $%s.", loss)

## Part b
new_VaR <- 12000
new_loss <- 12000/100000
z_20_perc <- qnorm(0.05, mean = 0, sd = 1, lower.tail = TRUE)
mu <- (abs(z_20_perc)*sd - new_loss)*100

sprintf("The expected growth to reduce the VaR to $12,000 is %.3f%%.", mu)

# problem11

n <- 1000
p <- 0.026

round(pbinom(39, n, p, lower.tail = TRUE),digits = 5)

