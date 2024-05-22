# Assignment 3- Soudabeh Rafieisakhaei
# Question 3
#The diameter of a brand of tennis balls is approximately normally distributed,
#with a mean of 2.53 inches and a standard deviation of 0.04inch.
#A random sample of 11  tennis balls is selected. 
# Part_B
# What is the probability that the sample mean is less than 2.51 inches
population_mean <- 2.53
std_dev <- 0.04
n <- 11
x_bar <- 2.51
std_error_of_means <- std_dev/sqrt(n)

Prob_lower_than_2.51 <- pnorm(x_bar,population_mean,
                              std_error_of_means,
                              lower.tail = TRUE)
round(Prob_lower_than_2.51,digits = 4)

# part_C
#What is the probability that the sample mean is between 2.52 and2.54 inches?
  
population_mean <- 2.53
std_dev <- 0.04
n <- 11
x_bar_one <- 2.54
x_bar_two <- 2.52
std_error_of_means <- std_dev/sqrt(n)
Probablity_of_X_bar <- pnorm(x_bar_one,population_mean,std_error_of_means,lower.tail = TRUE)-
  pnorm(x_bar_two,population_mean,std_error_of_means,lower.tail = TRUE)
round(Probablity_of_X_bar,digits = 4)

#part_D
# The probability is 59% that the sample mean will be between what two values
# symmetrically distributed around the population mean?
### Sample mean
mean <- 2.53
### Sample Size
n <- 11
### Sample Std. Dev.
std_dev <- 0.04

# Find the standard error
standard_error <- std_dev / sqrt(n)
alpha = 0.41
degrees_of_freedom = n - 1
t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
margin_error <- t_score * standard_error

# Calculating lower bound and upper bound
lower_bound <- round(mean - margin_error, digits = 2)
upper_bound <- round(mean + margin_error, digits = 2)

# Print the confidence interval
print(c(lower_bound,upper_bound))



# Question4
#According to a report from a business intelligence company, smartphone owners are using an
# average of 19 apps per month. Assume that number of apps used per month by smartphone owners
# is normally distributed and that the standard deviation is 3 .
#Complete parts (a) through (d) below.
# Part a
# a. If you select a random sample of 25 smartphone owners, what is the probability that the
# sample mean is between 18.5 and 19.5 ?
population_mean <- 19
std_dev <- 3
n <- 25
x_bar_one <- 19.5
x_bar_two <- 18.5
std_error_of_means <- std_dev/sqrt(n)
Probablity_of_X_bar <- pnorm(x_bar_one,population_mean,std_error_of_means,lower.tail = TRUE)-
  pnorm(x_bar_two,population_mean,std_error_of_means,lower.tail = TRUE)
round(Probablity_of_X_bar,digits = 3)


# Part b
# b. If you select a random sample of 25 smartphone owners, what is the probability that
#the sample mean is between 18 and 19?

population_mean <- 19
std_dev <- 3
n <- 25
x_bar_one <- 19
x_bar_two <- 18
std_error_of_means <- std_dev/sqrt(n)
Probablity_of_X_bar <- pnorm(x_bar_one,population_mean,std_error_of_means,lower.tail = TRUE)-
  pnorm(x_bar_two,population_mean,std_error_of_means,lower.tail = TRUE)
round(Probablity_of_X_bar,digits = 3)

# Part c
# c. If you select a random sample of 100 smartphone​ owners, what is the probability that
# the sample mean is between 18.5 and 19.5 ?
population_mean <- 19
std_dev <- 3
n <- 100
x_bar_one <- 19.5
x_bar_two <- 18.5
std_error_of_means <- std_dev/sqrt(n)
Probablity_of_X_bar <- pnorm(x_bar_one,population_mean,std_error_of_means,lower.tail = TRUE)-
  pnorm(x_bar_two,population_mean,std_error_of_means,lower.tail = TRUE)
round(Probablity_of_X_bar,digits = 3)

# The sample size in (c) is greater than the sample size in (a),
# so the standard error of the mean (or the standard deviation of the sampling distribution) in(c) is 
# less than in​ (a). 
# As the standard error decreases,values become more concentrated around the mean.
# Therefore, the probability that the sample mean will fall close to the population mean will always 
#increase when the sample size increases.



#Question 7
# Part a
lower_bound <- 13
upper_bound <- 35
constant = 0.453
c(round (lower_bound*constant, digits = 3),round (upper_bound*constant, digits = 3))

# Part b
lower_bound <- 2500
upper_bound <- 4700
constant = 116.3
c(round (lower_bound*constant, digits = 3),round (upper_bound*constant, digits = 3))

# Part c
lower_bound <- 78.90
upper_bound <- 101.54
constant = 25
c(round (lower_bound-constant, digits = 3),round (upper_bound-constant, digits = 3))

#Part D
lower_bound <- 479000
upper_bound <- 724000
constant = 50
c(round (lower_bound/constant, digits = 3),round (upper_bound/constant, digits = 3))

# Question8
# The clothing buyer for a chain of department stores wants to be sure the store orders the right mix of sizes.
# As part of a survey, she measured the height (in inches) of men who bought suits at this store.
# Her software reported the confidence interval shown below. Complete parts a through d.
# With 90.00% confidence, 69.9194 < mu<75.9828

#a. a) Explain carefully what the software output means.
#.     She is 90% confident that the mean height of men who visit this store lies between about 69.9 and 76 inches.

#b (b) What's the margin of error for this interval?
lower_bound <- 69.9194
upper_bound <- 75.9828
margin_error<- (upper_bound -lower_bound)/2
round(margin_error, digits = 1)

#d (d) If the researchers had calculated a 99% confidence nterval, would the output have shown a longer or shorter interval? 
# A 99 % confidence interval requires a longer margin of error. In order to increase confidence, the interval must be longer .

# Question 10
# A sample of 110  calls to a customer help line during the week found callers there were kept waiting on average for 21 minutes with s =7.
# (a) Find the margin for error for this survey if we use a 95% confidence interval for the length of time all customers during this period are kept waiting.
# (b) Interpret for management the margin of error.
# (c) If weneed to be 90% confident, does the confidence interval become wider or narrower?
# (d) Find the 90% confidence interval.

# Part a
### Sample mean
mean <- 21
### Sample Size
n <- 110
### Sample Std. Dev.
std_dev <- 7

# Find the standard error
standard_error <- std_dev / sqrt(n)
alpha = 0.05
degrees_of_freedom = n - 1
t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
margin_error <- t_score * standard_error
round(margin_error, digits = 3)

# part b
# Management can be 95% confident that average waiting time for all callers is within a number of minutes equal
# to the margin of error of the average wait of 21 minutes.

# Partc
# ​(c) The need to only be 95% confident makes the margin of error smaller,
# because the multiplier (or percentile) that comes from wanting 90% percent confidence
# and using a t-distribution (or normal model) to describe the sampling distribution is 
# smaller than the multiplier for 95% interval. Thus, the confidence interval becomes 
# narrower.


# Part d
### Sample mean
mean <- 21
### Sample Size
n <- 110
### Sample Std. Dev.
std_dev <- 7

# Find the standard error
standard_error <- std_dev / sqrt(n)
alpha = 0.1
degrees_of_freedom = n - 1
t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
margin_error <- t_score * standard_error
#round(margin_error, digits = 3)
lower_bound <- round(mean - margin_error, digits = 3)
upper_bound <- round(mean + margin_error, digits = 3)

# Print the confidence interval
print(c(lower_bound,upper_bound))



# Question 11
# Part a
### Sample mean
mean <- 27
### Sample Size
n <- 350
### Sample Std. Dev.
std_dev <- 14

# Find the standard error
standard_error <- std_dev / sqrt(n)
alpha = 0.05
degrees_of_freedom = n - 1
t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
margin_error <- t_score * standard_error
round(margin_error, digits = 2)

# Part d
### Sample mean
mean <- 27
### Sample Size
n <- 350
### Sample Std. Dev.
std_dev <- 14

# Find the standard error
standard_error <- std_dev / sqrt(n)
alpha = 0.01
degrees_of_freedom = n - 1
t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
margin_error <- t_score * standard_error
#round(margin_error, digits = 3)
lower_bound <- round(mean - margin_error, digits = 2)
upper_bound <- round(mean + margin_error, digits = 2)

# Print the confidence interval
print(c(lower_bound,upper_bound))


# Question12
# part a

### Sample mean
mean <- 157
### Sample Size
n <- 38
### Sample Std. Dev.
std_dev <- 36

# Find the standard error
standard_error <- std_dev / sqrt(n)
alpha = 0.05
degrees_of_freedom = n - 1
t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
margin_error <- t_score * standard_error
#round(margin_error, digits = 3)
lower_bound <- round(mean - margin_error, digits = 2)
upper_bound <- round(mean + margin_error, digits = 2)

# Print the confidence interval
print(c(lower_bound,upper_bound))


# Part b
n <- 37
### Sample Proportion
p_hat <- 0.3
### Checking the condition for large sample size...
### Checking for successes and failures >= 5...
n*p_hat >= 5
n*(1 - p_hat) >= 5

### For 95% confidence... 
z95 <- qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE)

### 95% Confidence Interval
lower_bound <- p_hat - (z95 * sqrt(p_hat * (1 - p_hat)/n))
upper_bound <- p_hat + (z95 * sqrt(p_hat * (1 - p_hat)/n))

print(c(round(lower_bound, digits = 3),round(upper_bound, digits = 3)))

# Question 13
# Part a
# Direct mail advertisers send solicitations (junk mail) to thousands of potential customers hoping that some will buy the company's product.
# The response rate is usually quite low. Suppose a company wants to test the response to a new flyer and sends it to  randomly 1000 selected people.
# The company gets orders from 134 of the recipients and decides to do a mass mailing list to everyone on its mailing list of over 200,000.
#Create a 95% confidence interval for the percentage of those people who will order something.
n <- 1000

### Count the number of people who order
num_buying <- 134

### Sample Proportion: Proportion of people preferring product
p_hat <- num_buying/n

### Checking the condition for large sample size...
### Checking for successes and failures >= 5...
n*p_hat >= 5
n*(1 - p_hat) >= 5

### For 95% confidence... 
z95 <- qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE)

### 95% Confidence Interval
lower_bound <- p_hat - (z95 * sqrt(p_hat * (1 - p_hat)/n))
upper_bound <- p_hat + (z95 * sqrt(p_hat * (1 - p_hat)/n))

print(c(round(lower_bound, digits = 3)*100,round(upper_bound, digits = 3)*100))


# Question 14
# Fireman's Fund commissioned an online survey of 1,200  wealthy homeowners to find out what they knew about their insurance coverage.
# Complete parts​ (a) through​ (c) below.
# Part a
# (a) When asked whether they knew the replacement value of their home,62% replied yes. In this case, should
#Fireman's Fund conclude that more than half of wealthy homeowners know the value of their homes?
# Find the 95% confidence interval for the parameter of interest.
n <- 1200
### Sample Proportion
p_hat <- 0.62
### Checking the condition for large sample size...
### Checking for successes and failures >= 5...
n*p_hat >= 15
n*(1 - p_hat) >= 15

### For 95% confidence... 
z95 <- qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE)

### 95% Confidence Interval
lower_bound <- p_hat - (z95 * sqrt(p_hat * (1 - p_hat)/n))
upper_bound <- p_hat + (z95 * sqrt(p_hat * (1 - p_hat)/n))

# The entire confidence interval is greater than 0.5, so we can conclude that
# more than half of wealthy homeowners know the replacement value of their home.

# Part b
print(c(round(lower_bound, digits = 6),round(upper_bound, digits = 6)))


# The entire confidence interval is greater than 0.5, so we can conclude that
# more than half of wealthy homeowners know the replacement value of their home.

# Part c
# (c) The results of the survey were accompanied by the statement, "In theory, with probability samples of this size,
# one could say with 95 percent certainty that the results have a statistical precision of about plus or minus 3 percentage points.
# This online sample was not a probability sample." What is the point of this comment?
# 
# The comment is recognizing that because the sample was online,the respondents chose to respond to the survey and therefore were not randomly selected,
#as in a probability sample.Thus, the theoretical precision may not apply here.



# Question 15
# Part a
### Sample mean
mean <- 37
### Sample Size
n <- 50
### Sample Std. Dev.
std_dev <- 10

# Find the standard error
standard_error <- std_dev / sqrt(n)
alpha = 0.05
degrees_of_freedom = n - 1
t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
margin_error <- t_score * standard_error
#round(margin_error, digits = 3)
lower_bound <- round(mean - margin_error, digits = 2)
upper_bound <- round(mean + margin_error, digits = 2)

# Print the confidence interval
print(c(lower_bound,upper_bound))


# Question 16
# Part a
# In a survey of 3376 adults,1445  say they have started paying bills online in the last year.
# Construct a 99% confidence interval for the population proportion. Interpret the results.
# A )99% confidence interval for the population proportion is 

n <- 3376

### Count the number of people who prefer Crest
num_paying_online <- 1445

### Sample Proportion: Proportion of people preferring Crest
p_hat <- num_paying_online/n

### Checking the condition for large sample size...
### Checking for successes and failures >= 5...
n*p_hat >= 5
n*(1 - p_hat) >= 5

### For 95% confidence... 
z95 <- qnorm(0.995, mean = 0, sd = 1, lower.tail = TRUE)
z95
### 95% Confidence Interval
lower_bound <- p_hat - (z95 * sqrt(p_hat * (1-p_hat)/n))
upper_bound <- p_hat + (z95 * sqrt(p_hat * (1- p_hat)/n))

print(c(round(lower_bound, digits = 3),round(upper_bound, digits = 3)))

