## Installing the package to use the readxl function
#install.packages("tidyverse")
library(readxl)

## Installing the ggplot2 package
#install.packages("ggplot2")
library(ggplot2)

library(dplyr)

# Question1
Question1 <- read_excel("/Users/soudabeh/desktop/Question1.xlsx")
Question1


Size <- Question1$Size
Rent <- Question1$ Rent

## Equation of Rent and Size
model <- lm(Rent ~ Size)
model

## Display the Coefficients information
summary(model)

## Storing in model_summary
model_summary <- summary(model)

## Retrieve the t-value for the slope from summary
round(model_summary$coefficients[2, 3], 2)

## Retrieve the p-value for the slope
options(scipen = 999)
sprintf("%1.3f", model_summary$coefficients[2, 4])
options(scipen = 0)

## Display 95% CI for slope
lcl <- confint(model, "Size", level = 0.95)[1]
round(lcl, 4)

ucl <- confint(model, "Size", level = 0.95)[2]
round(ucl, 4)


#Question2
Question2 <- read_excel("/Users/soudabeh/desktop/Question2.xlsx")

View(Question2)

Size <- Question2$Size
Rent <- Question2$Rent

## Equation of Rent and Size
model <- lm(Rent ~ Size)
model

## Display the Coefficients information
summary(model)

## Display the 90% confidence interval estimate
ci <- predict(model, data.frame(Size = 813), interval = "confidence", level = 0.95)

## Retrieve the lower limit
lcl <- ci[2]
round(lcl, 2)

## Retrieve the upper limit
ucl <- ci[3]
round(ucl, 2)

## Display the 90% prediction interval estimate
pi <- predict(model, data.frame(Size = 813), interval = "prediction", level = 0.95)

## Retrieve the lower limit
lpl <- pi[2]
round(lpl, 2)

## Retrieve the upper limit
upl <- pi[3]
round(upl, 2)

# Question3
Question3 <- read_excel("/Users/soudabeh/desktop/Question3.xlsx")
Question3

CompanyReturn <- Question3$CompanyReturn
MarketReturn <- Question3$MarketReturn

## Equation of CompanyReturn and MarketReturn
model <- lm(CompanyReturn ~ MarketReturn)
model

## Display the Coefficients information
summary(model)

## Storing in model_summary
model_summary <- summary(model)

## Retrieve the t-value for the slope from summary
round(model_summary$coefficients[2, 3], 2)

## Retrieve the p-value for the slope
options(scipen = 999)
sprintf("%1.4f", model_summary$coefficients[2, 4])
options(scipen = 0)

## Display 95% CI for intercept
lcl <- confint(model, "(Intercept)", level = 0.95)[1]
round(lcl, 3)

ucl <- confint(model, "(Intercept)", level = 0.95)[2]
round(ucl, 3)

## Display 95% CI for slope
lcl <- confint(model, "MarketReturn", level = 0.95)[1]
round(lcl, 3)

ucl <- confint(model, "MarketReturn", level = 0.95)[2]
round(ucl, 3)

# Question 4
Question4 <- read_excel("/Users/soudabeh/desktop/Question4.xlsx")

View(Question4)


Profit <- Question4$Profit
Accounts <- Question4$NumberofAccounts

ln_Profit <- log(Profit)
ln_Accounts <- log(Accounts)

## Equation of ln(Profit) and ln(Accounts)
model <- lm(ln_Profit ~ ln_Accounts)
model

## Display the Coefficients information
summary(model)

model_summary <- summary(model) ## to be used later

## Check required conditions

## Linearity
## Create a scatterplot of ln_Profit and ln_Accounts to check for linearity
plot(x = ln_Accounts, 
     y = ln_Profit,
     xlab = "ln_Accounts",
     ylab = "ln_Profits",
     main = "Plot of ln(Profit) and ln(Accounts")
abline(lm(log(Profit) ~ log(Accounts))) # Optional. Only if you want to see the line.

## Obtain the residuals
residuals <- resid(model)

## Obtain the fits
fits <- fitted(model)

## Constant variance
## Create a residual versus fits plot
plot(x = fits, 
     y = residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Plot of Residuals by fits")
abline(h = 0) ## Optional: add a horizontal line at 0.

## Normality
hist(residuals) ## Histogram

## Normal quantile plot
qqnorm(residuals)
qqline(residuals)

## Independence
Time <- seq(1:nrow(Question4))

## Create the line plot of Company_Return vs. Time
plot(Time, residuals, type = "l", col = "blue",
     xlab = "Time", ylab = "Residuals",
     ylim = c(-1.1, 1.1),
     main = "Time plot of residuals")
abline(h = 0) ## Optional: add a horizontal line at 0.

## Elasticity of Profit with respect to the Number of Accounts

#### First, we need to find the t_critical value. We will use qt function in R.
#### Note the degrees of freedom (df) is n - k - 1 where 
#### k = number of predictors, which in this case is 1.

t_critical <- qt(0.025, nrow(Question4)-1-1, lower.tail = FALSE)
t_critical

#### Next, we need to find the t_test_statistic value.
#### t = (b - beta)/std. error

## Retrieve the slope estimate from the summary
slope_est <- model_summary$coefficients[2, 1]

## Retrieve the se of the slope from the summary
slope_se <- model_summary$coefficients[2, 2]

t_test_stat <- (slope_est - 0.35)/slope_se
round(t_test_stat, 2)

## p-value
2*pt(t_test_stat, df = nrow(Question4)-1-1, lower.tail = FALSE)

## Round the p-value to 4 decimals
round(2*pt(t_test_stat, df = nrow(Question4)-1-1, lower.tail = FALSE), 4)

## Multiply the 95% CI for slope by 5 and check if 1 is in the CI
adj_lcl <- 5*confint(model, "ln_Accounts", level = 0.95)[1]

adj_ucl <- 5*confint(model, "ln_Accounts", level = 0.95)[2]

sprintf("The 95%% CI for the slope when number of accounts increases by 5%% is [%4.4f, %4.4f].", adj_lcl, adj_ucl)

## 95% PI for 50 accounts opened
predict(model, data.frame(ln_Accounts = log(50)), interval = "prediction", level = 0.95)

pi <- predict(model, data.frame(ln_Accounts = log(50)), interval = "prediction", level = 0.95)
## Retrieve the lower limit
lpl <- pi[2]
round(exp(lpl), 2)
formatted_lpl <- format(round(as.numeric(exp(lpl)), 2), nsmall=1, big.mark=",")

## Retrieve the upper limit
upl <- pi[3]
round(exp(upl), 2)
formatted_upl <- format(round(as.numeric(exp(upl)), 2), nsmall=1, big.mark=",")

sprintf("The 95%% prediction interval for sales of a rep who opens 50 accounts is [%s, %s] dollars.", formatted_lpl, formatted_upl)


