
library(readxl)

library(ggplot2)

library(lmtest)

# Question3

Question3 <- read_excel("/Users/soudabeh/desktop/Question3.xlsx")

View(Question3)

Sales <- Question3$Sales
Volume <- Question3$Volume
Washes <- Question3$Washes

## Create a scatterplot of Sales and Volume
plot(x = Volume, 
     y = Sales,
     xlab = "Volume",
     ylab = "Sales",
     main = "Plot of Volume and Sales",
     xlim = c(2065, 5333),
     ylim = c(1895, 3199))

## Create a scatterplot of Sales and Washes
plot(x = Washes, 
     y = Sales,
     xlab = "Washes",
     ylab = "Sales",
     main = "Plot of Washes and Sales",
     xlim = c(0, 664),
     ylim = c(1895,3199))

## Create a scatterplot of Washes and Volume
plot(x = Volume, 
     y = Washes,
     xlab = "Volume",
     ylab = "Washes",
     main = "Plot of Washes and Volume",
     xlim = c(2736, 6864),
     ylim = c(0, 842))

## Create a correlation matrix in R
cor_mtx <- cor(Question3)
round(cor_mtx, 3)

## Fit the multiple regression model
GasConv_Model <- lm(Sales ~ Volume + Washes, data = Question3)

options(scipen = 999) ## Turn off scientific notation
anova(GasConv_Model)
summary(GasConv_Model)
options(scipen = 0) ## Turn on scientific notation

### Retrieve the values individually
GasConv_Model_Summary <- summary(GasConv_Model)

## R-square and standard error
round(GasConv_Model_Summary$r.squared, 3)
round(GasConv_Model_Summary$sigma, 3)

## Values for the intercept
## Estimate
round(GasConv_Model_Summary$coefficients[1,1], 0)
## Std error
round(GasConv_Model_Summary$coefficients[1,2], 0)
## t-Ratio
round(GasConv_Model_Summary$coefficients[1,3], 2)
## p-value
round(GasConv_Model_Summary$coefficients[1,4], 3)

## Values for the Volume (Gallons)
## Estimate
round(GasConv_Model_Summary$coefficients[2,1], 3)
## Std error
round(GasConv_Model_Summary$coefficients[2,2], 3)
## t-Ratio
round(GasConv_Model_Summary$coefficients[2,3], 2)
## p-value
round(GasConv_Model_Summary$coefficients[2,4], 3)

## Values for the Car Washes
## Estimate
round(GasConv_Model_Summary$coefficients[3,1], 3)
## Std error
round(GasConv_Model_Summary$coefficients[3,2], 3)
## t-Ratio
round(GasConv_Model_Summary$coefficients[3,3], 2)
## p-value
round(GasConv_Model_Summary$coefficients[3,4], 3)

## Checking the conditions for the MRM
## Obtain the residuals
residuals <- resid(GasConv_Model)

## Obtain the fits
fits <- fitted(GasConv_Model)

## Constant variance condition
plot(x = fits, 
     y = residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Plot of Residuals by fits")
abline(h = 0) ## Optional: add a horizontal line at 0.

## Normality condition
hist(residuals)

## Normal quantile plot
qqnorm(residuals)
qqline(residuals)

## Independence condition
## Independence
Time <- seq(1:nrow(Question3))

## Create the line plot of Company_Return vs. Time
plot(Time, residuals, type = "l", col = "blue",
     xlab = "Time", ylab = "Residuals",
     main = "Time plot of residuals")
abline(h = 0) ## Optional: add a horizontal line at 0.

## Calculate the DW statistic
dwtest(GasConv_Model)

DWTestResults <- dwtest(GasConv_Model)

## Retrieve the DW statistic (rounded to two decimals)
round(DWTestResults$statistic, 2)

## Retrieve the DW p-value (rounded to three decimals)
round(DWTestResults$p.value, 3)

## Display the summary table for the model
options(scipen = 999) ## Turn off scientific notation
summary(GasConv_Model)
options(scipen = 0) ## Turn on scientific notation

### 95% confidence interval for partial slope coeff. Age.
confint(GasConv_Model, 'Washes', level=0.95)

## Display 95% CI for slope
lcl <- confint(GasConv_Model, "Washes", level = 0.95)[1]
lcl <- round(lcl, 2)

ucl <- confint(GasConv_Model, "Washes", level = 0.95)[2]
ucl <- round(ucl, 2)

sprintf("The 95%% CI for the Washes is [%4.2f dollars per car wash, %4.2f dollars per car wash].", lcl, ucl)

# question4

Question4 <- read_excel("/Users/soudabeh/desktop/Question4.xlsx")

View(Question4)

Share <- Question4$Share
Detail <- Question4$`Detail (x1)`
Sample <- Question4$`Sample (x2)`

## Plot of Share versus Detail
plot(x = Detail, 
     y = Share,
     xlab = "Detail",
     ylab = "Share",
     main = "Plot of Share and Detail",
     xlim = c(0, 0.2),
     ylim = c(0.21, 0.25))

## Plot of Share versus Sample
plot(x = Sample, 
     y = Share,
     xlab = "Sample",
     ylab = "Share",
     main = "Plot of Share and Sample",
     xlim = c(0, 1),
     ylim = c(0.21, 0.25))

## Plot of Detail versus Sample
plot(x = Sample, 
     y = Detail,
     xlab = "Sample",
     ylab = "Detail",
     main = "Plot of Detail and Sample",
     xlim = c(0, 1),
     ylim = c(0, 0.2))

## Fit the multiple regression model
PromoSpending_Model <- lm(Share ~ Detail + Sample, data = Question4)

options(scipen = 999) ## Turn off scientific notation
anova(PromoSpending_Model)
summary(PromoSpending_Model)
options(scipen = 0) ## Turn on scientific notation

### Retrieve the values individually
PromoSpending_Model_Summary <- summary(PromoSpending_Model)

## R-square and standard error
round(PromoSpending_Model_Summary$r.squared, 4)
round(PromoSpending_Model_Summary$sigma, 4)

## Values for the intercept
## Estimate
round(PromoSpending_Model_Summary$coefficients[1,1], 4)
## Std error
round(PromoSpending_Model_Summary$coefficients[1,2], 4)
## t-Ratio
round(PromoSpending_Model_Summary$coefficients[1,3], 2)
## p-value
round(PromoSpending_Model_Summary$coefficients[1,4], 3)

## Values for the Detail
## Estimate
round(PromoSpending_Model_Summary$coefficients[2,1], 4)
## Std error
round(PromoSpending_Model_Summary$coefficients[2,2], 4)
## t-Ratio
round(PromoSpending_Model_Summary$coefficients[2,3], 2)
## p-value
round(PromoSpending_Model_Summary$coefficients[2,4], 3)

#values for sample
## Estimate
round(PromoSpending_Model_Summary$coefficients[3,1], 4)
## Std error
round(PromoSpending_Model_Summary$coefficients[3,2], 4)
## t-Ratio
round(PromoSpending_Model_Summary$coefficients[3,3], 2)
## p-value
round(PromoSpending_Model_Summary$coefficients[3,4], 3)


## Checking the conditions for the MRM
## Obtain the residuals
residuals <- resid(PromoSpending_Model)

## Obtain the fits
fits <- fitted(PromoSpending_Model)

### Constant variance condition
plot(x = fits, 
     y = residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Plot of Residuals by fits")
abline(h = 0) ## Optional: add a horizontal line at 0.

## Normality condition
hist(residuals)

## Normal quantile plot
qqnorm(residuals)
qqline(residuals)

## Independence condition
## Independence
Time <- seq(1:nrow(SolvedEx4))

## Create the line plot of Company_Return vs. Time
plot(Time, residuals, type = "l", col = "blue",
     xlab = "Time", ylab = "Residuals",
     main = "Time plot of residuals")
abline(h = 0) ## Optional: add a horizontal line at 0.

## Calculate the DW statistic
dwtest(PromoSpending_Model)

DWTestResults <- dwtest(PromoSpending_Model)

## Retrieve the DW statistic (rounded to two decimals)
round(DWTestResults$statistic, 2)

## Retrieve the DW p-value (rounded to three decimals)
round(DWTestResults$p.value, 3)

## F-statistic
round(PromoSpending_Model_Summary$fstatistic[1], 2)

## p-value
round(pf(PromoSpending_Model_Summary$fstatistic[1],
         PromoSpending_Model_Summary$fstatistic[2],
         PromoSpending_Model_Summary$fstatistic[3], 
         lower.tail=F), 3)

## t-statistic for detailing
round(PromoSpending_Model_Summary$coefficients[2,3], 2)

## p-value for detailing
round(PromoSpending_Model_Summary$coefficients[2,4], 3)
