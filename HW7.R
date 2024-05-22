#Soudabeh Rafieisakhaei

#HW 7-8-9-10-11

# HW 7 

## Installing the package to use the readxl function
#install.packages("tidyverse")
library(readxl)

## Installing the ggplot2 package
#install.packages("ggplot2")
library(ggplot2)

library(dplyr)
Profit <- read_excel("/Users/soudabeh/desktop/205/HomeWorks/HW7Data/Profit.xlsx")
Profit

profit <- Profit$ProfitFromSales
profit = profit/1000

accounts <- Profit$NumberOfAccounts

plot(x = accounts, 
     y = profit,
     xlab = "Accounts",
     ylab = "Profit",
     xlim = c(0, 50),
     ylim = c(0,40),
     main = "Plot")
abline(lm(profit ~ accounts, data = Profit), col = "blue")

model <- lm(profit ~ accounts, data = Profit)
model
summary(model)


#what is the gain in profit to the firm of getting agents to open  additional
#accounts in the first 3months?
x<-586.70*19
round(x,2)

#Exclude the data for agents who open 20 or fewer accounts in the first 3 months.
#Does the fit of the least squares line change much?
Profit <- Profit %>%
  filter(NumberOfAccounts > 20)

View(Profit)

profit <- Profit$ProfitFromSales
profit = profit/1000

accounts <- Profit$NumberOfAccounts
model <- lm(profit ~ accounts, data = Profit)
summary(model)

x <- (0.8645 *1000) 
round (x, 2)

#Residual Standard error 
k=length(model$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(model$residuals**2)
n=length(model$residuals)
round((sqrt(SSE/(n-(1+k))))*1000, 2)



## Obtain the residuals
residuals <- resid(model)

## Obtain the fitted values
fits <- fitted(model)

## Testing the assumptions of independence, normality, and constant variance

## Independence assumption - not required because the data are not time-series.

## Constant variance assumption
## Create a residual versus fits plot
plot(x = fits, 
     y = residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Plot of Residuals by fits")
abline(h = 0) ## Optional: add a horizontal line at 0.


Profit <- Profit %>%
  filter(NumberOfAccounts > 20)

View(Profit)

profit <- Profit$ProfitFromSales
profit = profit/1000

accounts <- Profit$NumberOfAccounts
model <- lm(profit ~ accounts, data = Profit)
model
summary(model)

#Question2
Question2 <- read_excel("/Users/soudabeh/desktop/Question2.xlsx")
View(Question2)
company <- Question2$Company
market <- Question2$Market
treasury <- Question2$Treasury
Time <- seq(1:nrow(Question2))

plot(Time,company,type = "l",col ="blue")

plot(Time,market,type = "l",col ="blue")

plot(x = market, 
     y = company,
     xlab = "Engine",
     ylab = "Power",
     xlim = c(-0.15,0.15),
     ylim = c(-0.32,0.32),
     main = "Plot")
model <- lm(company ~ market, data = Question2)
summary(model)
round(0.001542,4)
round( 1.723090 ,4)
round( 0.3233 ,3)
k=length(model$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(model$residuals**2)
n=length(model$residuals)
round((sqrt(SSE/(n-(1+k)))), 4)


x<- 10*1.7231
round(x,3)


companyReturn <- company - treasury
marketReturn <- market - treasury

model <- lm (companyReturn ~ marketReturn)
model

round( 0.004873 ,4)
round( 1.709127 ,4)
round( 1.555859 ,4)
x
# Question 3
Question3 <- read_excel("/Users/soudabeh/desktop/Question3.xlsx")
Question3

engine<- Question3$Engine

power <- Question3$Horsepower

plot(x = engine, 
     y = power,
     xlab = "Engine",
     ylab = "Power",
     xlim = c(0, 8),
     ylim = c(0,550),
     main = "Plot")
abline(lm(power ~ engine, data = Question3), col = "blue")
model <- lm(power ~ engine, data = Question3)
model
summary(model)
#Residual Standard error 
k=length(model$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(model$residuals**2)
n=length(model$residuals)
round((sqrt(SSE/(n-(1+k)))), 2)

residuals <- resid(model)
residuals

fits <- fitted(model)

## Testing the assumptions of independence, normality, and constant variance

## Independence assumption - not required because the data are not time-series.

## Constant variance assumption
## Create a residual versus fits plot
plot(x = fits, 
     y = residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Plot of Residuals by fits")
abline(h = 0) ## Optional: add a horizontal line at 0.

# Question4

Question4 <- read_excel("/Users/soudabeh/desktop/205/HomeWorks/HW7Data/Question4.xlsx")
View(Question4)

Sales <- Question4$Sales
DisplayFeet <- Question4$Display

## Create a scatterplot
plot(x = DisplayFeet, 
     y = Sales,
     xlab = "Shelf feet",
     ylab = "Sales (dollars)",
     xlim = c(0,10),
     ylim = c(0,500),
     main = "Plot of Shelf feet and Sales (dollars")
abline(lm(Sales ~ DisplayFeet)) # Optional. Only if you want to see the line.

## Equation of Sales and Shelf feet
model <- lm(Sales ~ DisplayFeet)
model

## Display the Coefficients information
summary(model)

## Transform the Shelf feet to log(Shelf Feet)
ln_DisplayFeet <- log(DisplayFeet)

## Scatterplot of Ln(DisplayFeet) and Sales
plot(x = ln_DisplayFeet, 
     y = Sales,
     xlab = "Ln(Shelf feet)",
     ylab = "Sales (dollars)",
     xlim = c(0, 3),
     ylim = c(0, 500),
     main = "Plot of Ln(Shelf feet) and Sales (dollars")
abline(lm(Sales ~ ln_DisplayFeet)) # Optional. Only if you want to see the line.


## Equation of Sales and Ln(Shelf feet)
transf_model <- lm(Sales ~ ln_DisplayFeet)
transf_model

## Display the Coefficients information
summary(transf_model)



#Question5
Question5 <- read_excel(("/Users/soudabeh/desktop/205/HomeWorks/HW7Data/Question5.xlsx"))

View(Question5)

Age <- Question5$Age
AskingPrice <- Question5$Price

## Create a scatterplot
plot(x = Age, 
     y = AskingPrice,
     xlab = "Age (years)",
     ylab = "Asking Price ($000)",
     xlim = c(0, 20),
     ylim = c(0, 25),
     main = "Plot of Asking Price and Age")
abline(lm(AskingPrice ~ Age)) # Optional. Only if you want to see the line.

## Equation of Age and Asking Price
model <- lm(AskingPrice ~ Age)
model
round(-0.9764,3)
## Display the Coefficients information
summary(model)

## Residual by X plot
residuals <- resid(model)

## Create a plot
plot(x = Age, 
     y = residuals,
     xlab = "Age (years)",
     ylab = "Residuals",
     xlim = c(0, 20),
     ylim = c(-10, 10),
     main = "Plot of Residuals by Age(years)")
abline(h = 0) ## Optional: add a horizontal line at 0.

## Transform the Age to log(Age)
logAge <- log(Age)

## Scatterplot of Ln(Age) and Asking Price
plot(x = logAge, 
     y = AskingPrice,
     xlab = "Log(Age)",
     ylab = "Asking Price ($000)",
     xlim = c(0, 3),
     ylim = c(0, 25),
     main = "Plot of LogAge and AskingPrice")
abline(lm(AskingPrice ~ logAge)) # Optional. Only if you want to see the line.

## Equation of AskingPrice and log(Age)
transf_model <- lm(AskingPrice ~ logAge)
transf_model

## Display the Coefficients information
summary(transf_model)

## Residual by X plot
transf_residuals <- resid(transf_model)

## Create a plot
plot(x = logAge, 
     y = transf_residuals,
     xlab = "logAge",
     ylab = "Residuals",
     xlim = c(0, 3),
     ylim = c(-3, 3),
     main = "Plot of Residuals by logAge")
abline(h = 0) ## Optional: add a horizontal line at 0.

## Compare the two scatterplots and two residual plots side by side
par(mfrow = c(2, 2))

plot(x = Age, 
     y = AskingPrice,
     xlab = "Age (years)",
     ylab = "Asking Price ($000)",
     xlim = c(0, 20),
     ylim = c(0, 25),
     main = "Plot of Asking Price and Age")
abline(lm(AskingPrice ~ Age)) # Optional. Only if you want to see the line.

plot(x = logAge, 
     y = AskingPrice,
     xlab = "Log(Age)",
     ylab = "Asking Price ($000)",
     xlim = c(0, 3),
     ylim = c(0, 25),
     main = "Plot of LogAge and AskingPrice")
abline(lm(AskingPrice ~ logAge)) # Optional. Only if you want to see the line.

plot(x = Age, 
     y = residuals,
     xlab = "Age (years)",
     ylab = "Residuals",
     xlim = c(0, 20),
     ylim = c(-10, 10),
     main = "Plot of Residuals by Age(years)")
abline(h = 0) ## Optional: add a horizontal line at 0.

plot(x = logAge, 
     y = transf_residuals,
     xlab = "logAge",
     ylab = "Transformed Residuals",
     xlim = c(0, 3),
     ylim = c(-3, 3),
     main = "Plot of Transformed Residuals by logAge")
abline(h = 0) ## Optional: add a horizontal line at 0.

par(mfrow = c(1, 1))

## predictvals function
# Given a model, predict values of yvar from xvar
# This function supports one predictor and one predicted variable
# xrange: If NULL, determine the x range from the model object. If a vector with
#   two numbers, use those as the min and max of the prediction range.
# samples: Number of samples across the x range.
# ...: Further arguments to be passed to predict()
predictvals <- function(model, xvar, yvar, xrange = NULL, samples = 100, ...) {
  
  # If xrange isn't passed in, determine xrange from the models.
  # Different ways of extracting the x range, depending on model type
  if (is.null(xrange)) {
    if (any(class(model) %in% c("lm", "glm")))
      xrange <- range(model$model[[xvar]])
    else if (any(class(model) %in% "loess"))
      xrange <- range(model$x)
  }
  
  newdata <- data.frame(x = seq(xrange[1], xrange[2], length.out = samples))
  names(newdata) <- xvar
  newdata[[yvar]] <- predict(model, newdata = newdata, ...)
  newdata
}

## Create scatterplots of both models on one
linear_model <- lm(AskingPrice ~ Age)
transformed_model <- loess(AskingPrice ~ Age)

linear_model_predicted <- predictvals(linear_model, "Age", "AskingPrice")
transformed_model_predicted <- predictvals(transformed_model, "Age", "AskingPrice")

plot_Solved_Example2 <- ggplot(Question5, aes(x = Age, y = AskingPrice)) +
  geom_point(colour = "grey40")

plot_Solved_Example2 +
  geom_line(data = linear_model_predicted, colour = "blue", size = .8) +
  geom_line(data = transformed_model_predicted, colour = "red", size = .8)

## Compare R-sq and se for both the models to see which one fits the data best
mtx_comp <- matrix(c(round(summary(model)$r.squared, 3), 
                     round(summary(model)$sigma, 3), 
                     round(summary(transf_model)$r.squared, 3), 
                     round(summary(transf_model)$sigma, 3)),
                   byrow = TRUE, nrow = 2)
rownames(mtx_comp) <- c("Linear model", "Transformed model")
colnames(mtx_comp) <- c("R-squared", "se ($000)")

## Display the matrix
mtx_comp

# Question6
Question6 <- read_excel("/Users/soudabeh/desktop/205/HomeWorks/HW7Data/Question6.xlsx")

View(Question6)

Volume <-Question6$Volume
Price <- Question6$Price

## Natural logs
ln_Volume <- log(Volume)
ln_Price <- log(Price)

## Common logs
log_Volume <- log10(Volume)
log_Price <- log10(Price)

## Create the plots
par(mfrow = c(1, 2))

## Scatterplot of Ln(Voume) and Ln(Price)
plot(x = ln_Price, 
     y = ln_Volume,
     xlab = "Log(Price)",
     ylab = "Log(Volume)",
     xlim = c(-0.4, 0.3),
     ylim = c(10, 12),
     main = "Plot of Natural logs")

## Scatterplot of Ln(Voume) and Ln(Price)
plot(x = log_Price, 
     y = log_Volume,
     xlab = "Log(Price)",
     ylab = "Log(Volume)",
     xlim = c(-0.2, 0.2),
     ylim = c(4, 5.5),
     main = "Plot of common logs")

par(mfrow = c(1, 1))

## Estimated regression equations
## Natural logs
ln_model <- lm(ln_Volume ~ ln_Price)
ln_model


## Common logs
log_model <- lm(log_Volume ~ log_Price)
log_model
summary(log_model)

## Store the coefficients for the natural log model
ln_coeff <- coef(ln_model)

## Retrieve the intercept part (not needed, but just for reference)
ln_coeff["(Intercept)"]

## Retrieve the slope part
ln_coeff["ln_Price"]

## Store the coefficients for the common log model
log_coeff <- coef(log_model)

## Retrieve the intercept part (not needed, but just for reference)
log_coeff["(Intercept)"]

## Retrieve the slope part
log_coeff["log_Price"]

## Compare R-sq and se for both the models to see which one fits the data best
mtx_comp <- matrix(c(
  round(ln_coeff["(Intercept)"], 2),
  round(ln_coeff["ln_Price"], 2),
  round(summary(ln_model)$r.squared, 3),
  round(summary(ln_model)$sigma, 3),
  round(log_coeff["(Intercept)"], 2),
  round(log_coeff["log_Price"], 2),
  round(summary(log_model)$r.squared, 3),
  round(summary(log_model)$sigma, 3)
),
byrow = TRUE, nrow = 2)
rownames(mtx_comp) <- c("LN Model", "LOG10 model")
colnames(mtx_comp) <- c("Intercept", "Slope", "R-squared", "se")

## Display the matrix
mtx_comp

## Plot of ln(Volume) versus log10(Volume)
plot(x = log_Volume, 
     y = ln_Volume,
     xlab = "Log(Volume)",
     ylab = "Ln(Volume)",
     xlim = c(4, 5.5),
     ylim = c(10, 12),
     main = "Plot of Ln versus Log")

## Model of ln(Volume) versus log(Volume)
ln_log_model <- lm(ln_Volume ~ log_Volume)
ln_log_model

options(scipen = 999)
## Store the coefficients for the natural log model
ln_log_coeff <- coef(ln_log_model)

## Retrieve the intercept part (not needed, but just for reference)
round(ln_log_coeff["(Intercept)"],2)

## Retrieve the slope part
ln_log_coeff["log_Volume"]
options(scipen = 0)


