# Soudabeh Rafieisakhaei
# Home work 11

library(readxl)

## Install the car package for VIFs
install.packages("car")

library(car)
################
#Question1######
################
Question1 <- read_excel("/Users/soudabeh/desktop/Question1.xlsx")

View(Question1)

Price <- Question1$`Price ($)`
Length_inch <- Question1$`Length (inch)`
Width_mm <- Question1$`Width (mm)`

## Derived variables
Length_mm <- Length_inch*25.4
Vol_cu_mm <- pi*Length_mm*(Width_mm/2)^2

## Correlation between Volume and Width
round(cor(Vol_cu_mm, Width_mm), 3)

## Multiple regression of Price on Width and Volume
GoldChains_Model <- lm(Price ~ Width_mm + Vol_cu_mm, data = Question1)

options(scipen = 999) ## Turn off scientific notation
anova(GoldChains_Model)
summary(GoldChains_Model)
options(scipen = 0) ## Turn on scientific notation

## Display the VIFs
vif(GoldChains_Model)

## Round to two decimal places
round(vif(GoldChains_Model), 2)

## Interpretation of the coefficient of Volume
## Estimate
GoldChains_Summary <- summary(GoldChains_Model)
round(GoldChains_Summary$coefficients[3,1], 2)

## Interpretation
sprintf("For chains of a given width, the retailer charges about, $%.2f per additional cu. mm.", 
        round(GoldChains_Summary$coefficients[3,1], 2))

## Multiple regression model
sprintf("Price = %.2f + %.2f*Width(mm) + %.2f*Vol_cu_mm", 
        round(GoldChains_Summary$coefficients[1,1], 2),
        round(GoldChains_Summary$coefficients[2,1], 2),
        round(GoldChains_Summary$coefficients[3,1], 2))

## Marginal correlation between Price and Width
round(cor(Price, Width_mm), 2)


##############
# Question2###
##############
Question2 <- read_excel("/Users/soudabeh/desktop/Question2.xlsx")

View(Question2)

CompanyX <- Question2$`Company X Excess Return`
Market <- Question2$`Market Excess Return`
Index <- Question2$`Index Excess Return`
CompanyY <- Question2$`Company Y Excess Return`

## Create the time variable
Time <- seq(1:nrow(Question2))

## Plot Company X Excess Returns versus time
plot(Time, CompanyX, type = "l", col = "blue",
     xlab = "Time", ylab = "Company X Excess Returns",
     main = "Time plot of Company X Excess Returns",
     xlim = c(0, 200),
     ylim = c(-1, 1))
abline(h = 0) ## Optional: add a horizontal line at 0.

## Plot Market Excess Returns versus time
plot(Time, Market, type = "l", col = "blue",
     xlab = "Time", ylab = "Market Excess Returns",
     main = "Time plot of Market Excess Returns",
     xlim = c(0, 200),
     ylim = c(-0.2, 0.2))
abline(h = 0) ## Optional: add a horizontal line at 0.

## Plot Index Excess Returns versus time
plot(Time, Index, type = "l", col = "blue",
     xlab = "Time", ylab = "Index Excess Returns",
     main = "Time plot of Index Excess Returns",
     xlim = c(0, 200),
     ylim = c(-0.2, 0.2))
abline(h = 0) ## Optional: add a horizontal line at 0.

## Plot Company Y Excess Returns versus time
plot(Time, CompanyY, type = "l", col = "blue",
     xlab = "Time", ylab = "CompanyY Excess Returns",
     main = "Time plot of CompanyY Excess Returns",
     xlim = c(0, 200),
     ylim = c(-0.4, 0.4))
abline(h = 0) ## Optional: add a horizontal line at 0.

sprintf("None of the plots show a linear pattern.")

## Run the multiple regression
CompanyX_Model <- lm(CompanyX ~ Market + Index + CompanyY, data = Question2)

options(scipen = 999) ## Turn off scientific notation
anova(CompanyX_Model)
summary(CompanyX_Model)
options(scipen = 0) ## Turn on scientific notation

## Get the coefficients
CompanyX_Model_Summary <- summary(CompanyX_Model)

sprintf("CompanyXReturns = %.4f + %.2f*Market_Excess_Return + %.2f*Index_Excess_Return + %.3f*CompanyY_Excess_Return", 
        CompanyX_Model_Summary$coefficients[1,1],
        CompanyX_Model_Summary$coefficients[2,1],
        CompanyX_Model_Summary$coefficients[3,1],
        CompanyX_Model_Summary$coefficients[4,1])

## Retrieve the F-value and the p-value
## F-statistic
round(CompanyX_Model_Summary$fstatistic[1], 2)

## p-value
sprintf("p-value is %1.3f.", pf(CompanyX_Model_Summary$fstatistic[1],
                                CompanyX_Model_Summary$fstatistic[2],
                                CompanyX_Model_Summary$fstatistic[3], 
                                lower.tail=F))

## Comparing marginal slope and the partial slope for the Index Excess Return

## Partial slope
sprintf("CompanyXReturns = %.4f + %.2f*Market_Excess_Return + %.2f*Index_Excess_Return + %.3f*CompanyY_Excess_Return", 
        CompanyX_Model_Summary$coefficients[1,1],
        CompanyX_Model_Summary$coefficients[2,1],
        CompanyX_Model_Summary$coefficients[3,1],
        CompanyX_Model_Summary$coefficients[4,1])

## Run the simple regression
CompanyX_SRM_Model <- lm(CompanyX ~ Index, data = Question2)

options(scipen = 999) ## Turn off scientific notation
anova(CompanyX_SRM_Model)
summary(CompanyX_SRM_Model)
options(scipen = 0) ## Turn on scientific notation

CompanyX_SRM_Model_Summary <- summary(CompanyX_SRM_Model)

## Marginal slope
sprintf("CompanyXReturns = %.4f + %.2f*Index_Excess_Return", 
        CompanyX_SRM_Model_Summary$coefficients[1,1],
        CompanyX_SRM_Model_Summary$coefficients[2,1])

## Partial slope again
sprintf("CompanyXReturns = %.4f + %.2f*Market_Excess_Return + %.2f*Index_Excess_Return + %.3f*CompanyY_Excess_Return", 
        CompanyX_Model_Summary$coefficients[1,1],
        CompanyX_Model_Summary$coefficients[2,1],
        CompanyX_Model_Summary$coefficients[3,1],
        CompanyX_Model_Summary$coefficients[4,1])

## Display the VIF for Index Excess Returns
round(vif(CompanyX_Model), 1)

## Final model addressing the effect if collinearity
## Run the multiple regression
CompanyX_Final_Model <- lm(CompanyX ~ Market + CompanyY, data = Question2)

options(scipen = 999) ## Turn off scientific notation
anova(CompanyX_Final_Model)
summary(CompanyX_Final_Model)
options(scipen = 0) ## Turn on scientific notation

## Get the coefficients
CompanyX_Final_Model_Summary <- summary(CompanyX_Final_Model)

sprintf("CompanyXReturns = %.4f + %.2f*Market_Excess_Return + %.3f*CompanyY_Excess_Return", 
        CompanyX_Final_Model_Summary$coefficients[1,1],
        CompanyX_Final_Model_Summary$coefficients[2,1],
        CompanyX_Final_Model_Summary$coefficients[3,1])

summary(CompanyX_Final_Model)

## Examine the VIF in this new model
vif(CompanyX_Final_Model)

################
# Question 4 ####
################

Question4 <- read_excel("/Users/soudabeh/desktop/Question4.xlsx")

View(Question4)

str(Question4) ## View the structure

## Create vectors for each variable
End_of_Training <- Question4$End_of_Training
Proficiency <- Question4$Proficiency
Method <- Question4$Method

## Create dummy variables
Classroom <- ifelse(Question4$Method == 'classroom', 1, 0)
Online <- ifelse(Question4$Method == 'online', 1, 0)
App <- ifelse(Question4$Method == 'courseware app', 1, 0)

## Add the dummy variables to the dataframe
Question4$Classroom <- Classroom
Question4$Online <- Online
Question4$App <- App

## View the dataframe again
View(Question4)

## Multiple regression
## Dummies used: Classroom and Online
## Base or the reference: App
Training_Model <- lm(End_of_Training ~ Proficiency + Classroom + Online, data = Question4)

options(scipen = 999) ## Turn off scientific notation
anova(Training_Model)
summary(Training_Model)
options(scipen = 0) ## Turn on scientific notation

## Retrieve the coefficients rounded to three decimals
Training_Model_Summary <- summary(Training_Model)

sprintf("End_of_Training = %.3f + %.3f*Proficiency + %.3f*Classroom + %.3f*Online", 
        Training_Model_Summary$coefficients[1,1],
        Training_Model_Summary$coefficients[2,1],
        Training_Model_Summary$coefficients[3,1],
        Training_Model_Summary$coefficients[4,1])

## Interpretation of coefficients
sprintf("Taking into account the effect of the training method, for each additional point scored on the proficiency exam, the predicted end-of-training exam score is estimated to increase by %.3f points. Holding constant the score on the proficiency exam, the estimated effect of an underwriter being trained by the classroom method rather than the courseware app method or the online method is to increase the end-of-training exam score by %.3f points. Holding constant the score on the proficiency exam, the estimated effect of an underwriter being trained by the online method rather than the courseware app method or the classroom method is to increase the end-of-training exam score by %.3f points.", 
        Training_Model_Summary$coefficients[2,1],
        Training_Model_Summary$coefficients[3,1],
        Training_Model_Summary$coefficients[4,1])

## Predict the mean end-of-training exam score for a student with a 
## proficiency exam score of 102 who had classroom-based training.


## Residual analysis
## Obtain the residuals
residuals <- resid(Training_Model)

## Residual plot - Residuals versus End of Training
plot(x = End_of_Training, 
     y = residuals,
     xlab = "End of Training",
     ylab = "Residuals",
     main = "Plot of Residuals and End of Training",
     xlim = c(0, 100),
     ylim = c(-22, 22))

## Obtain the F-statistic and the p-value
## F-statistic
round(Training_Model_Summary$fstatistic[1], 2)

## p-value
sprintf("p-value is %1.3f.", pf(Training_Model_Summary$fstatistic[1],
                                Training_Model_Summary$fstatistic[2],
                                Training_Model_Summary$fstatistic[3], 
                                lower.tail=F))

## Model: 
## End_of_Training = beta0 + beta1*Proficiency + beta2*Classroom + beta3*Online + epsilon

## Display the summary
Training_Model_Summary

## Test of beta1 (Proficiency)
## t-value
round(Training_Model_Summary$coefficients[2,3], 2)

## p-value
options(scipen = 999) ## Turn off scientific notation
format(round(Training_Model_Summary$coefficients[2,4], 3), nsmall = 3)
options(scipen = 0) ## Turn on scientific notation

## Test of beta2 (Classroom)
## t-value
format(round(Training_Model_Summary$coefficients[3,3], 2), nsmall = 2)

## p-value
options(scipen = 999) ## Turn off scientific notation
format(round(Training_Model_Summary$coefficients[3,4], 3), nsmall = 3)
options(scipen = 0) ## Turn on scientific notation

## Test of beta3 (Online)
## t-value
format(round(Training_Model_Summary$coefficients[4,3], 2), nsmall = 2)

## p-value
options(scipen = 999) ## Turn off scientific notation
format(round(Training_Model_Summary$coefficients[4,4], 3), nsmall = 3)
options(scipen = 0) ## Turn on scientific notation

## 95% confidence interval estimate of the population slope for the relationship 
## between the end-of-training exam score and the proficiency exam score.
## Display 95% CI for slope
lcl <- confint(Training_Model, "Proficiency", level = 0.95)[1]
lcl <- round(lcl, 3)

ucl <- confint(Training_Model, "Proficiency", level = 0.95)[2]
ucl <- round(ucl, 3)

sprintf("The 95%% CI for the Proficiency is [%4.3f points, %4.3f points].", lcl, ucl)

## Interpretation:
sprintf("Taking into account the effect of the training method, for each additional point scored on the proficiency exam, the predicted end-of-training exam score is estimated to increase by approximately %4.3f and %4.3f points.", lcl, ucl)

## 95% confidence interval estimate of the population slope for the relationship between the end-of-training exam score and type of training method.

## 95% CI for dummy variable Classroom
lcl <- confint(Training_Model, "Classroom", level = 0.95)[1]
lcl <- round(lcl, 3)

ucl <- confint(Training_Model, "Classroom", level = 0.95)[2]
ucl <- round(ucl, 3)

sprintf("The 95%% CI for the Classroom is [%4.3f points, %4.3f points].", lcl, ucl)

## Interpretation:
sprintf("Taking into account the other variables, for classroom training, the predicted end-of-training exam score is estimated to decrease by approximately %4.3f and %4.3f points.", lcl, ucl)

## 95% CI for dummy variable Online
lcl <- confint(Training_Model, "Online", level = 0.95)[1]
lcl <- round(lcl, 3)

ucl <- confint(Training_Model, "Online", level = 0.95)[2]
ucl <- round(ucl, 3)

sprintf("The 95%% CI for the Online training is [%4.3f points, %4.3f points].", lcl, ucl)

## Interpretation:
sprintf("Taking into account the other variables, for online training, the predicted end-of-training exam score is estimated to change by approximately %4.3f and %4.3f points.", lcl, ucl)

## Adjusted R-square
round(Training_Model_Summary$adj.r.squared, 3)

## Coefficients of partial determination
## r_squareY1.23
## Proportion of variation in Y explained by X1 (Proficiency) which is not explained by X2 (classroom)
## and X3 (Online)

### FULL MODEL: Contains ALL the independent variables.
### In this case, we have three - Proficiency, Classroom, and Online.
fullmodel <- lm(End_of_Training ~ Proficiency + Classroom + Online, data = Question4)

### REDUCED MODEL: Removes the independent variables to be tested.
### In this case, we have one independent variable to be tested 
### Proficiency.
reducedmodel <- lm(End_of_Training ~ Classroom + Online, data = Question4)

f <- anova(fullmodel)
r <- anova(reducedmodel)
f
r
### Retrieving the SSEs for the full and the reduced models
SSE_F <- f$`Sum Sq`[4]
SSE_R <- r$`Sum Sq`[3]

## Calculation of Partial R-square
round(((SSE_R-SSE_F)/SSE_R), 3)

## Interpretation
sprintf("For given value(s) of X2 (Classroom) and X3 (Online), %.1f%% of the variation in Y (End of training) is explained by the variation in X1 (Proficiency).", round(((SSE_R-SSE_F)/SSE_R)*100, 1)) 

## Coefficients of partial determination
## r_squareY2.13
## Proportion of variation in Y explained by X2 (Classroom) which is not explained by X1 (Proficiency)
## and X3 (Online)

### FULL MODEL: Contains ALL the independent variables.
### In this case, we have three - Proficiency, Classroom, and Online.
fullmodel <- lm(End_of_Training ~ Proficiency + Classroom + Online, data = Question4)

### REDUCED MODEL: Removes the independent variables to be tested.
### In this case, we have one independent variable to be tested 
### Classroom.
reducedmodel <- lm(End_of_Training ~ Proficiency + Online, data = Question4)

f <- anova(fullmodel)
r <- anova(reducedmodel)

### Retrieving the SSEs for the full and the reduced models
SSE_F <- f$`Sum Sq`[4]
SSE_R <- r$`Sum Sq`[3]

## Calculation of Partial R-square
round(((SSE_R-SSE_F)/SSE_R), 3)

## Interpretation
sprintf("For given value(s) of X1 (Proficiency) and X3 (Online), %.1f%% of the variation in Y (End of training) is explained by the variation in X2 (Classroom).", round(((SSE_R-SSE_F)/SSE_R)*100, 1)) 

## Coefficients of partial determination
## r_squareY3.12
## Proportion of variation in Y explained by X3 (Online) which is not explained by X1 (Proficiency)
## and X3 (Classroom)

### FULL MODEL: Contains ALL the independent variables.
### In this case, we have three - Proficiency, Classroom, and Online.
fullmodel <- lm(End_of_Training ~ Proficiency + Classroom + Online, data = Question4)

### REDUCED MODEL: Removes the independent variables to be tested.
### In this case, we have one independent variable to be tested 
### Online.
reducedmodel <- lm(End_of_Training ~ Proficiency + Classroom, data = Question4)

f <- anova(fullmodel)
r <- anova(reducedmodel)

### Retrieving the SSEs for the full and the reduced models
SSE_F <- f$`Sum Sq`[4]
SSE_R <- r$`Sum Sq`[3]

## Calculation of Partial R-square
round(((SSE_R-SSE_F)/SSE_R), 3)

## Interpretation
sprintf("For given value(s) of X1 (Proficiency) and X2 (Classroom), %.1f%% of the variation in Y (End of training) is explained by the variation in X3 (Online).", round(((SSE_R-SSE_F)/SSE_R)*100, 1)) 

## Interaction variables
## X1 = Proficiency
## X2 = Classroom
## X3 = Online
## X4 = X1*X2
## X5 = X1*X3

## Run a multiple regression with interaction variables.
Training_Model_With_Interaction <- lm(End_of_Training ~ Proficiency + Classroom + Online + 
                                        Proficiency:Classroom + 
                                        Proficiency:Online, data = Question4)

options(scipen = 999) ## Turn off scientific notation
anova(Training_Model_With_Interaction)
summary(Training_Model_With_Interaction)
options(scipen = 0) ## Turn on scientific notation

## Retrieve the coefficients rounded to three decimals
Training_Model_With_Interaction_Summary <- summary(Training_Model_With_Interaction)

sprintf("End_of_Training = %.3f + %.3f*Proficiency + %.3f*Classroom + %.3f*Online +  %.3f*Proficiency*Classroom + %.3f*Proficiency*Online", 
        Training_Model_With_Interaction_Summary$coefficients[1,1],
        Training_Model_With_Interaction_Summary$coefficients[2,1],
        Training_Model_With_Interaction_Summary$coefficients[3,1],
        Training_Model_With_Interaction_Summary$coefficients[4,1],
        Training_Model_With_Interaction_Summary$coefficients[5,1],
        Training_Model_With_Interaction_Summary$coefficients[6,1])

## Partial F-test to see if the interaction variables are significant
## End_of_Training = beta0 + beta1*Proficiency + beta2*Classroom + beta3*Online 
## + beta4*Proficiency*Classtoom + beta5*Proficiency*Online + epsilon

## Test of beta4 and beta5

## Full model: All independent variables
fullmodel <- lm(End_of_Training ~ Proficiency + Classroom + Online + Proficiency*Classroom
                + Proficiency*Online, data = Question4)

## Reduced model: Proficiency, Classroom, Online
reducedmodel <- lm(End_of_Training ~ Proficiency + Classroom + Online, data = Question4)

f <- anova(fullmodel)
r <- anova(reducedmodel)

### Retrieving the SSEs for the full and the reduced models
SSE_F <- f$`Sum Sq`[6]
SSE_R <- r$`Sum Sq`[4]
MSE_F <- f$`Mean Sq`[6]

Num <- (SSE_R - SSE_F)/2
Den <- MSE_F

### Calculating the Partial F-statistic
Partial_F_stat <- Num/Den

### Partial F-statistic (rounded to 2 decimals)
round(Partial_F_stat, 2)

### Calculating the p-value
pf(Partial_F_stat, 2, nrow(Question4)-5-1, lower.tail = FALSE)

## p-value (rounded to 2 decimals)
round(pf(Partial_F_stat, 2, nrow(Question4)-5-1, lower.tail = FALSE), 3)

### Conclusion and Interpretation
### The p-value from the ANOVA output leads to the non-rejection of H0
### H0: The reduced model and the full model do not differ significantly, so choose the reduced model.
### H1: The full model is significantly better.
### We conclude that the reduced model is better, so we infer at any reasonable significance level
### that the interaction variables do not contribute significantly to explain the variation in End of Training.
### Choose the reduced model without interaction variables.


################
# Question5 ####
################

Question5 <- read_excel("/Users/soudabeh/desktop/Question5.xlsx")

View(Question5)

str(Question5) ## View the structure

## Create vectors for each variable
Alcohol <- Question5$Alcohol
Type_of_Wine <- Question5$TypeofWine
Quality <- Question5$Quality


## Multiple regression
## Dummies used: Type_of_Wine
## Base or the reference: White
Training_Model<- lm(Quality ~ Alcohol + Type_of_Wine , data = Question5)

options(scipen = 999) ## Turn off scientific notation
anova(Training_Model)
summary(Training_Model)
options(scipen = 0) ## Turn on scientific notation

## Retrieve the coefficients rounded to three decimals
Training_Model_Summary <- summary(Training_Model)

sprintf("Quality = %.3f + %.3f*Alcohol + %.3f*Type_of_Wine", 
        Training_Model_Summary$coefficients[1,1],
        Training_Model_Summary$coefficients[2,1],
        Training_Model_Summary$coefficients[3,1])
        
summary(Training_Model)


# Predict the mean quality for a red wine that has 10% alcohol.
# Construct a 0.95% confidence interval estimate and a  0.95% prediction interval.

round(predict(Training_Model,data.frame(Alcohol = 10, Type_of_Wine =1)),3)

## Display the 95% confidence interval estimate
ci <- predict(Training_Model, data.frame(Alcohol = 10,Type_of_Wine =1), interval = "confidence", level = 0.95)

## Retrieve the lower limit
lcl <- ci[2]
round(lcl, 3)

## Retrieve the upper limit
ucl <- ci[3]
round(ucl, 3)

## Display the 95% prediction interval estimate
pi <- predict(Training_Model, data.frame(Alcohol = 10,Type_of_Wine =1), interval = "prediction", level = 0.95)

## Retrieve the lower limit
lpl <- pi[2]
round(lpl, 3)

## Retrieve the upper limit
upl <- pi[3]
round(upl, 3)

## Residual analysis
## Obtain the residuals
residuals <- resid(Training_Model)

## Residual plot - Residuals versus prediction
plot(x = Quality, 
     y = residuals,
     xlab = "prediction",
     ylab = "Residuals",
     main = "Plot of Residuals and End of Training",
     xlim = c(2, 10),
     ylim = c(-4,4))

## Residual plot - Residuals versus Alcohol
plot(x = Alcohol, 
     y = residuals,
     xlab = "Alcohol",
     ylab = "Residuals",
     main = "Plot of Residuals and End of Training",
     xlim = c(8, 15),
     ylim = c(-4, 4))

## Residual plot - Residuals versus Type_of_Wine
plot(x = Type_of_Wine, 
     y = residuals,
     xlab = "Type_of_Wine",
     ylab = "Residuals",
     main = "Plot of Residuals and End of Training",
     xlim = c(0, 1),
     ylim = c(-4, 4))

## Obtain the F-statistic and the p-value
## F-statistic
round(Training_Model_Summary$fstatistic[1], 2)

## p-value
sprintf("p-value is %1.3f.", pf(Training_Model_Summary$fstatistic[1],
                                Training_Model_Summary$fstatistic[2],
                                Training_Model_Summary$fstatistic[3], 
                                lower.tail=F))

## Model: 

## Display the summary
Training_Model_Summary

## Test of beta1 (Alcohol)
## t-value
round(Training_Model_Summary$coefficients[2,3], 2)

## p-value
options(scipen = 999) ## Turn off scientific notation
format(round(Training_Model_Summary$coefficients[2,4], 3), nsmall = 3)
options(scipen = 0) ## Turn on scientific notation

## Test of beta2 (Type_of_Wine)
## t-value
format(round(Training_Model_Summary$coefficients[3,3], 2), nsmall = 2)

## p-value
options(scipen = 999) ## Turn off scientific notation
format(round(Training_Model_Summary$coefficients[3,4], 3), nsmall = 3)
options(scipen = 0) ## Turn on scientific notation


## 95% confidence interval estimate of the population slope for the relationship 
## between the wine quality and the percentage of alcohol.
## Display 95% CI for slope
lcl <- confint(Training_Model, "Alcohol", level = 0.95)[1]
lcl <- round(lcl, 3)

ucl <- confint(Training_Model, "Alcohol", level = 0.95)[2]
ucl <- round(ucl, 3)

sprintf("The 95%% CI for alcohol is [%4.3f points, %4.3f points].", lcl, ucl)


## 95% confidence interval estimate of the population slope for the relationship between
# the wine quality and type of wine.

## 95% CI for dummy variable Classroom
lcl <- confint(Training_Model, "Type_of_Wine", level = 0.95)[1]
lcl <- round(lcl, 3)

ucl <- confint(Training_Model, "Type_of_Wine", level = 0.95)[2]
ucl <- round(ucl, 3)

sprintf("The 95%% CI for the Type_of_Wine is [%4.3f points, %4.3f points].", lcl, ucl)


## R-square
round(Training_Model_Summary$r.squared, 3)

## Adjusted R-square
round(Training_Model_Summary$adj.r.squared, 3)
## Coefficients of partial determination
## r_squareY1.23
## Proportion of variation in Y explained by X1 (Alcohol) which is not explained by X2 (Type_of_ Wine)

### FULL MODEL: Contains ALL the independent variables.
### In this case, we have three - Alcohol,and Type_of_Wine.
fullmodel <- lm(Quality ~ Alcohol + Type_of_Wine, data = Question5)

### REDUCED MODEL: Removes the independent variables to be tested.
### In this case, we have one independent variable to be tested 
### Alcohol.
reducedmodel <- lm(Quality ~ Type_of_Wine, data = Question5)

f <- anova(fullmodel)
r <- anova(reducedmodel)
f
r
### Retrieving the SSEs for the full and the reduced models
SSE_F <- f$`Sum Sq`[3]
SSE_R <- r$`Sum Sq`[2]

## Calculation of Partial R-square
round(((SSE_R-SSE_F)/SSE_R), 3)

### FULL MODEL: Contains ALL the independent variables.
### In this case, we have three - Alcohol,and Type_of_Wine.
fullmodel <- lm(Quality ~ Alcohol + Type_of_Wine, data = Question5)

### REDUCED MODEL: Removes the independent variables to be tested.
### In this case, we have one independent variable to be tested 
### Type_of_Wine.
reducedmodel <- lm(Quality ~ Alcohol, data = Question5)

f <- anova(fullmodel)
r <- anova(reducedmodel)
f
r
### Retrieving the SSEs for the full and the reduced models
SSE_F <- f$`Sum Sq`[3]
SSE_R <- r$`Sum Sq`[2]

## Calculation of Partial R-square
round(((SSE_R-SSE_F)/SSE_R), 3)


## Interaction variables
#X1 = Alcohol
#X2 = Type_of_Wine
#X3 = X1*X2

## Run a multiple regression with interaction variables.
Training_Model_With_Interaction <- lm(Quality ~ Alcohol + Type_of_Wine + 
                                        Alcohol:Type_of_Wine, data = Question5)

options(scipen = 999) ## Turn off scientific notation
anova(Training_Model_With_Interaction)
summary(Training_Model_With_Interaction)
options(scipen = 0) ## Turn on scientific notation

## Retrieve the coefficients rounded to three decimals
Training_Model_With_Interaction_Summary <- summary(Training_Model_With_Interaction)
Training_Model_With_Interaction_Summary

sprintf("Quality = %.3f + %.3f*Alcohol + %.3f*Type_of_Wine + %.3f*Alcohol*Type_of_Wine", 
        Training_Model_With_Interaction_Summary$coefficients[1,1],
        Training_Model_With_Interaction_Summary$coefficients[2,1],
        Training_Model_With_Interaction_Summary$coefficients[3,1],
        Training_Model_With_Interaction_Summary$coefficients[4,1])


## Test of beta3 (Alcohol* Type_of_Wine)
## t-value
format(round(Training_Model_With_Interaction_Summary$coefficients[4,3], 2), nsmall = 2)


## p-value
format(round(Training_Model_With_Interaction_Summary$coefficients[4,4], 3), nsmall = 2)

