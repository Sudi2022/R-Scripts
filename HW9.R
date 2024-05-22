library(readxl)

library(ggplot2)

## Installing the lmtest package (for DW test)
install.packages("lmtest")
library(lmtest)

Question1 <- read_excel("/Users/soudabeh/desktop/Question1.xlsx")

View(Question1)

Day <- Question1$Day
Sales <- Question1$Sales
Volume <- Question1$Volume

## Equation of Sales and Volume
model <- lm(Sales ~ Volume)

## Obtain the residuals
residuals <- resid(model)

## Create the timeplot
## Create the line plot of Residuals vs. Day
plot(Day, residuals, type = "l", col = "blue",
     xlab = "Day", ylab = "Residuals",
     ylim = c(-1400, 1400),
     xlim = c(0, 100),
     main = "Time plot of residuals")
abline(h = 0) ## Optional: add a horizontal line at 0.

## Calculate the DW statistic
dwtest(model)

DWTestResults <- dwtest(model)

## Retrieve the DW statistic (rounded to two decimals)
round(DWTestResults$statistic, 2)

## Effect of the outlier

## View the residuals
View(residuals)

## Convert to a data frame
residuals <- data.frame(residuals)

## Sort descending so that the largest residual appears first
residuals %>%
  arrange(-residuals)

## New data set excluding the observation corresponding to the outlier
new_df <- Question1 %>%
  filter(Day != 14)

## Check to see if Day 14 is excluded
print(new_df, n = Inf)

## New Sales and New Volume vectors
Sales_wo_Outlier <- new_df$Sales
Volume_wo_Outlier <- new_df$Volume

## Rerun the regression
model_new <- lm(Sales_wo_Outlier ~ Volume_wo_Outlier)

## Store the coefficients for the models
with_outlier_model_coeff <- coef(model)
without_outlier_model_coeff <- coef(model_new)

## Retrieve the intercept part
with_outlier_model_coeff["(Intercept)"]
without_outlier_model_coeff["(Intercept)"]

## Retrieve the slope part
with_outlier_model_coeff["Volume"]
without_outlier_model_coeff["Volume_wo_Outlier"]

## Compare slope, intercept, R-sq, and se for both the models
mtx_comp <- matrix(c(round(with_outlier_model_coeff["(Intercept)"], 3),
                     round(with_outlier_model_coeff["Volume"], 3),
                     round(summary(model)$r.squared, 3),
                     round(summary(model)$sigma, 3), 
                     round(without_outlier_model_coeff["(Intercept)"], 3),
                     round(without_outlier_model_coeff["Volume_wo_Outlier"], 3),
                     round(summary(model_new)$r.squared, 3), 
                     round(summary(model_new)$sigma, 3)),
                   byrow = TRUE, nrow = 2)
rownames(mtx_comp) <- c("With outlier", "Without outlier")
colnames(mtx_comp) <- c("Intercept", "Slope", "R-squared", "se")

## Display the matrix
mtx_comp

## Optional: Create two regression lines to see the effects visually
## Create a scatterplot
#plot(x = Volume, 
    # y = Sales,
    # xlab = "Volume (gallons)",
    # ylab = "Sales (dollars)",
    # main = "Plot of Volume (gallons) and Sales (dollars)",
    # xlim = c(0, 5000),
    # ylim = c(1000, 4000))
#legend("topleft", 
      # lty = 1:1, 
      # c("With outlier", "Without outlier"), 
      # col = c("red", "blue")) 
##abline(lm(Sales ~ Volume), col = "red")
#abline(lm(Sales_wo_Outlier ~ Volume_wo_Outlier), col = "blue")


# Question2

Question2 <- read_excel("/Users/soudabeh/desktop/Question2.xlsx")

View(Question2)

SqFt <- Question2$`Square Feet`
Price <- Question2$Price


## Create a scatterplot
plot(x = SqFt, 
     y = Price,
     xlab = "Square Feet",
     ylab = "Price (dollars)",
     main = "Plot of Price and Sq Ft",
     xlim = c(0, 5000),
     ylim = c(0, 3000000))

## Run the model and obtain the residuals

## Equation of Price and Sq Ft
model <- lm(Price ~ SqFt)

## Obtain the residuals
#residuals <- resid(model)

## Obtain the fits
#fits <- fitted(model)

## Check for heteroscedasticity
#plot(x = fits, 
   #  y = residuals,
   #  xlab = "Fitted values",
   #  ylab = "Residuals",
    # main = "Plot of Residuals by fits")
#abline(h = 0) ## Optional: add a horizontal line at 0.

## Optional: Create side-by-side box plots

## First categorize homes into small, medium, and large
## Small = Less than 1500 sq ft
## Medium = 1500 to 2500 sq ft
## Large = More than 2500 sq ft

#Question2 <- Question2 %>%
 # mutate(Size = case_when(
  #  SqFt < 1500 ~ 'Small',
  #  SqFt > 2500 ~ 'Large',
  #  TRUE ~ 'Medium'
 # ))

## Check
#print(Question2, n = Inf)

## For ordering
#Question2 <- factor(Question2$Size, ordered = TRUE, 
                         levels = c("Small", "Medium", "Large"))

## Create a boxplot
#options(scipen = 999) ## Turn off scientific notation
#boxplot(residuals ~ Question2$Size,
   #     main='Residuals by Size',
    #    xlab='Size',
     #   ylab='Residuals')
#options(scipen = 0) ## Turn on scientific notation

## Create prediction intervals (Optional: Not for homework)
# Add predictions to the original data set
#prediction_int <- predict(model, interval = "prediction", level = 0.95)
#NewDataSet <- cbind(SolvedEx2, prediction_int)
# Regression line + confidence intervals
#plt <- ggplot(NewDataSet, aes(SqFt, Price)) +
#  geom_point() +
 # ggtitle("95% PI") +
 # theme(plot.title = element_text(hjust = 0.5, face = "bold"))
# 3. Add prediction intervals
#plt + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
 # geom_line(aes(y = upr), color = "red", linetype = "dashed")

## Transformation
Price_Per_Sq_Ft <- Price/SqFt
Inv_Sq_Ft <- 1/SqFt

## Create a scatterplot
plot(x = Inv_Sq_Ft, 
     y = Price_Per_Sq_Ft,
     xlab = "1/Square Feet",
     ylab = "Price/Sq Ft",
     main = "Plot of Price/Sq Ft and 1/Sq Ft",
     xlim = c(0, 0.003),
     ylim = c(0, 1000))

## Equation of Price/Sq Ft and 1/Sq Ft
model_trans <- lm(Price_Per_Sq_Ft ~ Inv_Sq_Ft)

## Obtain the residuals
residuals_trans <- resid(model_trans)

## Obtain the fits
fits_trans <- fitted(model_trans)

## Check for heteroscedasticity
plot(x = fits_trans, 
     y = residuals_trans,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Plot of Residuals by fits")
abline(h = 0) ## Optional: add a horizontal line at 0.

## Include Size in this transformed data frame
df_Transformed <- data.frame(Price_Per_Sq_Ft, Inv_Sq_Ft, 1/Inv_Sq_Ft)

df_Transformed <- df_Transformed %>%
  mutate(Size = case_when(
    SqFt < 1500 ~ 'Small',
    SqFt > 2500 ~ 'Large',
    TRUE ~ 'Medium'
  ))

## Check
print(df_Transformed)

## For ordering
df_Transformed$Size <- factor(df_Transformed$Size, ordered = TRUE, 
                              levels = c("Small", "Medium", "Large"))

## Create a boxplot
options(scipen = 999) ## Turn off scientific notation
boxplot(residuals_trans ~ df_Transformed$Size,
        main='Residuals_Price_Per_Sq_Ft by Size',
        xlab='Size',
        ylab='Residuals')
options(scipen = 0) ## Turn on scientific notation

## Store the coefficients for the model
trans_model_coeff <- coef(model_trans)

## Retrieve the intercept part
round(trans_model_coeff["(Intercept)"], 0)

## Retrieve the slope part
round(trans_model_coeff["Inv_Sq_Ft"], 0)

## Display the R-sq and se
round(summary(model_trans)$r.squared, 2)
round(summary(model_trans)$sigma, 2)

## Compare the confidence intervals of the marginal slopes of the two models

## Original model: Price on Sq Ft
## Display 95% CI for slope
lcl <- confint(model, "SqFt", level = 0.95)[1]
lcl <- round(lcl, 2)

ucl <- confint(model, "SqFt", level = 0.95)[2]
ucl <- round(ucl, 2)

sprintf("The 95%% CI for the marginal cost for the original model is [%4.2f dollars, %4.2f dollars].", lcl, ucl)

## Transformed model: Price_Per_Sq_Ft on 1/Sq Ft
## Display 95% CI for intercept
lcl <- confint(model_trans, "(Intercept)", level = 0.95)[1]
round(lcl, 2)

ucl <- confint(model_trans, "(Intercept)", level = 0.95)[2]
round(ucl, 2)

sprintf("The 95%% CI for the marginal cost for the original model is [%4.2f dollars, %4.2f dollars].", lcl, ucl)

## se for the two models

## Untransformed model
model_summary <- summary(model)
slope_se <- model_summary$coefficients[2, 2]
round(slope_se, 2)

## Transformed model
trans_model_summary <- summary(model_trans)
int_se <- trans_model_summary$coefficients[1, 2]
round(int_se, 2)

sprintf("The se for the marginal slope for the untransformed model is %4.2f dollars and that for the transformed model is %4.2f dollars.", 
        round(slope_se, 2),
        round(int_se, 2))

## Including the missing observation:
## penthouse condo with 1,586 square feet priced at $1.789 million.

## Reread the file
Question2 <- read_excel("/Users/soudabeh/desktop/Question2.xlsx")

## Add a new row
Question2 <- Question2 %>%
  add_row(`Square Feet` = 1584, Price = 1788000)

print(Question2, n = Inf) ## Check

## Store the variables
Price <- Question2$Price
SqFt <- Question2$`Square Feet`

## Transformation
Price_Per_Sq_Ft <- Price/SqFt
Inv_Sq_Ft <- 1/SqFt

## Create a data frame
df_Transformed <- data.frame(Price_Per_Sq_Ft, Inv_Sq_Ft, 1/Inv_Sq_Ft)

## Create a new model
## Equation of Price/Sq Ft and 1/Sq Ft
model_trans_1 <- lm(Price_Per_Sq_Ft ~ Inv_Sq_Ft)

trans_model_1_summary <- summary(model_trans_1)

## Store the coefficients for the model
trans_model_1_coeff <- coef(model_trans_1)

## Retrieve the intercept part
round(trans_model_1_coeff["(Intercept)"], 0)

## Retrieve the slope part
round(trans_model_1_coeff["Inv_Sq_Ft"], 0)

## Obtain the r-sq and se
round(summary(model_trans_1)$r.squared, 2)
round(summary(model_trans_1)$sigma, 2)

## Compare
mtx_comp <- matrix(c(round(trans_model_coeff["(Intercept)"], 0),
                     round(trans_model_coeff["Inv_Sq_Ft"], 0),
                     round(summary(model_trans)$r.squared, 2),
                     round(summary(model_trans)$sigma, 2), 
                     round(trans_model_1_coeff["(Intercept)"], 0),
                     round(trans_model_1_coeff["Inv_Sq_Ft"], 0),
                     round(summary(model_trans_1)$r.squared, 2), 
                     round(summary(model_trans_1)$sigma, 2)),
                   byrow = TRUE, nrow = 2)
rownames(mtx_comp) <- c("Model Trans", "Model Trans (with additional row))")
colnames(mtx_comp) <- c("Intercept", "Slope", "R-squared", "se")

mtx_comp
