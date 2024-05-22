#Soudabeh Rafieisakhaei HW6
## Installing the package to use the readxl function
install.packages("tidyverse")
library(readxl)
library(dplyr)


# Question1
Question1 <- read_excel("/Users/soudabeh/desktop/Q1.xlsx")
View(Question1)

Avg_Jan_Temp <- Question1$Avg_Jan_Temp
Pct_4WD <- Question1$Pct_4WD

## Create a scatterplot
plot(x = Avg_Jan_Temp, 
     y = Pct_4WD,
     xlab = "Temp (degrees F)",
     ylab = "% 4-WD",
     xlim = c(0, 70),
     ylim = c(0, 80),
     main = "Plot of Temp and % 4WD sold")


## Highest
Question1 %>%
  slice_max(Pct_4WD, n = 1)

Question1 %>%
  slice_min(Pct_4WD, n = 1)

## Correlation coefficient
round(cor(Avg_Jan_Temp, Pct_4WD), 3)


#Question 2
Question2 <- read_excel("/Users/soudabeh/desktop/Q2.xlsx")
View(Question2)

Errors <- Question22$Errors
Entered <- Question2$Entered

## Create a scatterplot
plot(x = Entered, 
     y = Errors,
     xlab = "Entered",
     ylab = "Errors",
     xlim = c(0, 8000),
     ylim = c(0, 60),
     main = "Plot of Errors and Entries made")


round(cor(Entered, Errors), 3)


#Question3

Question3 <- read_excel("/Users/soudabeh/desktop/Q3.xlsx")
View(Question3)

Weight <- SolvedEx3$Weight_pounds
Highway <- SolvedEx3$Highway_MPG

## Create a scatterplot
plot(x = Highway, 
     y = Weight,
     xlab = "Weight",
     ylab = "Highway_MPG",
     xlim = c(180, 540),
     ylim = c(8, 30),
     main = "Plot")


round(cor(Highway, Weight), 3)


#Question4
Question4 <- read_excel("/Users/soudabeh/desktop/Q4.xlsx")
View(Question4)

Weight <- SolvedEx3$Weight_pounds
Highway <- SolvedEx3$Highway_MPG

## Create a scatterplot
plot(x = Weight, 
     y = Highway_MPG,
     xlab = "Weight",
     ylab = "Highway_MPG",
     xlim = c(15, 35),
     ylim = c(3500, 5000),
     main = "Plot")

round(cor(Highway, Weight), 3)


#Question 5
Question5 <- read_excel("/Users/soudabeh/desktop/Q5.xlsx")
View(Question5)

New_System <- Question5$New_System
Scale_Weight <- Question5$Scale_Weight

## Create a scatterplot
plot(x = Scale_Weight,
     y = New_System,
     xlab = "Scale_Weight",
     ylab = "New_System",
     xlim = c(20, 45),
     ylim = c(25, 40),
     main = "Plot of New_System and Scale_Weight made")

round(cor(Scale_Weight, New_System), 3)


