#Soudabeh Rafieisakhaei HW5
# Question 1
Obs <- c(24,42,36,24,24)
Exp <-rep(1/5,5)
Exp
chisq.test(x= Obs,p=Exp)
chisq.test(x= Obs,p=Exp)$expected

# Question 2
Obs <- c(215,190,200,195,200)
Exp <-rep(1/5,5)
Exp
chisq.test(x= Obs,p=Exp)
chisq.test(x= Obs,p=Exp)$expected


#Question3
customer_complains <- read.csv("/Users/soudabeh/desktop/Q3.csv")
dim(customer_complains)

head(customer_complains)

customer_complains <- as.table(rbind(c(31, 40, 48,67), c(43, 57,128,70)))
dimnames(customer_complains) <- list(customer = c("Long-term", "Recent"),
                    party = c("Sales_Person","Billing_Error", "Store
                              _Display","Lack_of_Inventory"))
(Xsq <- chisq.test(customer_complains))

# Question4
customer_satisfaction <- as.table(rbind(c(64, 59, 57), c(27, 22,24), c(9,19,19)))
dimnames(customer_satisfaction) <- list(Level = c("V", "S","U"),
                                     party = c("Y1","Y2", "Y3"))
(Xsq <- chisq.test(customer_satisfaction))


