data <- read.table(file.choose(),header = F)

library(dplyr)
names(data) <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD",
                 "TAX","PTRATIO","B","LSTAT","MEDV")
# 1. Data Exploration
plot(x=data$AGE,y=data$MEDV)
plot(x=data$DIS,y=data$MEDV)
plot(x=data$RM,y=data$MEDV) #some linear relation
plot(x=data$NOX,y=data$MEDV)
plot(x=data$TAX,y=data$MEDV)
plot(x=data$CRIM,y=data$MEDV)
plot(x=data$ZN,y=data$MEDV)
plot(x=data$LSTAT,y=data$MEDV) #some linear relation
plot(x=data$B,y=data$MEDV)
plot(x=data$INDUS,y=data$MEDV)
plot(x=data$PTRATIO,y=data$MEDV)
plot(x=data$RAD,y=data$MEDV)
plot(x=data$CHAS,y=data$MEDV)


summary(data)
#-------------------------------------------------------------------------------------------------------------------------------
# 2. X and Y Identification

# Y - the outcome or the attribute that we want to predict is MEDV i.e. the median value of owner occupied home in $1000's
y <- data$MEDV

# X - the predictor variables are all the other variables other than MEDV



#-------------------------------------------------------------------------------------------------------------------------------
# 3a. Multiple Linear Regression(Analysis and Final Model)
model <- lm(data$MEDV~data$CRIM+data$ZN+data$INDUS+data$CHAS+data$NOX+data$RM+data$AGE+data$DIS+data$RAD+data$TAX+data$PTRATIO+data$B+data$LSTAT, data = data)
summary(model)

plot(x=data$RAD,y=data$MEDV)
abline(model)




##################################################################################################################################
# 3b. Ridge Regression(Analysis and Final Model)





####################################################################################################################################
# 3c. Lasso Regression(Analysis and Final Model)




#-------------------------------------------------------------------------------------------------------------------------------
# 4. Summary Table



#-------------------------------------------------------------------------------------------------------------------------------
# 5. Description of Cross Validation algorithm(CV)




#---------------------------------------------------------------------------------------------------------------------------------
# 6. References and citations






