data <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data",stringsAsFactors = FALSE)

library(dplyr)
library(ggplot2)
library(corrplot)
library(highcharter)
library(Amelia)

names(data) <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","3AGE","DIS","RAD",
                 "TAX","PTRATIO","B","LSTAT","MEDV")

# 1. Data Exploration

#Summary of data
summary(data)

#missing values in data
missmap(data,col = c('yellow','black'))

# A distribution of data


# Pair plot of data
pairs(~ CRIM + ZN + INDUS + CHAS + NOX + RM + MEDV , data =data)
pairs(~ AGE + DIS + RAD + TAX + PTRATIO + B + LSTAT + MEDV, data=data)     #We can see that some variables show a linear relation to median house prices.

# Correlation plot to quantify the relation
corr_matrix <- cor(data)
corrplot(corr_matrix, method = "circle")

#-------------------------------------------------------------------------------------------------------------------------------
# 2. X and Y Identification

# Y - the outcome or the attribute that we want to predict is MEDV i.e. the median value of owner occupied home in $1000's
median_income <- data$MEDV

# X - the predictor variables are all the other variables other than MEDV


#-------------------------------------------------------------------------------------------------------------------------------
# 3a. Multiple Linear Regression(Analysis and Final Model)

#creating training and testing sets in the ratio of 80:20
set.seed(490)
Random.seed <- c("Mersenne-Twister", 1) 

training.indices <- sample(1:nrow(data), 0.8 * nrow(data), replace = FALSE)
training_set <- data[training.indices,]
testing_set <- data[-training.indices,]
View(training_set)
View(testing_set)

#Run multiple linear regression on training data
multi.model <- lm(MEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT, data = training_set)
summary(multi.model)
mse <- mean(residuals(multi.model)^2)
mse
rss <- sum(residuals(multi.model)^2)
rss

par(mfrow=c(2,2))
plot(multi.model)

#Run multiple linear regression on testing data
multi.model.predictions <- predict(multi.model, testing_set)

#Calculating testing errors
test.multi.model.ssl <- sum((testing_set$MEDV - multi.model.predictions)^2)   #observed - prediction is what we are doing here
test.multi.model.mse <- test.multi.model.ssl / nrow(testing_set)
test.multi.model.rmse <- sqrt(test.multi.model.mse)
sprintf("SSL/SSR/SSE: %f", test.multi.model.ssl)
sprintf("MSE: %f", test.multi.model.mse)
sprintf("RMSE: %f", test.multi.model.rmse)

#Running the model on the entire dataset
multi.model.full <- lm(MEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT, data = data)
summary(multi.model.full)
mse <- mean(residuals(multi.model.full)^2)
mse
rss <- sum(residuals(multi.model.full)^2)
rss

par(mfrow=c(2,2))
plot(multi.model.full)

par(mfrow = c(1,1))


##################################################################################################################################
# 3b. Ridge Regression(Analysis and Final Model)

library(glmnet)
library(plotmo)

# separating the independent and the dependent variables

set.seed(490)
# x is the data
x <- data[1:13]
# median_income is the dependent variable
median_income <- data$MEDV


# Creating a grid of possible values of lambda to test
grid <- 10^seq(6, -3, length=10) 

# Construct linear model with the following parameters:
# scale function centers mean of each column around the same value
# alpha = 0 specifies ridge regression beacuse by default it is Lassso i.e.alpha = 1
# lambda = grid specifies to run the model for each lambda value in grid
# thresh = 1e-2 defines the stopping criteria for optimization
# standardize = TRUE converts X into same range (mean = 0, sd = 1)
ridge.mod <- glmnet(scale(x), median_income, alpha=0, lambda=grid, thresh=1e-2, standardize = TRUE)

# Plot results over different values of lambda
plot_glmnet(ridge.mod, xvar = "lambda", label = 4)



# Perform cross-validation
cv.out <- cv.glmnet(scale(x), median_income, alpha=0, nfolds = 10)
cv.out

# Results below yield lambda with minimum MSE as well as 
# Lambda.1se is the largest value of lambda such that error is within 1 standard  error of the minimum mse

# Plot the MSE for each lambda value
plot(cv.out)


# Determine best lamda
best.lambda <- cv.out$lambda.1se 
best.lambda

# Run ridge regression at the best lambda and evaluate using MSE
ridge.final <- glmnet(scale(x), median_income, alpha=0, lambda=best.lambda, thresh=1e-2, standardize = TRUE)
predict(ridge.final, type="coefficients", s=best.lambda)

# Evaluate MSE and RMSE of the ridge model with the optimal lambda
ridge.pred <- predict(ridge.final, s=best.lambda, newx=scale(x))

print(paste('MSE:', mean((ridge.pred - median_income)^2))) 

print(paste('RMSE:', sqrt(mean((ridge.pred - median_income)^2))))
print(paste('SSE:', sum(((ridge.pred - median_income)^2))))
print(paste('SST:', sum(((median_income - mean(median_income))^2))))
print(paste('R^2:', 1 - sum(((ridge.pred - median_income)^2))/sum(((median_income - mean(median_income))^2))))


#library(caret)
#plot(varImp(ridge.final,lambda = 2.73615, scale=F))

####################################################################################################################################
# 3c. Lasso Regression(Analysis and Final Model)


# Create a grid of possible values of lambda to test
grid <- 10^seq(6, -3, length=10)

# Create linear model with the following parameters:
# scale function centers mean of each column around the same value
# alpha = 1 specifies lasso regression
# lambda = grid specifies to run the model for each lambda value in grid
# thresh = 1e-2 defines the stopping criteria for optimization
# standardize = TRUE converts X into same range (mean = 0, sd = 1)

lasso.mod <- glmnet(scale(x), median_income, alpha=1, lambda=grid, thresh=1e-2, standardize = TRUE)
# Plot coefficient values at different lambda
plot_glmnet(lasso.mod, xvar="lambda", label = 4)

# Perform cross-validation to determine best lambda value
lasso.cv.out <- cv.glmnet(scale(x), median_income, alpha=1, nfolds = 10) 
lasso.cv.out

plot(lasso.cv.out)

# Create model using value of best lambda with a small number of variables and print output
lasso.best.lambda <- lasso.cv.out$lambda.1se
lasso.best.lambda
lasso.final <- glmnet(scale(x), median_income, alpha=1, lambda= lasso.best.lambda, thresh=1e-2, standardize = TRUE)
predict(lasso.final, type="coefficients", s=lasso.best.lambda )

# Calculate MSE and RMSE for best model    
lasso.pred <- predict(lasso.final, s= lasso.best.lambda, newx=scale(x)) 
print(paste('MSE:', mean((lasso.pred - median_income)^2))) 
print(paste('RMSE:', sqrt(mean((lasso.pred - median_income)^2))))

print(paste('SSE:', sum(((lasso.pred - median_income)^2))))
print(paste('SST:', sum(((median_income - mean(median_income))^2))))
print(paste('R^2:', 1 - sum(((lasso.pred - median_income)^2))/sum(((median_income - mean(median_income))^2))))




#-------------------------------------------------------------------------------------------------------------------------------
# 4. Summary Table



#-------------------------------------------------------------------------------------------------------------------------------
# 5. Description of Cross Validation algorithm(CV)




#---------------------------------------------------------------------------------------------------------------------------------
# 6. References and citations






