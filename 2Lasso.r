data <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data",stringsAsFactors = FALSE)
library(dplyr)
names(data) <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD",
                 "TAX","PTRATIO","B","LSTAT","MEDV")


median_income <- data$MEDV



# Lasso using Caret

library(caret)
# Custom Control Parameters
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = T)

set.seed(490)
lasso <- train(MEDV ~ .,
               training_set,
               method = 'glmnet',
               tuneGrid = expand.grid(alpha = 1, lambda = seq(0.0001, 0.2, length = 5)), 
               trControl = custom)
plot(lasso) 
lasso
lambda <- lasso$bestTune$lambda
plot_glmnet(lasso$finalModel, xvar="lambda", label = 4)
plot_glmnet(lasso$finalModel, xvar = 'dev', label = 4)

plot(varImp(lasso, scale = F))
predict(lasso$finalModel, type="coefficients", s=lambda )


#-------------------------------------------------------------------------------------------------

# Lasso using glmnet

library(glmnet)
library(plotmo)

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

