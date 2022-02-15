install.packages("glmnet")
library(glmnet)
install.packages('plotmo')
library(plotmo)

#RIGDE Regression

#Load dataset
credit<- read.csv("/Users/pranavvinod/Downloads/archive/Credit.csv", sep = ',', header = TRUE)  
credit <- credit[, 2:12] #  Remove first column (unneeded)

# Create input matrix of data with text variables changed to numeric, e.g. Gender Male/Female changed to 1/0
credit.mat <- model.matrix(Balance ~  .-1, data=credit) 
# Delete unnecessary info at the bottom 
credit.mat <- credit.mat[,-8]
head(credit.mat)

set.seed(1) # Set seed for reproducibility

# Separate the features (independent) from the target (dependent) variables
x <- credit.mat 
y <- credit[, 'Balance']


# Create a grid of possible values of lambda to test
grid <- 10^seq(6, -3, length=10) 

# Construct linear model with the following parameters:
# scale function centers mean of each column around the same value
# alpha = 0 specifies ridge regression beacuse by default it is Lassso i.e.alpha = 1
# lambda = grid specifies to run the model for each lambda value in grid
# thresh = 1e-2 defines the stopping criteria for optimization
# standardize = TRUE converts X into same range (mean = 0, sd = 1)

ridge.mod <- glmnet(scale(x), y, alpha=0, lambda=grid, thresh=1e-2, standardize = TRUE)

# Plot results over different values of lambda
# If you modify the function so that label = 2, what happens?
plot_glmnet(ridge.mod, xvar = "lambda", label = 4)

# Perform cross-validation
cv.out <- cv.glmnet(scale(x), y, alpha=0, nfolds = 10)
cv.out

# Results below yield lambda with minimum MSE as well as 
# Lambda.1se is the largest value of lambda such that error is within 1 standard  error of the minimum mse
# In this case (ridge regression) they are the same
##
#Call:  cv.glmnet(x = scale(x), y = y, nfolds = 10, alpha = 0) 
##
# Plot the MSE for each lambda value
plot(cv.out)

# Determine best lamda
best.lambda <- cv.out$lambda.1se 
best.lambda


# Run ridge regression at the best lambda and evaluate using MSE
ridge.final <- glmnet(scale(x), y, alpha=0, lambda=best.lambda, thresh=1e-2, standardize = TRUE)
predict(ridge.final, type="coefficients", s=best.lambda)

# Evaluate MSE and RMSE of the ridge model with the optimal lambda
ridge.pred <- predict(ridge.final, s=best.lambda, newx=scale(x))
print(paste('MSE:', mean((ridge.pred - y)^2))) 

print(paste('RMSE:', sqrt(mean((ridge.pred - y)^2))))

######################################################################################

#LASSO Regression

# Create a grid of possible values of lambda to test
grid <- 10^seq(6, -3, length=10)

# Create linear model with the following parameters:
# scale function centers mean of each column around the same value
# alpha = 1 specifies lasso regression
# lambda = grid specifies to run the model for each lambda value in grid
# thresh = 1e-2 defines the stopping criteria for optimization
# standardize = TRUE converts X into same range (mean = 0, sd = 1)

lasso.mod <- glmnet(scale(x), y, alpha=1, lambda=grid, thresh=1e-2, standardize = TRUE)
# Plot coefficient values at different lambda
plot_glmnet(lasso.mod, xvar="lambda", label = 4)

# Perform cross-validation to determine best lambda value
lasso.cv.out <- cv.glmnet(scale(x), y, alpha=1, nfolds = 10) 
lasso.cv.out

plot(lasso.cv.out)

# Create model using value of best lambda with a small number of variables and print output
lasso.best.lambda <- lasso.cv.out$lambda.1se
lasso.best.lambda
lasso.final <- glmnet(scale(x), y, alpha=1, lambda= lasso.best.lambda, thresh=1e-2, standardize = TRUE)
predict(lasso.final, type="coefficients", s=lasso.best.lambda )

# Calculate MSE and RMSE for best model    
lasso.pred <- predict(lasso.final, s= lasso.best.lambda, newx=scale(x)) 
print(paste('MSE:', mean((lasso.pred - y)^2))) 
print(paste('RMSE:', sqrt(mean((lasso.pred - y)^2))))

