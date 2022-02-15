auto.mpg <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data', stringsAsFactors = FALSE)
View(auto.mpg)
head(auto.mpg,5)

#Assign names to all columns
names(auto.mpg) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "model.year", "origin", "name")

set.seed(490)
Random.seed <- c("Mersenne-Twister", 1) 

#Split data into training and testing data
training.indices <- sample(1:nrow(auto.mpg), 0.5 * nrow(auto.mpg), replace = FALSE)
training.data <- auto.mpg[training.indices, ]
View(training.data)
testing.data <- auto.mpg[-training.indices,]
View(testing.data)

#Run simple linear regression on training data
simple.model <- lm(mpg ~ weight, data = training.data)
print(summary(simple.model))
mse <- mean(residuals(simple.model)^2)
mse
rss <- sum(residuals(simple.model)^2)
rss

simple.model.predictions <- predict(simple.model, testing.data)

#Calculate the testing data set errors
test.simple.model.ssl <- sum((testing.data$mpg - simple.model.predictions)^2)   #observed - prediction is what we are doing here
test.simple.model.mse <- test.simple.model.ssl / nrow(testing.data)
test.simple.model.rmse <- sqrt(test.simple.model.mse)
sprintf("SSL/SSR/SSE: %f", test.simple.model.ssl)
sprintf("MSE: %f", test.simple.model.mse)
sprintf("RMSE: %f", test.simple.model.rmse)

#Plot the best fit line for testing data set
scatter.smooth(x= testing.data$model.year, y= testing.data$mpg, main="mpg ~ model year")

#Now run simple regression on the entire data
simple.model.full <- lm(mpg ~ weight, data = auto.mpg)
print(summary(simple.model.full))
mse <- mean(residuals(simple.model.full)^2)
mse
rss <- sum(residuals(simple.model.full)^2)
rss

###################################################################################################################
#Now Run a multiple regression model

#Define a model
multi.var.model <- lm(mpg ~ cylinders + displacement + weight + acceleration + model.year, data = training.data)
print(summary(multi.var.model))
mse <- mean(residuals(multi.var.model)^2)
mse
rss <- sum(residuals(multi.var.model)^2)
rss
#Can see that only two predictors are statistically significant

#Run the model on testing set
multi.model.predictions <- predict(multi.var.model, testing.data)

#Calculate the testing data set errors
test.multi.model.ssl <- sum((testing.data$mpg - multi.model.predictions)^2)   #observed - prediction is what we are doing here
test.multi.model.mse <- test.multi.model.ssl / nrow(testing.data)
test.multi.model.rmse <- sqrt(test.multi.model.mse)
sprintf("SSL/SSR/SSE: %f", test.multi.model.ssl)
sprintf("MSE: %f", test.multi.model.mse)
sprintf("RMSE: %f", test.multi.model.rmse)

#Plot the best fit line for testing data set
plot(mpg ~ weight, data=testing.data)
abline(multi.var.model)

#Now we run the multiple regression model on the entire dataset
multi.var.model.full <- lm(mpg ~ cylinders + displacement + weight + acceleration + model.year, data = auto.mpg)
print(summary(multi.var.model.full))
mse <- mean(residuals(multi.var.model.full)^2)
mse
rss <- sum(residuals(multi.var.model.full)^2)
rss

###################################################################################################################

# Now run parsimonious model using weight and model year as variables

#Run model on training set
#Define a model
sig.model <- lm(mpg ~ weight + model.year, data = training.data)
print(summary(sig.model))
mse <- mean(residuals(sig.model)^2)
mse
rss <- sum(residuals(sig.model)^2)
rss

#Now running the model on testing data

#Run the model on testing set
sig.model.predictions <- predict(sig.model, testing.data)

#Calculate the testing data set errors
test.sig.model.ssl <- sum((testing.data$mpg - sig.model.predictions)^2)   #observed - prediction is what we are doing here
test.sig.model.mse <- test.sig.model.ssl / nrow(testing.data)
test.sig.model.rmse <- sqrt(test.sig.model.mse)
sprintf("SSL/SSR/SSE: %f", test.sig.model.ssl)
sprintf("MSE: %f", test.sig.model.mse)
sprintf("RMSE: %f", test.sig.model.rmse)

#Now running the model on the entire dataset

sig.model.full <- lm(mpg ~ weight + model.year, data = auto.mpg)
print(summary(sig.model.full))
mse <- mean(residuals(sig.model.full)^2)
mse
rss <- sum(residuals(sig.model.full)^2)
rss


