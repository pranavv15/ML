# Load necessary libraries
library(ISLR)
library(rpart)
library(rpart.plot)
library(ggplot2)


# Regression Tree

# Set the seed
set.seed(200)

# Load the data
data(Hitters)
Hitters <- na.omit(Hitters) #Remove NA for demo
Hitters$Salary <- log(Hitters$Salary)
# Perform train-test split
hitter.split <- sample(1:nrow(Hitters), size=nrow(Hitters) * 0.7)
h.train <- Hitters[hitter.split,]
h.test <- Hitters[-hitter.split,]

# Summarize data
summary(Hitters)

# Construct Regression Tree on training set
example.cart <- rpart(formula = Salary ~ Years + Hits + RBI + PutOuts + Runs + Walks, data = h.train, method = "anova", control = rpart.control(minbucket = 6))
# Plot the regression tree from rpart
prp(example.cart, roundint = FALSE)

# View the performance at each level of the tree
example.cart$cptable

# Set up a grid of 10 potential alpha values
cp.param <- example.cart$cptable
cp.param
train.mse <- double(10)
cv.mse <- double(10)
test.mse <- double(10)

# Calculate the average MSE on the train set, the test set, and the CV
for (i in 1:10) {
  alpha <- cp.param[i, 'CP']
  train.mse[i] <- mean((h.train$Salary - predict(prune(example.cart, cp=alpha), newdata = h.train))^2)
  cv.mse[i] <- cp.param[i, 'xerror'] * cp.param[i, 'rel error']
  test.mse[i] <- mean((h.test$Salary - predict(prune(example.cart, cp=alpha), newdata = h.test))^2)
}

# Print MSE values
train.mse
test.mse
cv.mse

# Plot training, CV and testing errors at # of Splits
matplot(cp.param[,'nsplit'], cbind(train.mse, cv.mse, test.mse), pch=19, col=c("red", "black", "blue"), type="b", ylab="Mean Squared Error", xlab="# of Splits")
legend("right", c('Train', 'CV', 'Test'),col=seq_len(3),cex=0.8,fill=c("red", "black", "blue"))

plotcp(example.cart)

# Print pruned and unpruned train, test, and all error
# Error at the maximum splits
train.mse[10] # Unpruned train error
test.mse[10] # Unpruned test error
cv.mse[10] # Unpruned all error

# Error at n=2 splits
train.mse[3] # pruned train error
test.mse[3] # pruned test error
cv.mse[3] # pruned all error

# Plot the actual partitions of the tree
region <- ifelse(h.train$Years < 4.5, 'Region 3', ifelse(h.train$Hits < 118, 'Region 1', 'Region 2'))

ggplot(data=h.train) + geom_point(aes(x=Years, y=Hits, color=Salary, shape=region)) + 
  geom_vline(xintercept = 4.5) +
  geom_segment(x = 4.5, y= 118, xend=30, yend=118) +
  theme_minimal()

# Result of adding an additional split; creates trivial region
region <- ifelse(h.train$Years < 4.5, 'Region 4', ifelse(h.train$Hits < 111, 'Region 1', ifelse(h.train$Hits < 118, 'Region 2', 'Region 3')))
ggplot(data=h.train) + 
  geom_point(aes(x=Years, y=Hits, color=Salary, shape=region)) +
  geom_vline(xintercept = 4.5) +
  geom_segment(x = 4.5, y= 118, xend=30, yend=118) +
  geom_segment(x = 4.5, y= 111, xend=30, yend=111) +
  theme_minimal()

#--------------------------------------------------------------------------------------------------------------------

# Classification Tree

Heart <- read.csv('https://www.statlearning.com/s/Heart.csv')
Heart <- na.omit(Heart) # Remove NA

set.seed(490)
heart.split <- sample(1:nrow(Heart), size=nrow(Heart) * 0.7)
heart.train <- Heart[heart.split,]
heart.test <- Heart[-heart.split,]

summary(Heart)

class.cart <- rpart(formula = AHD ~ ., data = heart.train, method = "class", control = rpart.control(minbucket = 2, xval = 10))
prp(class.cart, roundint = FALSE)

cp.class.param <- class.cart$cptable
cp.class.param

# if the below throws an error, change 6 to 7
train.err <- double(6)
cv.err <- double(6)
test.err <- double(6)

for (i in 1:nrow(cp.class.param)) {
  alpha <- cp.class.param[i, 'CP']
  train.cm <- table(heart.train$AHD, predict(prune(class.cart, cp=alpha), newdata = heart.train, type='class'))
  train.err[i] <- 1-sum(diag(train.cm))/sum(train.cm)
  cv.err[i] <- cp.class.param[i, 'xerror'] * cp.class.param[i, 'rel error']
  test.cm <- table(heart.test$AHD, predict(prune(class.cart, cp=alpha), newdata = heart.test, type='class'))
  test.err[i] <- 1-sum(diag(test.cm))/sum(test.cm)
}

# Print classification error (1 – accuracy) values
train.err
test.err

# Plot training, CV and testing errors at # of Splits/depth

matplot(cp.class.param[,'nsplit'], cbind(train.err, cv.err, test.err), pch=19, col=c("red", "black", "blue"), type="b", ylab="Loss/error", xlab="Depth/# of Splits")
legend("right", c('Train', 'CV', 'Test') ,col=seq_len(3),cex=0.8,fill=c("red", "black", "blue"))

plotcp(class.cart)

# Check CP table, when size of tree =4, the nsplit =3 and CP = 0.02631579       
# Prune the tree at nsplit =3 defined by the complexity parameter
prune.class.trees <- prune(class.cart, cp=cp.class.param[3,'CP'])
prp(prune.class.trees)

# Calculate confusion table and accuracy
conf.mat.tree <- table(heart.test$AHD, predict(prune.class.trees, type = 'class', newdata = heart.test))
conf.mat.tree

# accuracy at nsplit =3, and cp = 0.02631579       
train.prune.acc <- 1 - train.err[3] # Pruned train accuracy
train.prune.acc
test.prune.acc <- 1 - test.err[3] # Pruned test accuracy
test.prune.acc
cv.prune.acc <- 1 - cv.err[3] # Pruned all accuracy
cv.prune.acc


