# Load necessary libraries
library(ISLR)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(dplyr)

credit <- read.csv(file.choose(), header = TRUE)

# Data Cleaning

# rename headers 
headers <- credit[1,]
names(credit) <- headers
credit <- credit[-c(1),]
names(credit)[25] <- "DEFAULT"

# change data types in columns from factor to numeric
cols <- c(1,2,6,13,14,15,16,17,18,19,20,21,22,23,24)
cols2 <- c(3,4,5,7,8,9,10,11,12,25)
for (i in cols) {
  credit[,i] <- as.numeric((unlist(credit[,i])))
}
for (j in cols2) {
  credit[,j] <- as.factor((unlist(credit[,j])))
}


# omit na
credit <- na.omit(credit)

# Descriptive analysis
summary(credit)
str(credit)

plot(credit$AGE,credit$LIMIT_BAL)

# Classification Tree

set.seed(490)

# split into training and testing
credit.split <- sample(1:nrow(credit), size = nrow(credit)*0.8)
train_set <- credit[credit.split,]
test_set <- credit[-credit.split,]

class.cart2 <- rpart(formula = payment_default ~ .-ID, data = train_set, method = "class", control = rpart.control(minbucket = 60,minsplit = 30, xval = 10, cp=0.0001))
prp(class.cart2, roundint = FALSE)


cp.class.param <- class.cart2$cptable
cp.class.param

# Cost Complexity Pruning

train.err <- double(14)
cv.err <- double(14)
test.err <- double(14)

for (i in 1:nrow(cp.class.param)) {
  alpha <- cp.class.param[i, 'CP']
  train.cm <- table(train_set$payment_default, predict(prune(class.cart2, cp=alpha), newdata = train_set, type='class'))
  train.err[i] <- 1-sum(diag(train.cm))/sum(train.cm)
  cv.err[i] <- cp.class.param[i, 'xerror'] * cp.class.param[i, 'rel error']
  test.cm <- table(test_set$payment_default, predict(prune(class.cart2, cp=alpha), newdata = test_set, type='class'))
  test.err[i] <- 1-sum(diag(test.cm))/sum(test.cm)
}

# Print classification error (1 – accuracy) values
train.err
test.err

# Plot training, CV and testing errors at # of Splits/depth

matplot(cp.class.param[,'nsplit'], cbind(train.err, cv.err, test.err), pch=19, col=c("red", "black", "blue"), type="b", ylab="Loss/error", xlab="Depth/# of Splits")
legend("right", c('Train', 'CV', 'Test') ,col=seq_len(3),cex=0.8,fill=c("red", "black", "blue"))

plotcp(class.cart2)

# Check CP table, when size of tree =13, the nsplit =12 and CP = 0.0009372071           
# Prune the tree at nsplit =12 defined by the complexity parameter
prune.class.trees <- prune(class.cart2, cp=cp.class.param[7,'CP'])
prp(prune.class.trees)

# Model Evaluation

# accuracy at unpruned tree
train.prune.acc <- 1 - train.err[14] # Pruned train accuracy
train.prune.acc
test.prune.acc <- 1 - test.err[14] # Pruned test accuracy
test.prune.acc
cv.prune.acc <- 1 - cv.err[14] # Pruned all accuracy
cv.prune.acc

#accuracy at nsplit =9, and cp = 0.0013086558             
train.prune.acc <- 1 - train.err[7] # Pruned train accuracy
train.prune.acc
test.prune.acc <- 1 - test.err[7] # Pruned test accuracy
test.prune.acc
cv.prune.acc <- 1 - cv.err[7] # Pruned all accuracy
cv.prune.acc

# Creating confusion tables 

# For testing data
cftable <- table(test_set$payment_default, predict(prune.class.trees, type = 'class', newdata = test_set))
cftable

# Calculate Accuracy
accuracy <- sum(diag(cftable))/sum(cftable)
accuracy

# Calculate Sensitivity
sensitivity<-cftable[1]/(cftable[1] + cftable[2])
sensitivity

# Calculate Specificity
specificity <- cftable[4]/(cftable[3] + cftable[4])
specificity

# Calculate Positive Predictive Value
ppv <- cftable[1]/(cftable[1] + cftable[3])
ppv

# Calculate Negative Predictive Value
npv <- cftable[4]/(cftable[2] + cftable[4])
npv
#--------------------------------------------------------------------------------------------

# For complete dataset
cftable <- table(credit$payment_default, predict(prune.class.trees, type = 'class', newdata = credit))
cftable

# Calculate Accuracy
accuracy <- sum(diag(cftable))/sum(cftable)
accuracy

# Calculate Sensitivity
sensitivity<-cftable[1]/(cftable[1] + cftable[2])
sensitivity

# Calculate Specificity
specificity <- cftable[4]/(cftable[3] + cftable[4])
specificity

# Calculate Positive Predictive Value
ppv <- cftable[1]/(cftable[1] + cftable[3])
ppv

# Calculate Negative Predictive Value
npv <- cftable[4]/(cftable[2] + cftable[4])
npv

#------------------------------------------------------------------------------------------------------

# For training data
cftable <- table(train_set$payment_default, predict(prune.class.trees, type = 'class', newdata = train_set))
cftable

# Calculate Accuracy
accuracy <- sum(diag(cftable))/sum(cftable)
accuracy

# Calculate Sensitivity
sensitivity<-cftable[1]/(cftable[1] + cftable[2])
sensitivity

# Calculate Specificity
specificity <- cftable[4]/(cftable[3] + cftable[4])
specificity

# Calculate Positive Predictive Value
ppv <- cftable[1]/(cftable[1] + cftable[3])
ppv

# Calculate Negative Predictive Value
npv <- cftable[4]/(cftable[2] + cftable[4])
npv


# ROC curves

# test data 
test.prob <- predict(prune.class.trees, newdata = test_set, type = "class")
str(test.prob)
table(test.prob)
summary(test_set)

test.prob <- as.numeric(test.prob)
test.roc = roc(test_set$payment_default, test.prob)
plot.roc(test.roc, col=par("fg"),plot=TRUE,print.auc = FALSE, legacy.axes = TRUE, asp =NA, main = "Testing ROC Curve for Classification")
test.roc

# train data
train.prob <- predict(prune.class.trees, newdata = train_set, type = "class")
str(train.prob)
table(train.prob)
summary(train_set)

train.prob <- as.numeric(train.prob)
train.roc = roc(train_set$payment_default, train.prob)
plot.roc(train.roc, col=par("fg"),plot=TRUE,print.auc = FALSE, legacy.axes = TRUE, asp =NA, main = "Training ROC Curve")
train.roc

# Full data
full.prob <- predict(prune.class.trees, newdata = credit, type = "class")
str(full.prob)
table(full.prob)
summary(credit)

full.prob <- as.numeric(full.prob)
full.roc = roc(credit$payment_default, full.prob)
plot.roc(full.roc, col=par("fg"),plot=TRUE,print.auc = FALSE, legacy.axes = TRUE, asp =NA, main = "Full Data ROC Curve")
full.roc




#######################################################################################################


