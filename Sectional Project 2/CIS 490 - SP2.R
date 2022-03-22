# Load Libraries
library(Amelia)
library(ggplot2)
library(pROC)
library(ROCR)
library(dplyr)
library(InformationValue)
library(MASS)
library(DataExplorer)
library(rpart)
library(rpart.plot)
# Load Dataset
data <- read.csv(file.choose(), header = T)

# Change target variable name to payment_default and set as factor variable
colnames(data)[25] <- "payment_default"
data$payment_default<-as.factor(data$payment_default)

# Change SEX, EDUCATION, and MARRIAGE to factor
data$SEX<-ifelse(data$SEX==1,"Male","Female")

data$EDUCATION<-ifelse(data$EDUCATION==1,"Graduate School",
                    ifelse(data$EDUCATION==2,"University",
                        (ifelse(data$EDUCATION==3,"High School",
                            ifelse(data$EDUCATION==4,"Others","Unknown")))))                                                         

data$MARRIAGE<-ifelse(data$MARRIAGE==1,"Married",
                      ifelse(data$MARRIAGE==2,"Single","Others"))

names<-c("SEX","EDUCATION","MARRIAGE")
data[names]<-lapply(data[names],as.factor)

# Check number of counts under each repayment category
PAY_VAR<-lapply(data[,c("PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6")], function(x) table(x))
print(PAY_VAR)

# Change payment attributes to categorical and group 5 months or more delayed together
data$PAY_0<-ifelse(data$PAY_0 < 1 ,"Paid Duly",
                   ifelse(data$PAY_0==1,"1 month delay",
                        ifelse(data$PAY_0==2,"2 month delay",ifelse(data$PAY_0==3,"3 month delay",
                            ifelse(data$PAY_0==4,"4 month delay","5 month or more delay")))))

data$PAY_2<-ifelse(data$PAY_2 <1 ,"Paid Duly",
                   ifelse(data$PAY_2==1,"1 month delay",
                        ifelse(data$PAY_2==2,"2 month delay",
                            ifelse(data$PAY_2==3,"3 month delay",
                               ifelse(data$PAY_2==4,"4 month delay","5 month or more delay")))))

data$PAY_3<-ifelse(data$PAY_3 <1 ,"Paid Duly",
                   ifelse(data$PAY_3==1,"1 month delay",
                      ifelse(data$PAY_3==2,"2 month delay",
                          ifelse(data$PAY_3==3,"3 month delay",
                              ifelse(data$PAY_3==4,"4 month delay","5 month or more delay")))))

data$PAY_4<-ifelse(data$PAY_4 <1 ,"Paid Duly",
                   ifelse(data$PAY_4==1,"1 month delay",
                      ifelse(data$PAY_4==2,"2 month delay",
                          ifelse(data$PAY_4==3,"3 month delay",
                              ifelse(data$PAY_4==4,"4 month delay","5 month or more delay")))))

data$PAY_5<-ifelse(data$PAY_5 <1 ,"Paid Duly",
                   ifelse(data$PAY_5==1,"1 month delay",
                       ifelse(data$PAY_5==2,"2 month delay",
                              ifelse(data$PAY_5==3,"3 month delay",
                                  ifelse(data$PAY_5==4,"4 month delay","5 month or more delay")))))

data$PAY_6<-ifelse(data$PAY_6 <1 ,"Paid Duly",
                 ifelse(data$PAY_6==1,"1 month delay",
                    ifelse(data$PAY_6==2,"2 month delay",
                        ifelse(data$PAY_6==3,"3 month delay",
                            ifelse(data$PAY_6==4,"4 month delay","5 month or more delay")))))

names<-c("PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6")
data[names]<-lapply(data[names],as.factor)

# Analysis of continous variables
plot_histogram(data[,-1])

# Check and remove outliers
Outlier<-data.frame(apply(data[,c("LIMIT_BAL","BILL_AMT1","BILL_AMT2","BILL_AMT3",
                                  "BILL_AMT4","BILL_AMT5","BILL_AMT6",
                                  "PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4",
                                  "PAY_AMT5","PAY_AMT6")],
                    2, function(x) quantile(x, probs = seq(0, 1, by= 0.00001))))
head(Outlier)
tail(Outlier)

data<-subset(data, !(data$LIMIT_BAL> quantile(data$LIMIT_BAL, 0.99999) |
                   data$BILL_AMT1< quantile(data$BILL_AMT1, 0.00001)))

# Colinearity among variables
numeric_fields<-c("LIMIT_BAL","BILL_AMT1","BILL_AMT2","BILL_AMT3",
                  "BILL_AMT4","BILL_AMT5","BILL_AMT6",
                  "PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")
df_numeric<-subset(data, select=numeric_fields)
plot_correlation(df_numeric)

data$PAY_RATIO_APR<-ifelse(is.nan(data$PAY_AMT1/data$BILL_AMT1),0,
                         ifelse(is.infinite(data$PAY_AMT1/data$BILL_AMT1),0,
                                round(data$PAY_AMT1/data$BILL_AMT1,2)))

data$PAY_RATIO_MAY<-ifelse(is.nan(data$PAY_AMT2/data$BILL_AMT2),0,
                         ifelse(is.infinite(data$PAY_AMT2/data$BILL_AMT2),0,
                                round(data$PAY_AMT2/data$BILL_AMT2,2)))

data$PAY_RATIO_JUNE<-ifelse(is.nan(data$PAY_AMT3/data$BILL_AMT3),0,
                          ifelse(is.infinite(data$PAY_AMT3/data$BILL_AMT3),0,
                                 round(data$PAY_AMT3/data$BILL_AMT3,2)))

data$PAY_RATIO_JULY<-ifelse(is.nan(data$PAY_AMT4/data$BILL_AMT4),0,
                          ifelse(is.infinite(data$PAY_AMT4/data$BILL_AMT4),0,
                                 round(data$PAY_AMT4/data$BILL_AMT4,2)))

data$PAY_RATIO_AUG<-ifelse(is.nan(data$PAY_AMT5/data$BILL_AMT5),0,
                         ifelse(is.infinite(data$PAY_AMT5/data$BILL_AMT5),0,
                                round(data$PAY_AMT5/data$BILL_AMT5,2)))

data$PAY_RATIO_SEPT<-ifelse(is.nan(data$PAY_AMT6/data$BILL_AMT6),0,
                          ifelse(is.infinite(data$PAY_AMT6/data$BILL_AMT6),0,
                                 round(data$PAY_AMT6/data$BILL_AMT6,2)))

numeric_fields<-c("LIMIT_BAL","BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5",
                  "BILL_AMT6","PAY_RATIO_APR","PAY_RATIO_MAY","PAY_RATIO_JUNE",
                  "PAY_RATIO_JULY","PAY_RATIO_AUG","PAY_RATIO_SEPT",
                  "PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")
df_numeric<-subset(data, select=numeric_fields)
plot_correlation(df_numeric)

# Missing values in data
missmap(data, col = c('yellow', 'black'), y.at = 1, y.labels = '', legend = TRUE)

# Data Exploration
# Frequency Plot Sex, Education, Marriage Vs Target Variable

ggplot(data, aes(x = EDUCATION, fill = payment_default)) +
  geom_bar() +
  ggtitle("Frequency of Education Vs Target Variable") +
  labs(x = 'Education') +
  theme_minimal()

ggplot(data, aes(x = AGE, fill = payment_default)) +
  geom_bar() +
  ggtitle("Frequency of Age Vs Target Variable") +
  labs(x = 'Age') +
  theme_minimal()
  
ggplot(data, aes(x = SEX, fill = payment_default)) +
  geom_bar() +
  ggtitle("Frequency of Gender Vs Target Variable") +
labs(x = 'Sex') +
  theme_minimal()

ggplot(data, aes(x = MARRIAGE, fill = payment_default)) +
  geom_bar() +
  ggtitle("Frequency of Marriage Vs Target Variable") +
  labs(x = 'Marriage') +
  theme_minimal()
##################### Logistic Regression ######################################
Random.seed <- c('Mersenne-Twister', 1)
set.seed(490)

indices <-sample(1:nrow(data), 0.8 * nrow(data), replace = FALSE)
train <-data[indices,]
test <-data[-indices,]

# Check information value of categorical variables to understand if they can be 
# omitted.
SEX<-data.frame("SEX"=IV(train$SEX,train$payment_default))
EDUCATION<-data.frame("EDUCATION"=IV(train$EDUCATION,train$payment_default))
MARRIAGE<-data.frame("MARRIAGE"=IV(train$MARRIAGE,train$payment_default))
PAY_0<-data.frame("PAY_0"=IV(train$PAY_0,train$payment_default))
PAY_2<-data.frame("PAY_2"=IV(train$PAY_2,train$payment_default))
PAY_3<-data.frame("PAY_3"=IV(train$PAY_3,train$payment_default))
PAY_4<-data.frame("PAY_4"=IV(train$PAY_4,train$payment_default))
PAY_5<-data.frame("PAY_5"=IV(train$PAY_5,train$payment_default))
PAY_6<-data.frame("PAY_6"=IV(train$PAY_6,train$payment_default))

Iv<-cbind(SEX,EDUCATION,MARRIAGE,PAY_0,PAY_2,PAY_3,PAY_4,PAY_5,PAY_6)
print(Iv)

mod_1<-glm(payment_default~.-(ID+BILL_AMT1+BILL_AMT2+BILL_AMT3+
                              BILL_AMT4+BILL_AMT5+BILL_AMT6+SEX+MARRIAGE), 
                              train, family=binomial)
summary(mod_1)

# Check accuracy and confusion matrix of our model
predict_1<-predict(mod_1, train, type='response')
prob_1<-ifelse( predict_1 > 0.5,1,0)

confusion_matrix<-table(prob_1, train$payment_default)
print(confusion_matrix)

Accuracy<-sum(diag(confusion_matrix))/sum(confusion_matrix)
print(Accuracy*100)

# Employ StepAIC to find best subset of values
step_AIC<-stepAIC(mod_1,direction='backward')

mod_2<-glm(payment_default ~ LIMIT_BAL + EDUCATION + AGE + PAY_0 + PAY_2 + 
           PAY_3 + PAY_4 + PAY_5 + PAY_6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + 
           PAY_AMT4 + PAY_AMT5 + PAY_AMT6 + PAY_RATIO_APR, train, family='binomial')

summary(mod_2)

predict_2<-predict(mod_2,train,type='response')
prob_2<-ifelse(predict_2>0.5,1,0)

#Confusion Matrix
confusion_matrix<-table(prob_2, train$payment_default)
print(confusion_matrix)

## Evaluation train data
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy

# Calculate Sensitivity
sensitivity<-confusion_matrix[1]/(confusion_matrix[1] + confusion_matrix[2])
sensitivity

# Calculate Specificity
specificity <- confusion_matrix[4]/(confusion_matrix[3] + confusion_matrix[4])
specificity

# Calculate Positive Predictive Value
ppv <- confusion_matrix[1]/(confusion_matrix[1] + confusion_matrix[3])
ppv

# Calculate Negative Predictive Value
npv <- confusion_matrix[4]/(confusion_matrix[2] + confusion_matrix[4])
npv

#The ROCR Curve (Train)
train.roc = roc(train$payment_default, predict_2)
plot.roc(train.roc, col=par("fg"),plot=TRUE,print.auc = FALSE, legacy.axes = TRUE, asp =NA, main = "ROC Curve Training")
train.roc

# Run model on test data
predict_2<-predict(mod_2,test,type='response')
prob_2<-ifelse(predict_2>0.5,1,0)

#The ROCR Curve (Test)
test.roc = roc(test$payment_default, predict_2)
plot.roc(test.roc, col=par("fg"),plot=TRUE,print.auc = FALSE, legacy.axes = TRUE, asp =NA, main = "ROC Curve Testing")
test.roc

#Confusion matrix for test data
confusion_matrix_test<-table(prob_2,test$payment_default)
print(confusion_matrix_test)
#Accuarcy of the model 
Accuracy_test<-sum(diag(confusion_matrix_test))/sum(confusion_matrix_test)
print(Accuracy_test * 100)

## Evaluation test data
accuracy <- sum(diag(confusion_matrix_test))/sum(confusion_matrix_test)
accuracy

# Calculate Sensitivity
sensitivity<-confusion_matrix_test[1]/(confusion_matrix_test[1] + confusion_matrix_test[2])
sensitivity

# Calculate Specificity
specificity <- confusion_matrix_test[4]/(confusion_matrix_test[3] + confusion_matrix_test[4])
specificity

# Calculate Positive Predictive Value
ppv <- confusion_matrix_test[1]/(confusion_matrix_test[1] + confusion_matrix_test[3])
ppv

# Calculate Negative Predictive Value
npv <- confusion_matrix_test[4]/(confusion_matrix_test[2] + confusion_matrix_test[4])
npv

# Logistic Regression All Data
predict_3<-predict(mod_2,data,type='response')
prob_3<-ifelse(predict_3>0.5,1,0)
full.roc = roc(data$payment_default, predict_3)
plot.roc(full.roc, col=par("fg"),plot=TRUE,print.auc = FALSE, legacy.axes = TRUE, asp =NA, main="ROC Curve Full")
full.roc
#Confusion matrix for all data
confusion_matrix_all<-table(prob_3,data$payment_default)
print(confusion_matrix_all)
#Accuarcy of the model 
Accuracy_test<-sum(diag(confusion_matrix_all))/sum(confusion_matrix_all)
print(Accuracy_test * 100)

## Evaluation full data
accuracy <- sum(diag(confusion_matrix_all))/sum(confusion_matrix_all)
accuracy

# Calculate Sensitivity
sensitivity<-confusion_matrix_all[1]/(confusion_matrix_all[1] + confusion_matrix_all[2])
sensitivity

# Calculate Specificity
specificity <- confusion_matrix_all[4]/(confusion_matrix_all[3] + confusion_matrix_all[4])
specificity

# Calculate Positive Predictive Value
ppv <- confusion_matrix_all[1]/(confusion_matrix_all[1] + confusion_matrix_all[3])
ppv

# Calculate Negative Predictive Value
npv <- confusion_matrix_all[4]/(confusion_matrix_all[2] + confusion_matrix_all[4])
npv

# Calculate loglikelihood
logLik(mod_2)
# Calculate odds ratio
exp(coef(mod_2))
# Confidence Intervals
confint(mod_2)
# s curve
pay_mdl <- glm(payment_default ~ AGE, data = data, family = "binomial")
data$payment_default <- as.numeric(data$payment_default)-1
plot(payment_default ~ AGE, data = data, 
     col = "darkorange", pch = "|", xlim = c(-1000, 2500), ylim = c(0, 1),
     main = "Using Logistic Regression for Classification")
abline(h = 0, lty = 3)
abline(h = 1, lty = 3)
abline(h = 0.5, lty = 2)
curve(predict(pay_mdl, data.frame(AGE = x), type = "response"), add = TRUE, lwd = 3, col = "dodgerblue")
abline(v = -coef(pay_mdl)[1] / coef(pay_mdl)[2], lwd = 3)

# Classification Tree
data$payment_default <- as.factor(data$payment_default)
credit <- data
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
table(test.prob)

test.prob <- as.numeric(test.prob)
test.roc = roc(test_set$payment_default, test.prob)
plot.roc(test.roc, col=par("fg"),plot=TRUE,print.auc = FALSE, legacy.axes = TRUE, asp =NA, main = "Testing ROC Curve for Classification")
test.roc

# train data
train.prob <- predict(prune.class.trees, newdata = train_set, type = "class")
table(train.prob)

train.prob <- as.numeric(train.prob)
train.roc = roc(train_set$payment_default, train.prob)
plot.roc(train.roc, col=par("fg"),plot=TRUE,print.auc = FALSE, legacy.axes = TRUE, asp =NA, main = "Training ROC Curve")
train.roc

# Full data
full.prob <- predict(prune.class.trees, newdata = credit, type = "class")
table(full.prob)


full.prob <- as.numeric(full.prob)
full.roc = roc(credit$payment_default, full.prob)
plot.roc(full.roc, col=par("fg"),plot=TRUE,print.auc = FALSE, legacy.axes = TRUE, asp =NA, main = "Full Data ROC Curve")
full.roc
#######################################################################################################









