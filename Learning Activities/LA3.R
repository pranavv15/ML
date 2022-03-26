library(e1071)
library(pROC)
data(iris)

set.seed(490)	

# train-test split
training.indices <- sample(1:nrow(iris), 0.5 * nrow(iris))
x.train <- iris[training.indices, 1:4]
x.test <- iris[-training.indices, 1:4]
y.train <- iris[training.indices, 5]
y.test <- iris[-training.indices, 5]

# Construct the model
model <- naiveBayes(x.train, y.train)
# Predict for training 
pred.train <- predict(model, x.train, type="class")
# pred.train
conf.train <- table(pred.train, y.train)
conf.train

# Predict for testing

# type=class tells model to output specific class predicted (setosa, versicolor, or # virginica) rather than a probability for each
pred <- predict(model, x.test, type="class")
conf <- table(pred, y.test)
conf

# Predict for all data

pred.all <- predict(model, iris[1:4], type="class")
conf.all <- table(pred.all, iris$Species)
conf.all

# Train set
acc.train <- sum(diag(conf.train)) / sum(conf.train)
acc.train
err.train <- 1 - acc.train
err.train

# Test set
acc <- sum(diag(conf)) / sum(conf)
acc
err <- 1 - acc
err

# All data
acc.all <- sum(diag(conf.all)) / sum(conf.all)
acc.all 
err.all <- 1 - acc.all
err.all

#--------------------------------------------------------------------------------------------------

### Train Set Metrics ###
macro.sens <- 0
macro.spec <- 0

# Setosa
sens.train <- conf.train['setosa','setosa'] / sum(conf.train[,'setosa'])
sens.train

spec.train <- sum(diag(conf.train[2:3,2:3])) / (sum(diag(conf.train[2:3,2:3])) + sum(conf.train['setosa',2:3]))
spec.train

ppv.train <- conf.train['setosa','setosa'] / sum(conf.train['setosa',])
ppv.train

npv.train <- sum(diag(conf.train[2:3,2:3])) / (sum(diag(conf.train[2:3,2:3])) + sum(conf.train[2:3,'setosa']))
npv.train

macro.sens <- macro.sens + sens.train
macro.spec <- macro.spec + spec.train

# versicolor
sens.train <- conf.train['versicolor','versicolor'] / sum(conf.train[,'versicolor'])
sens.train

spec.train <- sum(diag(conf.train[c(1,3),c(1,3)])) / (sum(diag(conf.train[c(1,3),c(1,3)])) + sum(conf.train['versicolor',c(1,3)]))
spec.train

ppv.train <- conf.train['versicolor','versicolor'] / sum(conf.train['versicolor',])
ppv.train

npv.train <- sum(diag(conf.train[c(1,3),c(1,3)])) / (sum(diag(conf.train[c(1,3),c(1,3)])) + sum(conf.train[c(1,3),'versicolor']))
npv.train

macro.sens <- macro.sens + sens.train
macro.spec <- macro.spec + spec.train

# virginica
sens.train <- conf.train['virginica','virginica'] / sum(conf.train[,'virginica'])
sens.train

spec.train <- sum(diag(conf.train[1:2,1:2])) / (sum(diag(conf.train[1:2,1:2])) + sum(conf.train['virginica',1:2]))
spec.train

ppv.train <- conf.train['virginica','virginica'] / sum(conf.train['virginica',])
ppv.train

npv.train <- sum(diag(conf.train[1:2,1:2])) / (sum(diag(conf.train[1:2,1:2])) + sum(conf.train[1:2,'virginica']))
npv.train

macro.sens <- macro.sens + sens.train
macro.spec <- macro.spec + spec.train
macro.sens <- macro.sens / 3
macro.spec <- macro.spec / 3

macro.sens
macro.spec

#----------------------------------------------------------------------------------------------

### Test Set Metrics ###
macro.sens <- 0
macro.spec <- 0

# setosa
sens <- conf['setosa','setosa'] / sum(conf[,'setosa'])
sens

spec <- sum(diag(conf[2:3,2:3])) / (sum(diag(conf[2:3,2:3])) + sum(conf['setosa',2:3]))
spec

ppv <- conf['setosa','setosa'] / sum(conf['setosa',])
ppv

npv <- sum(diag(conf[2:3,2:3])) / (sum(diag(conf[2:3,2:3])) + sum(conf[2:3,'setosa']))
npv

macro.sens <- macro.sens + sens
macro.spec <- macro.spec + spec

# versicolor
sens <- conf['versicolor','versicolor'] / sum(conf[,'versicolor'])
sens

spec <- sum(diag(conf[c(1,3),c(1,3)])) / (sum(diag(conf[c(1,3),c(1,3)])) + sum(conf['versicolor',c(1,3)]))
spec

ppv <- conf['versicolor','versicolor'] / sum(conf['versicolor',])
ppv

npv <- sum(diag(conf[c(1,3),c(1,3)])) / (sum(diag(conf[c(1,3),c(1,3)])) + sum(conf[c(1,3),'versicolor']))
npv

macro.sens <- macro.sens + sens
macro.spec <- macro.spec + spec

# virginica
sens <- conf['virginica','virginica'] / sum(conf[,'virginica'])
sens

spec <- sum(diag(conf[1:2,1:2])) / (sum(diag(conf[1:2,1:2])) + sum(conf['virginica',1:2]))
spec

ppv <- conf['virginica','virginica'] / sum(conf['virginica',])
ppv

npv <- sum(diag(conf[1:2,1:2])) / (sum(diag(conf[1:2,1:2])) + sum(conf[1:2,'virginica']))
npv

macro.sens <- macro.sens + sens
macro.spec <- macro.spec + spec
macro.sens <- macro.sens / 3
macro.spec <- macro.spec / 3

macro.sens
macro.spec

#-------------------------------------------------------------------------------------------------

### All Metrics ###
macro.sens <- 0
macro.spec <- 0

# setosa
sens.all <- conf.all['setosa','setosa'] / sum(conf.all[,'setosa'])
sens.all

spec.all <- sum(diag(conf.all[2:3,2:3])) / (sum(diag(conf.all[2:3,2:3])) + sum(conf.all['setosa',2:3]))
spec.all

ppv.all <- conf.all['setosa','setosa'] / sum(conf.all['setosa',])
ppv.all

npv.all <- sum(diag(conf.all[2:3,2:3])) / (sum(diag(conf.all[2:3,2:3])) + sum(conf.all[2:3,'setosa']))
npv.all

macro.sens <- macro.sens + sens.all
macro.spec <- macro.spec + spec.all

# versicolor
sens.all <- conf.all['versicolor','versicolor'] / sum(conf.all[,'versicolor'])
sens.all

spec.all <- sum(diag(conf.all[c(1,3),c(1,3)])) / (sum(diag(conf.all[c(1,3),c(1,3)])) + sum(conf.all['versicolor',c(1,3)]))
spec.all

ppv.all <- conf.all['versicolor','versicolor'] / sum(conf.all['versicolor',])
ppv.all

npv.all <- sum(diag(conf.all[c(1,3),c(1,3)])) / (sum(diag(conf.all[c(1,3),c(1,3)])) + sum(conf.all[c(1,3),'versicolor']))
npv.all

macro.sens <- macro.sens + sens.all
macro.spec <- macro.spec + spec.all

# virginica
sens.all <- conf.all['virginica','virginica'] / sum(conf.all[,'virginica'])
sens.all

spec.all <- sum(diag(conf.all[1:2,1:2])) / (sum(diag(conf.all[1:2,1:2])) + sum(conf.all['virginica',1:2]))
spec.all

ppv.all <- conf.all['virginica','virginica'] / sum(conf.all['virginica',])
ppv.all

npv.all <- sum(diag(conf.all[1:2,1:2])) / (sum(diag(conf.all[1:2,1:2])) + sum(conf.all[1:2,'virginica']))
npv.all

macro.sens <- macro.sens + sens.all
macro.spec <- macro.spec + spec.all
macro.sens <- macro.sens / 3
macro.spec <- macro.spec / 3

macro.sens
macro.spec

#--------------------------------------------------------------------------------------------------
# ROC curve using Test Set

probs <- predict(model, x.test, type="raw")

setosa.labels <- rep(0, length(y.test))
versicolor.labels <- rep(0, length(y.test))
virginica.labels <- rep(0, length(y.test))
for (i in 1:length(y.test)) {
  if(y.test[i] == 'setosa') {
    setosa.labels[i] <- 1
  } else if (y.test[i] == 'versicolor') {
    versicolor.labels[i] <- 1
  } else if (y.test[i] == 'virginica') {
    virginica.labels[i] <- 1
  }
}

# Create a plot of ROC for each class label against the rest
setosa.roc <- roc(setosa.labels, probs[, 'setosa'], auc.polygon=TRUE, max.auc.polygon=TRUE, print.auc=TRUE, show.thres=TRUE)
setosa.smoothroc <- smooth(setosa.roc, method = "density")
plot(setosa.smoothroc, col = 'red', xaxt='n', xlab="False Positive Rate (1 - Specificity)", ylab = "True Positive Rate (Sensitivity)")

#par(new=T): make the second plot without cleaning the first
par(new=TRUE)
versicolor.roc <- roc(versicolor.labels, probs[, 'versicolor'], auc.polygon=TRUE, max.auc.polygon=TRUE, print.auc=TRUE, show.thres=TRUE)
versicolor.smoothroc <- smooth(versicolor.roc, method = "density")
plot(versicolor.smoothroc, col='blue', xaxt='n', xlab="", ylab = "")

par(new=TRUE)
virginica.roc <- roc(virginica.labels, probs[, 'virginica'], auc.polygon=TRUE, max.auc.polygon=TRUE, print.auc=TRUE, show.thres=TRUE)
virginica.smoothroc <- smooth(virginica.roc, method = "density")
plot(virginica.smoothroc, col='green', xaxt='n', xlab="", ylab = "")

y.labels <- c(setosa.labels, versicolor.labels, virginica.labels)
y.probs <- c(probs[, 'setosa'], probs[, 'versicolor'], probs[, 'virginica'])

par(new=TRUE)
micro.roc <- roc(y.labels, y.probs, auc.polygon=TRUE, max.auc.polygon=TRUE, print.auc=TRUE, show.thres=TRUE)
micro.smoothroc <- smooth(micro.roc, method = "density")
plot(micro.smoothroc, col = 'black', lty = 'dotdash', xaxt='n', xlab="", ylab = "")

macro.sensitivity <- (setosa.smoothroc$sensitivities + versicolor.smoothroc$sensitivities + virginica.smoothroc$sensitivities) / 3
macro.specificity <- ((setosa.smoothroc$specificities + versicolor.smoothroc$specificities + virginica.smoothroc$specificities) / 3)
lines(macro.specificity, macro.sensitivity, type='l', xlim = rev(range(macro.specificity)), col='magenta', lty=4)
axis(1, at=(5:0) * 0.2, labels=(0:5) * 0.2, pos=c(-0.04,0))

legend(0.4, 0.5, legend = c('setosa','versicolor', 'virginica', 'micro-avg', 'macro-avg'), col = c('red', 'blue', 'green', 'black', 'magenta'), lty = c(1,1,1,4,4))

probs.train <- predict(model, x.train, type="raw")
multiclass.roc(y.train, probs.train)

multiclass.roc(y.test, probs)

probs.all <- predict(model, iris[1:4], type="raw")
multiclass.roc(iris$Species, probs.all)




