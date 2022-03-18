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
#cols2 <- c(3,4,5,7,8,9,10,11,12,25)
for (i in cols) {
  credit[,i] <- as.numeric((unlist(credit[,i])))
}
# for (j in cols2) {
#   credit[,j] <- as.factor((unlist(credit[,j])))
# }

# omit na
credit <- na.omit(credit)

# Descriptive analysis
summary(credit)
str(credit)

plot(credit$AGE,credit$LIMIT_BAL)

# Classification Tree

set.seed(490)

# split into training and testing
credit.split <- sample(1:nrow(credit), size = nrow(credit)*0.7)
train_set <- credit[credit.split,]
test_set <- credit[-credit.split,]

#class.cart <- rpart(formula = AHD ~ ., data = heart.train, method = "class", control = rpart.control(minbucket = 2, xval = 10))
#prp(class.cart, roundint = FALSE)

class.cart2 <- rpart(formula = DEFAULT ~ ., data = train_set, method = "class", control = rpart.control(minbucket = 1,minsplit = 2, xval = 10, cp = 0.001))
prp(class.cart2, roundint = FALSE)

cp.class.param <- class.cart2$cptable
cp.class.param

#summary(Heart)
