library(MASS)
library(moments)  #download moments

mu = c(2,4,6)
mu

sigma = matrix(c(4,3,2,3,9,5,2,5,36),3,3);
sigma

set.seed(490)   #setting seed as 490
rannum <- mvrnorm(n=10,mu,sigma)
View(rannum)

summary(rannum)

x1 = rannum[,1]
x2 = rannum[,2]
x3 = rannum[,3]

means = c(mean(x1),mean(x2),mean(x3))
means

medians = c(median(x1),median(x2),median(x3))
medians

skew = c(skewness(x1),skewness(x2),skewness(x3))
skew

ktosis = c(kurtosis(x1),kurtosis(x2),kurtosis(x3))
ktosis

cov(rannum)

cor(rannum)



