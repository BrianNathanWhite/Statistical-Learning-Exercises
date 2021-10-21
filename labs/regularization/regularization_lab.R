library(glmnet)
library(tidyverse)
library(mapproj)
library(leaps)

data.fl <- read.csv("CrimeData_FL.csv", header=T, na.string=c("", "?")) 
names(data.fl) # sum(is.na(data.fl)); summary(data.fl)

dim(data.fl)

fit.fl.lm <- lm(violentcrimes.perpop~., data.fl) # dump everything in the model
summary(fit.fl.lm)  #anova(fit.fl.lm)

Y <- data.fl[, 98] # extract Y
X.fl <- model.matrix(violentcrimes.perpop~., data=data.fl)[, -1]
# Get X variables as a matrix. It will also code the categorical 
# variables correctly!. The first col of model.matrix is vector 1
# dim(X.fl)
colnames(X.fl)   # X.fl[1:2, 1:5]

fit.fl.lambda <- glmnet(X.fl, Y, alpha=1, lambda = 150) # alpha =1 corresponding to LASSO solutions
names(fit.fl.lambda) # to see the possible output  

fit.fl.lambda$lambda
fit.fl.lambda$beta

fit.fl.lambda <- glmnet(X.fl, Y, alpha=1)
str(fit.fl.lambda)
fit.fl.lambda$lambda # see the default proposal of lambda's #fit.fl.lambda$beta

par(mgp = c(1.8, 0.5, 0))
plot(fit.fl.lambda,
     main="LASSO path: Top axis shows model size")  

set.seed(10)  # to control the ramdomness in K folds 
fit.fl.cv <- cv.glmnet(X.fl, Y, alpha=1, nfolds=10) 
names(fit.fl.cv); summary(fit.fl.cv)


fit.fl.cv$cvm      

#plot(fit.fl.cv$lambda, fit.fl.cv$cvm, xlab="lambda", ylab="mean cv errors")
fit.fl.cv$lambda.min        # lambda.min returns the min point amoth all the cvm. 

fit.fl.cv$nzero             # number of non-zero coeff's returned for each lambda

plot(fit.fl.cv$lambda, 
     main = "There are 100 lambda used", 
     xlab = "Lambda Index", 
     ylab = "Lambda Value",
     type = "l") 

head(data.frame( Cross.Validation.Erorr = fit.fl.cv$cvm , Lambda = fit.fl.cv$lambda))           
plot(log(fit.fl.cv$lambda), fit.fl.cv$cvm, type = "l",
     xlab=expression(log(lambda)), ylab="mean cv errors")

head(data.frame(fit.fl.cv$lambda, fit.fl.cv$nzero))
plot(fit.fl.cv$lambda, fit.fl.cv$nzero, type = "l",
     xlab=expression(lambda), ylab="number of non-zeros")

plot(fit.fl.cv)
