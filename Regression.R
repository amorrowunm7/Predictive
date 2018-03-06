
pdf("regression_plots.pdf")
library(glmnet)
library(stats)
car <- read.table("car.txt",sep="\t",header=T)
head(car)
pairs(car)
cor(car)
glm_fit <- glm(mpg ~ ., data = car)
summary(glm_fit)

#separating predictor and response variables
predictors <- car[,-1]
response <- car[,1]

#fitting a regression model
fit0 = glmnet(as.matrix(predictors), response, alpha = 0, lambda = 0)
print(fit0)
fit0$beta


fit = glmnet(as.matrix(predictors), response, alpha = 0, nlambda = 200)
dim(fit$beta)
print(fit)
#plot 
plot(fit, xvar = "lambda", label = TRUE,xlim=c(-2,10))
L <- length(fit$lambda)
x <- log(fit$lambda[L])
y <- fit$beta[, L]
labs <- names(y)
text(x, y, labels=labs)


plot(fit, xvar = "dev", label = TRUE)


#cross validation
cvfit = cv.glmnet(as.matrix(predictors), response, alpha= 0,nlambda = 200,type.measure = "mse", nfolds = 10)
#getting value of lambda which gives minimum error
cvfit$lambda.min
cvfit$lambda.1se
# plot the cross validation errors with standard errors
plot(cvfit,xlim=c(-3,10))

#getting coefficients of the fit when minimum error
coef(cvfit, s = "lambda.min")
coef(cvfit, s = "lambda.1se")

#do a regression without penalization
glm_fit <- glm(mpg ~ . , data = car)
summary(glm_fit)
dev.off()

