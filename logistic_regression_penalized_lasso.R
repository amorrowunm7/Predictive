library(stats)
library(glmnet)
library(MASS)

pdf("heart_regression_plots.pdf")

data <- read.table("SAheart.txt",sep="\t",header=T)

#scale the predictors in the data
data[,c(1:9)] <- scale(data[,c(1:9)])

#explore the correlation, there should be attributes that are correlated
correlation <- cor(data)

#draw a heat map of the correlation matrix
library(gplots)
heatmap.2(as.matrix(correlation),col=bluered,scale = "none", breaks= seq(from=-1, to= 1, by = 0.1),Rowv=FALSE,Colv=FALSE,dendrogram="none",trace="none")


#model with all predictors
glm.fit1 <- glm(chd ~ . , data = data,family="binomial")
summary(glm.fit1)

#plot(glm.fit1$fitted.values,data[,10],xlab="predicted",ylab="observed")
#plot(glm.fit1$fitted.values,glm.fit1$residuals,xlab="predicted",ylab="residuals")
# remove some 
#check if the residuals are normally distributed
#qqnorm(glm.fit1$residuals)
#qqline(glm.fit1$residuals, col = 2)


#selecting attributes based on AIC
#creating formula if number of attributes is huge
fmla <- as.formula(paste( "~ " , paste(colnames(data)[1:9],collapse = "+")))
forward <- stepAIC(glm(chd ~ 1, data = data),scope = list(lower =  ~1, upper = fmla , data= data),direction="forward",trace=1)
backward <- stepAIC(glm(chd ~ ., data = data),scope = list(lower =  ~1, upper = fmla , data= data),direction="backward",trace=1)
#qqnorm(forward$residuals)
#qqline(forward$residuals, col = 2)

#Ridge regression cross validation
predictors <- data[,c(1:9)]
response <- data[,10]
cv.glmnet.fit1 <- cv.glmnet(as.matrix(predictors), response, alpha = 0, type.measure="class",nlambda = 200,family="binomial")
print(cv.glmnet.fit1$lambda.min)
choice=cv.glmnet.fit1$lambda.1se
print(choice)
plot(cv.glmnet.fit1,main="ridge error vs lambda")
#text(log(cv.glmnet.fit1$lambda.min, cv.glmnet.fit1$lambda.1se,sep=","))
plot(cv.glmnet.fit1$glmnet.fit,xvar="lambda",label=TRUE,main="ridge coef vs lambda")
print(coef(cv.glmnet.fit1, s = "lambda.min"))
print(coef(cv.glmnet.fit1, s = "lambda.1se"))

#get predicted class
cv.glmnet.fit1.class<- as.numeric(predict(cv.glmnet.fit1,as.matrix(predictors), type = "class",s=cv.glmnet.fit1$lambda.min))
#get fitted probabilities
cv.glmnet.fit1.prob<- predict(cv.glmnet.fit1,as.matrix(predictors), type = "response",s=cv.glmnet.fit1$lambda.min)


#do a finer search for lambda
#cv.glmnet.fit1 <- cv.glmnet(as.matrix(predictors), response, alpha = 0, type.measure="class",lambda = seq(from=0,to=choice,by=(choice-0)/200),family="binomial")
#cv.glmnet.fit1$lambda.min
#cv.glmnet.fit1$lambda.1se
#plot(cv.glmnet.fit1,main=paste(cv.glmnet.fit1$lambda.min, cv.glmnet.fit1$lambda.1se,sep=","))
#plot(cv.glmnet.fit1$glmnet.fit,xvar="lambda",label=TRUE,main=paste(cv.glmnet.fit1$lambda.min, cv.glmnet.fit1$lambda.1se,sep=","))
#coef(cv.glmnet.fit1, s = "lambda.min")
#coef(cv.glmnet.fit1, s = "lambda.1se")




#Lasso regression cross validation
cv.glmnet.fit2 <- cv.glmnet(as.matrix(predictors), response, type.measure="class",alpha = 1, nlambda = 200,family="binomial")
#getting value of lambda which gives minimum error
print(cv.glmnet.fit2$lambda.min)
choice=cv.glmnet.fit2$lambda.1se
print(choice)
plot(cv.glmnet.fit2,main="lasso error vs lambda")
plot(cv.glmnet.fit2$glmnet.fit,xvar="lambda",label=TRUE,main="lasso coef vs lambda")
print(coef(cv.glmnet.fit2, s = "lambda.min"))
print(coef(cv.glmnet.fit2, s = "lambda.1se"))


#get predicted class
cv.glmnet.fit2.class<- as.numeric(predict(cv.glmnet.fit2,as.matrix(predictors), type = "class",s=cv.glmnet.fit2$lambda.min))
#get fitted probabilities
cv.glmnet.fit2.prob<- predict(cv.glmnet.fit2,as.matrix(predictors), type = "response",s=cv.glmnet.fit2$lambda.min)


#do a finer search
#cv.glmnet.fit2 <- cv.glmnet(as.matrix(predictors), response, type.measure="mse",alpha = 1, lambda = seq(from=0,to=choice,by=(choice-0)/200),family="binomial")
#getting value of lambda which gives minimum error
#cv.glmnet.fit2$lambda.min
#choice=cv.glmnet.fit2$lambda.1se
#plot(cv.glmnet.fit2,main=paste(cv.glmnet.fit2$lambda.min, cv.glmnet.fit2$lambda.1se,sep=","))
#plot(cv.glmnet.fit2$glmnet.fit,xvar="lambda",label=TRUE,main=paste(cv.glmnet.fit2$lambda.min, cv.glmnet.fit2$lambda.1se,sep=","))
#coef(cv.glmnet.fit2, s = "lambda.min")
#coef(cv.glmnet.fit2, s = "lambda.1se")

dev.off()


